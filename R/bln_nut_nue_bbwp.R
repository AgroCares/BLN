#' Function to calculate and evaluate the NUE
#'
#' This is the NUE calculation as being used in the BBWP framework
#'
#' @param B_LU_BRP (numeric) The crop code
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_HELP_WENR (character) The soil type abbreviation, derived from 1:50.000 soil map
#' @param A_P_AL (numeric) The P-AL content of the soil
#' @param A_P_WA (numeric) The P-content of the soil extracted with water (mg P2O5 / 100 ml soil)
#' @param A_P_CC (numeric) The plant available P content, measured via 0.01M CaCl2 extraction (mg / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil (mg N / kg)
#' @param B_N_RT (numeric) The mean organic nitrogen content of the soil in the LSW region (mg N / kg). Optional.
#' @param B_N_RT_SD (numeric) The variance in organic nitrogen content of the soil in the LSW region (standard deviation) (mg N / kg). Optional.
#' @param penalty (boolean) the option to apply a penalty for high risk BBWP field indicators. Default is TRUE.
#'
#' @import data.table
#' @import OBIC
#'
#' @export
bln_nut_nue <- function(B_LU_BRP,B_HELP_WENR,B_GWL_CLASS,A_P_AL,A_P_CC,A_P_WA,
                        A_N_RT,B_N_RT = NA_real_,B_N_RT_SD = NA_real_,penalty = TRUE){

  # load internal table
  dt.lsw <- BLN::bln_lsw[B_LSW_ID == 'lsw_nlmean']
  dt.crop <- BLN::bln_crops[bln_country=='NL']

  # make internal table
  dt <- data.table(id = 1:length(B_LU_BRP),
                   B_LU_BRP = B_LU_BRP,
                   B_HELP_WENR = B_HELP_WENR,
                   B_GWL_CLASS = B_GWL_CLASS,
                   A_N_RT = A_N_RT,
                   A_P_AL= A_P_AL,
                   A_P_WA = A_P_WA,
                   A_P_CC = A_P_CC,
                   B_N_RT = B_N_RT,
                   B_N_RT_SD = B_N_RT_SD
  )

  # replace missing LSW properties
  dt[is.na(B_N_RT), B_N_RT := dt.lsw$B_N_RT]
  dt[is.na(B_N_RT_SD), B_N_RT_SD := dt.lsw$B_N_RT_SD]

  # merge with crop category
  dt <- merge(dt,dt.crop[,.(crop_code,crop_cat1)],by.x='B_LU_BRP',by.y='crop_code',all.x = TRUE)

  # Replace '-' with 'unknown'
  dt[! B_GWL_CLASS %in% c('GtI','GtII','GtIII','GtIV','GtV', 'GtVI','GtVII','GtVIII'), B_GWL_CLASS := '-']

  # estimate field properties

  # rank the nitrogen content as an estimate of total N content: a high value means high risk for N leaching
  dt[,ngw_nlv := pnorm(q = A_N_RT, mean = B_N_RT, sd = B_N_RT_SD)]

  # rank the risk for N pool in soil: higher NLV is associated to increased risks for N runoff
  dt[,nsw_nlv := ngw_nlv]

  # do nlv correction for grassland
  dt[grepl('grass',crop_cat1), nsw_nlv := pmax(0, ngw_nlv - 0.5)]

  # calculate the OBIC water risk index for combined drought and wetstress (% yield reduction)
  dt[, npe_wri := 1] # When B_HELP_WENR is `unknown`
  if (nrow(dt[B_HELP_WENR != 'unknown',]) > 0) {
    dt[B_HELP_WENR != 'unknown', npe_wri := OBIC::calc_waterstressindex(
      B_HELP_WENR = B_HELP_WENR,
      B_LU_BRP = B_LU_BRP,
      B_GWL_CLASS = B_GWL_CLASS,
      WSI = 'waterstress'
    ) * 0.01]
  }

  # calculate the P-availability-index (P fertilizer is more efficient on low PBI)
  dt[,npe_pbi := OBIC::calc_phosphate_availability(B_LU_BRP = B_LU_BRP,A_P_AL = A_P_AL,A_P_CC = A_P_CC,A_P_WA = A_P_WA)]

  # transform npe_pbi to an index between 0 and 1
  dt[,npe_pbi := OBIC::ind_phosphate_availability(npe_pbi)]

  # calculate the drought stress, as factor controlling N-efficiency on grassland
  dt[, npe_wdri := 1] # When B_HELP_WENR is `unknown`
  if (nrow(dt[B_HELP_WENR != 'unknown',]) > 0) {
    dt[B_HELP_WENR != 'unknown', npe_wdri := OBIC::calc_waterstressindex(
      B_HELP_WENR = B_HELP_WENR,
      B_LU_BRP = B_LU_BRP,
      B_GWL_CLASS = B_GWL_CLASS,
      WSI = 'droughtstress'
    ) * 0.01]
  }

  # rank the risk for N efficiency : low A_N_RT means high potential for improvement NUE
  dt[,npe_nlv := 1 - nsw_nlv]

  # estimate field indicator

  # columns to be selected
  cols <- colnames(dt)[grepl('npe_|id',colnames(dt))]

  # melt the data.table to simplify corrections
  dt.melt <- data.table::melt(dt[,mget(cols)], id.vars = 'id',variable.name = 'risk')

  # add correction factor based on risk itself
  dt.melt[,risk_cor := wf(value,type = "indicators",penalty = penalty)]

  # add groups of risk indicators
  dt.melt[,group := gsub('_[a-z]+$','',gsub('d_','',risk))]

  # add manual weighing factor for risks
  dt.melt[,mcf := 1]
  dt.melt[group=='npe' & grepl('_pbi$',risk), mcf := 2]

  # calculate the mean aggregated risk indicators
  dt.ind <- dt.melt[,list(risk = sum(risk_cor * value * mcf)/sum(risk_cor * mcf)),by=c('id','group')]
  dt.ind <- dcast(dt.ind,id~group,value.var='risk')

  # sort output based on id
  setorder(dt.ind,id)

  # add field indicator to the dt
  dt <- merge(dt,dt.ind,by='id')

  # estimate field score

  # correction for need for increased nutrient use efficiency
  dt[,cfnue := 0.5]

  # calculate the individual opportunity indexes
  dt[,d_opi_nue := (0.5 + cfnue/2) * bln_evaluate_logistic(npe, b=6, x0=0.4, v=.7)]

  # update the field score with measures (assume no measures to be taken)
  dt[,d_opi_nue := pmax(0,1 - pmax(0, d_opi_nue - 0))]

  # Set BBWP score
  dt[,s_bbwp_nue := d_opi_nue]

  # return value
  value <- dt[, s_bbwp_nue]

  return(value)

}
