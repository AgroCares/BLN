#' Function to calculate and evaluate the N buffering capacity of soils in view of water purification for groundwater quality
#'
#' This function gives the NGW score of the BBWP framework
#'
#' @param B_LU_BRP (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_GWP (boolean) is the field located in a groundwater protected area (options: TRUE or FALSE)
#' @param B_SC_WENR (integer) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006). Options include: 1,2,3,4,5,10,11,401,901 and 902.
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' @param B_N_RT (numeric) The mean organic nitrogen content of the soil in the LSW region (mg N / kg). Optional.
#' @param B_N_RT_SD (numeric) The variance in organic nitrogen content of the soil in the LSW region (standard deviation) (mg N / kg). Optional.
#' @param penalty (boolean) the option to apply a penalty for high risk BBWP field indicators. Default is TRUE.
#'
#' @import data.table
#' @import OBIC
#'
#' @export
bln_bbwp_ngw <- function(B_LU_BRP,B_SOILTYPE_AGR,B_SC_WENR,B_AER_CBS,B_GW,B_GWL_CLASS,A_SOM_LOI,A_N_RT,
                         B_N_RT = NA_real_,B_N_RT_SD = NA_real_, penalty = TRUE){

  # load internal table
  dt.lsw <- BLN::bln_lsw[B_LSW_ID == 'lsw_nlmean']
  dt.crop <- BLN::bln_crops[bln_country=='NL']
  dt.soil <- BLN::bln_soiltype[bln_country=='NL']

  # make internal table
  dt <- data.table(id = 1:length(B_LU_BRP),
                   B_LU_BRP = B_LU_BRP,
                   B_SC_WENR = B_SC_WENR,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_GWL_CLASS=B_GWL_CLASS,
                   B_AER_CBS=B_AER_CBS,
                   B_GWP = B_GWP,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   B_N_RT = B_N_RT,
                   B_N_RT_SD = B_N_RT_SD)

  # check on BBWP format Gt
  cols <- c('GtI','GtII','GtIII','GtIV','GtV', 'GtVI','GtVII','GtVIII')
  dt[!B_GWL_CLASS %in% cols & grepl('lg14|limburg',tolower(B_AER_CBS)), B_GWL_CLASS := 'GtVIII']
  dt[!B_GWL_CLASS %in% cols & !grepl('lg14|limburg',tolower(B_AER_CBS)), B_GWL_CLASS := 'GtII']

  # replace missing LSW properties
  dt[is.na(B_N_RT), B_N_RT := dt.lsw$B_N_RT]
  dt[is.na(B_N_RT_SD), B_N_RT_SD := dt.lsw$B_N_RT_SD]

  # merge with crop category
  dt <- merge(dt,dt.crop[,.(crop_code,crop_cat1)],by.x='B_LU_BRP',by.y='crop_code',all.x = TRUE)
  dt <- merge(dt,dt.soil[,.(bln_soil_cat1,bln_soil_cat2)],by.x='B_SOILTYPE_AGR',by.y='bln_soil_cat1',all.x=TRUE)

  # estimate field properties

  # reclassify soil compaction risk (scr) into a numeric value
  # a high value is indicative of high risk of leaching of nitrogen to groundwater
  dt[B_SC_WENR %in% c(902, 901, 401),ngw_scr := 1]
  dt[B_SC_WENR == 1, ngw_scr := 1]
  dt[B_SC_WENR %in% c(2, 10), ngw_scr := 0.8]
  dt[B_SC_WENR == 3, ngw_scr := 0.6]
  dt[B_SC_WENR == 4, ngw_scr := 0.4]
  dt[B_SC_WENR %in% c(5, 11), ngw_scr := 0.2]

  # Re-categorize crop and soil types to match OBIC table
  dt[crop_cat1 == 'arable',crop_cat1_nl := 'akkerbouw']
  dt[crop_cat1 == 'maize',crop_cat1_nl := 'mais']
  dt[crop_cat1 %in% c('grassland','nature'),crop_cat1_nl := 'gras']
  dt[bln_soil_cat2 %in% c('sand','loess'), bln_soil_cat2_nl := 'zand']
  dt[bln_soil_cat2 == 'clay', bln_soil_cat2_nl := 'klei']
  dt[bln_soil_cat2 == 'peat', bln_soil_cat2_nl := 'veen']

  # merge fraction of N leaching into 'dt', based on soil type x crop type x grondwatertrap
  dt <- merge(dt, OBIC::nleach_table[, list(bln_soil_cat2_nl = bodem, crop_cat1_nl = gewas, B_GWL_CLASS = B_GT, nf)],
              by = c("bln_soil_cat2_nl", "crop_cat1_nl", "B_GWL_CLASS"), all.x = TRUE)

  # for situations that nf is unknown
  dt[is.na(nf), nf := 0.5]

  # rank the risk on soil N leaching to groundwater given crop type, soil type and gt
  # a high value means high risks for N leaching
  dt[,ngw_lea := nf / max(OBIC::nleach_table$nf)]

  # rank the nitrogen content as an estimate of total N content: a high value means high risk for N leaching
  dt[,ngw_nlv := pnorm(q = A_N_RT, mean = B_N_RT, sd = B_N_RT_SD)]

  # estimate field indicators

  # columns to be selected
  cols <- colnames(dt)[grepl('ngw_|id',colnames(dt))]

  # melt the data.table to simplify corrections
  dt.melt <- data.table::melt(dt[,mget(cols)], id.vars = 'id',variable.name = 'risk')

  # add correction factor based on risk itself
  dt.melt[,risk_cor := wf(value,type = "indicators",penalty = penalty)]

  # add groups of risk indicators
  dt.melt[,group := gsub('_[a-z]+$','',gsub('d_','',risk))]

  # add manual weighing factor for risks
  dt.melt[,mcf := 1]
  dt.melt[group=='ngw' & grepl('_lea$',risk), mcf := 3]
  dt.melt[group=='ngw' & grepl('_nlv$',risk), mcf := 2]

  # calculate the mean aggregated risk indicators
  dt.ind <- dt.melt[,list(risk = sum(risk_cor * value * mcf)/sum(risk_cor * mcf)),by=c('id','group')]
  dt.ind <- dcast(dt.ind,id~group,value.var='risk')

  # sort output based on id
  setorder(dt.ind,id)

  # add field indicator to the dt
  dt <- merge(dt,dt.ind,by='id')

  # estimate field score

  # correction when field is in a ground water protection zone
  dt[,cfngw := fifelse(B_GWP, 1, 0.5)]

  # lower the regional target for nitrate leaching (compared to the general target 1)
  dt[B_GWL_CLASS %in% c('GtI','GtII','GtIII'), cfngw := cfngw * 0.5]
  dt[B_SOILTYPE_AGR == 'veen', cfngw := cfngw * 0.1]

  # calculate the individual opportunity indexes
  dt[,d_opi_ngw := (0.5 + cfngw/2) * bln_evaluate_logistic(ngw, b=6, x0=0.4, v=.7)]

  # update the field score with measures (assuming no measures to be taken)
  dt[,d_opi_ngw := pmax(0,1 - pmax(0, d_opi_ngw - 0))]

  # # set bbwp ngw score
  dt[,s_bbwp_ngw := d_opi_ngw]

  # extract value
  value <- dt[, round(s_bbwp_ngw,2)]

  # return value
  return(value)

}
