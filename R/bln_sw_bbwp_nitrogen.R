#' Function to calculate and evaluate the N buffering capacity of soils in view of water purification for surface quality
#'
#' This function gives the NSW score of the BBWP framework
#'
#' @param B_LU_BRP (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_SC_WENR (integer) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006). Options include: 1,2,3,4,5,10,11,401,901 and 902.
#' @param B_SLOPE_DEGREE (numeric) The slope of the field (degrees)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' @param D_SA_W (numeric) The wet perimeter index of the field, fraction that field is surrounded by water
#' @param D_RO_R (numeric) The risk that surface water runs off the parcel
#' @param B_N_RT (numeric) The mean organic nitrogen content of the soil in the LSW region (mg N / kg). Optional.
#' @param B_N_RT_SD (numeric) The variance in organic nitrogen content of the soil in the LSW region (standard deviation) (mg N / kg). Optional.
#' @param D_RO_R (numeric) The mean D_RO_R of the soil in the LSW region. Optional.
#' @param D_RO_R_SD (numeric) The variance in D_RO_R in the LSW region (standard deviation). Optional.
#' @param B_CT_NSW (numeric) the critical target for required reduction in N loss from agriculture (kg N / ha) to reach targets of KRW
#' @param B_CT_NSW_MAX (numeric) the max critical target for N reduction loss (kg N / ha)
#' @param penalty (boolean) the option to apply a penalty for high risk BBWP field indicators. Default is TRUE.
#'
#' @import data.table
#' @import OBIC
#'
#' @export
bln_bbwp_nsw <- function(B_LU_BRP,B_SOILTYPE_AGR,B_SC_WENR,B_AER_CBS,B_GWL_CLASS,B_SLOPE_DEGREE,A_SOM_LOI,A_N_RT,
                         D_RO_R, D_SA_W,
                         B_N_RT = NA_real_,B_RO_R = NA_real_,B_RO_R_SD = NA_real_,B_N_RT_SD = NA_real_,
                         B_CT_NSW, B_CT_NSW_MAX = 5, penalty = TRUE){

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
                   B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   D_RO_R = D_RO_R,
                   D_SA_W = D_SA_W,
                   B_N_RT = B_N_RT,
                   B_N_RT_SD = B_N_RT_SD,
                   B_RO_R = B_RO_R,
                   B_RO_R_SD = B_RO_R_SD
  )

  # check on BBWP format Gt
  cols <- c('GtI','GtII','GtIII','GtIV','GtV', 'GtVI','GtVII','GtVIII')
  dt[!B_GWL_CLASS %in% cols & grepl('lg14|limburg',tolower(B_AER_CBS)), B_GWL_CLASS := 'GtVIII']
  dt[!B_GWL_CLASS %in% cols & !grepl('lg14|limburg',tolower(B_AER_CBS)), B_GWL_CLASS := 'GtII']

  # replace missing LSW properties
  dt[is.na(B_N_RT), B_N_RT := dt.lsw$B_N_RT]
  dt[is.na(B_N_RT_SD), B_N_RT_SD := dt.lsw$B_N_RT_SD]
  dt[is.na(B_RO_R), B_RO_R := dt.lsw$B_RO_R]
  dt[is.na(B_RO_R_SD), B_RO_R_SD := dt.lsw$B_RO_R_SD]

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

  # reclassify soil compaction risk (scr) into a numeric value
  dt[,nsw_scr := 1 - ngw_scr]

  # reclassify the groundwater table (gwt) into a numeric value
  dt[B_GWL_CLASS %in% c('GtI', '-'), nsw_gwt := 1]
  dt[B_GWL_CLASS %in% c('GtIIb','GtIIIb','GtVb'), nsw_gwt := 0.9]
  dt[B_GWL_CLASS %in% c('GtII','GtIII','GtV'), nsw_gwt := 0.8]
  dt[B_GWL_CLASS %in% c('GtIV'), nsw_gwt := 0.7]
  dt[B_GWL_CLASS %in% c('GtVI'), nsw_gwt := 0.6]
  dt[B_GWL_CLASS %in% c('GtVII'), nsw_gwt := 0.5]
  dt[B_GWL_CLASS %in% c('GtVIII'), nsw_gwt := 0.4]

  # rank the risk for surface runoff (van Hattum, 2011)
  # higher risk is associated to increased risks for N runoff
  dt[,nsw_ro := pnorm(q = D_RO_R, mean = B_RO_R, sd = B_RO_R_SD)]

  # classify fields with a high slope as extra vulnerable for surface runoff
  # with fields with slope > 2% being vulnerabile (Groenendijk, 2020)
  dt[,nsw_slope := pmax(0.2,pmin(1,B_SLOPE_DEGREE/2))]

  # assess the risk for wet surroundings (Van Gerven, 2018): a high fraction equals a high risk
  # higher risk is associated to increased risks for N runoff
  dt[,nsw_ws := pmin(1,pmax(0,D_SA_W))]

  # rank the nitrogen content as an estimate of total N content: a high value means high risk for N leaching
  dt[,ngw_nlv := pnorm(q = A_N_RT, mean = B_N_RT, sd = B_N_RT_SD)]

  # rank the risk for N pool in soil: higher NLV is associated to increased risks for N runoff
  dt[,nsw_nlv := ngw_nlv]

  # do nlv correction for grassland
  dt[grepl('gras',crop_cat1), nsw_nlv := pmax(0, nsw_nlv - 0.5)]


  # estimate field indicators

  # columns to be selected
  cols <- colnames(dt)[grepl('nsw_|id',colnames(dt))]

  # melt the data.table to simplify corrections
  dt.melt <- data.table::melt(dt[,mget(cols)], id.vars = 'id',variable.name = 'risk')

  # add correction factor based on risk itself
  dt.melt[,risk_cor := wf(value,type = "indicators",penalty = penalty)]

  # add groups of risk indicators
  dt.melt[,group := gsub('_[a-z]+$','',gsub('d_','',risk))]

  # add manual weighing factor for risks
  dt.melt[,mcf := 1]
  dt.melt[group=='nsw' & grepl('_nlv$',risk), mcf := 3]

  # add criteria properties as column (to use as filter)
  dt.melt[,ws := value[risk=='nsw_ws'],by='id']
  dt.melt[,slope := value[risk=='nsw_slope'],by='id']

  # ensure that the final risk after aggregation gets the value 0.1 or 0.01
  dt.melt[ws <= 0.2 & slope < 1 & group %in% c('nsw'), c('mcf','risk_cor','value') :=  list(1,1000,0.1)]
  dt.melt[ws <= 0.1 & slope < 1 & group %in% c('nsw'), c('mcf','risk_cor','value') :=  list(1,1000,0.01)]
  dt.melt[,c('ws','slope') := NULL]

  # calculate the mean aggregated risk indicators
  dt.ind <- dt.melt[,list(risk = sum(risk_cor * value * mcf)/sum(risk_cor * mcf)),by=c('id','group')]
  dt.ind <- dcast(dt.ind,id~group,value.var='risk')

  # sort output based on id
  setorder(dt.ind,id)

  # add field indicator to the dt
  dt <- merge(dt,dt.ind,by='id')

  # estimate field score

  # correction when field is in a region with high target for N load reduction surface water
  dt[,cfnsw := pmax(0,pmin(1,B_CT_NSW / B_CT_NSW_MAX))]

  # replace to max critical limit when no information is ready
  dt[is.na(cfnsw), cfnsw := 1]

  # calculate the individual opportunity indexes
  dt[,d_opi_nsw := (0.5 + cfnsw/2) * bln_evaluate_logistic(ngw, b=6, x0=0.4, v=.7)]

  # update the field score with measures (assuming no measures to be taken)
  dt[,d_opi_nsw := pmax(0,1 - pmax(0, d_opi_nsw - 0))]

  # # set bbwp nsw score
  dt[,s_bbwp_nsw :=  d_opi_nsw]

  # extract value
  value <- dt[, round(s_bbwp_nsw,2)]

  # return value
  return(value)

}
