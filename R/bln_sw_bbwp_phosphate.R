#' Function to calculate and evaluate the P buffering capacity of soils in view of water purification for surface quality
#'
#' This function gives the PSW score of the BBWP framework
#'
#' @param B_LU_BRP (numeric) The crop code
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_SC_WENR (integer) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006). Options include: 1,2,3,4,5,10,11,401,901 and 902.
#' @param B_SLOPE_DEGREE (numeric) The slope of the field (degrees)
#' @param A_FE_OX (numeric) The aluminium content of soil (mmol / kg)
#' @param A_AL_OX (numeric) The iron content of soil (mmol / kg)
#' @param A_P_CC (numeric) The plant available P content, measured via 0.01M CaCl2 extraction (mg / kg)
#' @param A_P_SG (numeric) The P-saturation index (\%)
#' @param D_SA_W (numeric) The wet perimeter index of the field, fraction that field is surrounded by water
#' @param D_RO_R (numeric) The risk that surface water runs off the parcel
#' @param D_RO_R (numeric) The mean D_RO_R of the soil in the LSW region. Optional.
#' @param D_RO_R_SD (numeric) The variance in D_RO_R in the LSW region (standard deviation). Optional.
#' @param B_CT_PSW (numeric) the critical target for required reduction in P loss from agriculture (kg P / ha) to reach targets of KRW
#' @param B_CT_PSW_MAX (numeric) the max critical target for P reduction loss (kg P / ha)
#' @param penalty (boolean) the option to apply a penalty for high risk BBWP field indicators. Default is TRUE.
#'
#' @import data.table
#' @import OBIC
#'
#' @export
bln_bbwp_psw <- function(B_LU_BRP,B_SC_WENR,B_AER_CBS,B_GWL_CLASS,B_SLOPE_DEGREE,
                         A_P_CC, A_P_SG, A_AL_OX, A_FE_OX,
                         D_RO_R, D_SA_W,
                         B_RO_R = NA_real_,B_RO_R_SD = NA_real_,
                         B_P_CC = NA_real_, B_P_CC_SD = NA_real_, B_P_SG = NA_real_, B_P_SG_SD = NA_real_,
                         B_AL_OX = NA_real_, B_AL_OX_SD = NA_real_, B_FE_OX = NA_real_, B_FE_OX_SD = NA_real_,
                         B_CT_PSW, B_CT_PSW_MAX = 0.5, penalty = TRUE){

  # load internal table
  dt.lsw <- BLN::bln_lsw[B_LSW_ID == 'lsw_nlmean']
  dt.crop <- BLN::bln_crops[bln_country=='NL']

  # make internal table
  dt <- data.table(id = 1:length(B_LU_BRP),
                   B_LU_BRP = B_LU_BRP,
                   B_SC_WENR = B_SC_WENR,
                   B_GWL_CLASS=B_GWL_CLASS,
                   B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                   B_AER_CBS=B_AER_CBS,
                   A_P_CC = A_P_CC,
                   A_P_SG = A_P_SG,
                   A_AL_OX = A_AL_OX,
                   A_FE_OX = A_FE_OX,
                   D_RO_R = D_RO_R,
                   D_SA_W = D_SA_W,
                   B_RO_R = B_RO_R,
                   B_RO_R_SD = B_RO_R_SD,
                   B_P_CC = B_P_CC,
                   B_P_CC_SD = B_P_CC_SD,
                   B_P_SG = B_P_SG,
                   B_P_SG_SD = B_P_SG_SD,
                   B_AL_OX = B_AL_OX,
                   B_AL_OX_SD = B_AL_OX_SD,
                   B_FE_OX = B_FE_OX,
                   B_FE_OX_SD = B_FE_OX_SD
  )

  # check on BBWP format Gt
  cols <- c('GtI','GtII','GtIII','GtIV','GtV', 'GtVI','GtVII','GtVIII')
  dt[!B_GWL_CLASS %in% cols & grepl('lg14|limburg',tolower(B_AER_CBS)), B_GWL_CLASS := 'GtVIII']
  dt[!B_GWL_CLASS %in% cols & !grepl('lg14|limburg',tolower(B_AER_CBS)), B_GWL_CLASS := 'GtII']

  # replace missing LSW properties
  dt[is.na(B_P_CC), B_P_CC := dt.lsw$B_P_CC]
  dt[is.na(B_P_CC_SD), B_P_CC_SD := dt.lsw$B_P_CC_SD]
  dt[is.na(B_P_SG), B_P_SG := dt.lsw$B_P_SG]
  dt[is.na(B_P_SG_SD), B_P_SG_SD := dt.lsw$B_P_SG_SD]
  dt[is.na(B_AL_OX), B_AL_OX := dt.lsw$B_AL_OX]
  dt[is.na(B_AL_OX_SD), B_AL_OX_SD := dt.lsw$B_AL_OX_SD]
  dt[is.na(B_FE_OX), B_FE_OX := dt.lsw$B_FE_OX]
  dt[is.na(B_FE_OX_SD), B_FE_OX_SD := dt.lsw$B_FE_OX_SD]
  dt[is.na(B_RO_R), B_RO_R := dt.lsw$B_RO_R]
  dt[is.na(B_RO_R_SD), B_RO_R_SD := dt.lsw$B_RO_R_SD]

  # merge with crop category
  dt <- merge(dt,dt.crop[,.(crop_code,crop_cat1)],by.x='B_LU_BRP',by.y='crop_code',all.x = TRUE)

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
  dt[,psw_scr := 1 - ngw_scr]

  # reclassify the groundwater table (gwt) into a numeric value
  dt[B_GWL_CLASS %in% c('GtI', '-'), psw_gwt := 1]
  dt[B_GWL_CLASS %in% c('GtIIb','GtIIIb','GtVb'), psw_gwt := 0.9]
  dt[B_GWL_CLASS %in% c('GtII','GtIII','GtV'), psw_gwt := 0.8]
  dt[B_GWL_CLASS %in% c('GtIV'), psw_gwt := 0.7]
  dt[B_GWL_CLASS %in% c('GtVI'), psw_gwt := 0.6]
  dt[B_GWL_CLASS %in% c('GtVII'), psw_gwt := 0.5]
  dt[B_GWL_CLASS %in% c('GtVIII'), psw_gwt := 0.4]

  # rank the risk for surface runoff (van Hattum, 2011)
  # higher risk is associated to increased risks for N runoff
  dt[,psw_ro := pnorm(q = D_RO_R, mean = B_RO_R, sd = B_RO_R_SD)]

  # classify fields with a high slope as extra vulnerable for surface runoff
  # with fields with slope > 2% being vulnerabile (Groenendijk, 2020)
  dt[,psw_slope := pmax(0.2,pmin(1,B_SLOPE_DEGREE/2))]

  # assess the risk for wet surroundings (Van Gerven, 2018): a high fraction equals a high risk
  # higher risk is associated to increased risks for N runoff
  dt[,nsw_ws := pmin(1,pmax(0,D_SA_W))]

  # rank the risk for wet surroundings (Van Gerven, 2018)
  dt[,psw_ws := nsw_ws]

  # rank the risk for P pools in soil
  dt[,psw_pcc := pnorm(q = A_P_CC, mean = B_P_CC, sd = B_P_CC_SD)]
  dt[,psw_psg := pnorm(q = A_P_SG, mean = B_P_SG, sd = B_P_SG_SD)]
  dt[,psw_pret := 1- pnorm(q =  A_AL_OX + A_FE_OX,
                           mean = B_AL_OX + B_FE_OX,
                           sd =  sqrt(B_AL_OX_SD^2 + B_FE_OX_SD^2))]

  # estimate field indicators

  # columns to be selected
  cols <- colnames(dt)[grepl('psw_|id',colnames(dt))]

  # melt the data.table to simplify corrections
  dt.melt <- data.table::melt(dt[,mget(cols)], id.vars = 'id',variable.name = 'risk')

  # add correction factor based on risk itself
  dt.melt[,risk_cor := wf(value,type = "indicators",penalty = penalty)]

  # add groups of risk indicators
  dt.melt[,group := gsub('_[a-z]+$','',gsub('d_','',risk))]

  # add manual weighing factor for risks
  dt.melt[,mcf := 1]
  dt.melt[group=='psw' & grepl('_scr$|_ro$|_ws$',risk), mcf := 2]

  # add criteria properties as column (to use as filter)
  dt.melt[,ws := value[risk=='psw_ws'],by='id']
  dt.melt[,slope := value[risk=='psw_slope'],by='id']

  # ensure that the final risk after aggregation gets the value 0.1 or 0.01
  dt.melt[ws <= 0.2 & slope < 1 & group %in% c('psw'), c('mcf','risk_cor','value') :=  list(1,1000,0.1)]
  dt.melt[ws <= 0.1 & slope < 1 & group %in% c('psw'), c('mcf','risk_cor','value') :=  list(1,1000,0.01)]
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
  dt[,cfpsw := pmax(0,pmin(1,B_CT_PSW / B_CT_PSW_MAX))]

  # replace to max critical limit when no information is ready
  dt[is.na(cfpsw), cfpsw := 1]

  # calculate the individual opportunity indexes
  dt[,d_opi_psw := (0.5 + cfpsw/2) * bln_evaluate_logistic(ngw, b=6, x0=0.4, v=.7)]

  # update the field score with measures (assuming no measures to be taken)
  dt[,d_opi_psw := pmax(0,1 - pmax(0, d_opi_psw - 0))]

  # set bbwp psw score
  dt[,s_bbwp_psw := d_opi_psw]

  # extract value
  value <- dt[, round(s_bbwp_psw,2)]

  # return value
  return(value)

}
