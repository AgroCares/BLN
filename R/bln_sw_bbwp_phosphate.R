#' Function to calculate and evaluate the P buffering capacity of soils in view of water purification for surface quality
#'
#' This function gives the PSW score of the BBWP framework
#'
#' @param ID (character) A field id
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
#' @param B_RO_R (numeric) The mean D_RO_R of the soil in the LSW region. Optional.
#' @param B_RO_R_SD (numeric) The variance in D_RO_R in the LSW region (standard deviation). Optional.
#' @param B_P_CC (numeric) The mean P-CaCl2 content of the soil in the LSW region (mg P / kg). Optional.
#' @param B_P_CC_SD (numeric) The variance in P-CaCl2 content  of the soil in the LSW region (standard deviation) (mg P / kg). Optional.
#' @param B_P_SG (numeric) The mean P saturation degree of the soil in the LSW region (mg N / kg). Optional.
#' @param B_P_SG_SD (numeric) The variance in P saturation degree of the soil in the LSW region (standard deviation) (\%). Optional.
#' @param B_AL_OX (numeric) The mean Al-oxide content of the soil in the LSW region (\%). Optional.
#' @param B_AL_OX_SD (numeric) The variance in Al-oxide content of the soil in the LSW region (standard deviation) (mmol+ / kg). Optional.
#' @param B_FE_OX (numeric) The mean Fe-oxide content of the soil in the LSW region (mmol+ / kg). Optional.
#' @param B_FE_OX_SD (numeric) The variance in Fe-oxide content of the soil in the LSW region (standard deviation) (mmol+ / kg). Optional.
#' @param B_CT_PSW (numeric) the critical target for required reduction in P loss from agriculture (kg P / ha) to reach targets of KRW
#' @param B_CT_PSW_MAX (numeric) the max critical target for P reduction loss (kg P / ha)
#' @param penalty (boolean) the option to apply a penalty for high risk BBWP field indicators. Default is TRUE.
#'
#' @import data.table
#' @import OBIC
#'
#' @export
bln_bbwp_psw <- function(ID,B_LU_BRP,B_SC_WENR,B_AER_CBS,B_GWL_CLASS,B_SLOPE_DEGREE,
                         A_P_CC, A_P_SG, A_AL_OX, A_FE_OX,
                         D_RO_R, D_SA_W,
                         B_RO_R = NA_real_,B_RO_R_SD = NA_real_,
                         B_P_CC = NA_real_, B_P_CC_SD = NA_real_, B_P_SG = NA_real_, B_P_SG_SD = NA_real_,
                         B_AL_OX = NA_real_, B_AL_OX_SD = NA_real_, B_FE_OX = NA_real_, B_FE_OX_SD = NA_real_,
                         B_CT_PSW, B_CT_PSW_MAX = 0.5, penalty = TRUE){

  # add visual bindings
  bln_country = code = choices = value_min = value_max = . = crop_cat1 = crop_code = NULL
  ngw_scr = psw_scr = psw_gwt = psw_ro = psw_slope = nsw_ws = psw_ws = psw_pcc = psw_psg = psw_pret = risk_cor = NULL
  B_LSW_ID = group = risk = mcf= ID_CROP = ws = slope = cfpsw = d_opi_psw = psw = NULL

  # load internal table
  dt.lsw <- BLN::bln_lsw[B_LSW_ID == 'lsw_nlmean']
  dt.crop <- BLN::bln_crops[bln_country=='NL']
  blnp <- BLN::bln_parms

  # check input length
  arg.length <- max(length(B_LU_BRP), length(B_SC_WENR),length(B_AER_CBS),
                    length(B_GWL_CLASS),length(B_SLOPE_DEGREE),length(A_P_CC),length(A_P_SG),
                    length(A_AL_OX),length(A_FE_OX),
                    length(D_RO_R),length(D_SA_W),length(B_CT_PSW))

  # adjust input format
  B_AER_CBS <- bln_format_aer(B_AER_CBS,type='code')

  # check input
  checkmate::assert_subset(B_LU_BRP, choices = unlist(BLN::bln_crops$crop_code))
  checkmate::assert_integerish(B_LU_BRP, len = arg.length)
  checkmate::assert_subset(B_SC_WENR, choices = unlist(blnp[code == "B_SC_WENR", choices]))
  checkmate::assert_integerish(B_SC_WENR, len = arg.length)
  checkmate::assert_subset(B_AER_CBS, choices = unlist(blnp[code == "B_AER_CBS", choices]))
  checkmate::assert_character(B_AER_CBS, len = arg.length)
  checkmate::assert_subset(B_GWL_CLASS, choices = unlist(blnp[code == "B_GWL_CLASS", choices]))
  checkmate::assert_character(B_GWL_CLASS, len = arg.length)
  checkmate::assert_logical(penalty,len = 1)
  checkmate::assert_numeric(B_CT_PSW, lower = 0, upper = 100, len = arg.length)

  if(length(B_P_CC)>1){checkmate::assert_numeric(B_P_CC,lower =blnp[code == "A_P_CC", value_min], upper = blnp[code == "A_P_CC", value_max],len = arg.length)}
  if(length(B_P_SG)>1){checkmate::assert_numeric(B_P_SG,lower =blnp[code == "A_P_SG", value_min], upper = blnp[code == "A_P_SG", value_max],len = arg.length)}
  if(length(B_AL_OX)>1){checkmate::assert_numeric(B_AL_OX,lower =blnp[code == "A_AL_OX", value_min], upper = blnp[code == "A_AL_OX", value_max],len = arg.length)}
  if(length(B_FE_OX)>1){checkmate::assert_numeric(B_FE_OX,lower =blnp[code == "A_FE_OX", value_min], upper = blnp[code == "A_FE_OX", value_max],len = arg.length)}
  if(length(B_RO_R)>1){checkmate::assert_numeric(B_RO_R,lower =0, upper = 2,len = arg.length)}
  if(length(B_P_CC_SD)>1){checkmate::assert_numeric(B_P_CC_SD,lower = 0, upper = blnp[code == "A_P_CC", value_max],len = arg.length)}
  if(length(B_P_SG_SD)>1){checkmate::assert_numeric(B_P_SG_SD,lower = 0, upper = blnp[code == "A_P_SG", value_max],len = arg.length)}
  if(length(B_AL_OX_SD)>1){checkmate::assert_numeric(B_AL_OX_SD,lower = 0, upper = blnp[code == "A_AL_OX", value_max],len = arg.length)}
  if(length(B_FE_OX_SD)>1){checkmate::assert_numeric(B_FE_OX_SD,lower = 0, upper = blnp[code == "A_FE_OX", value_max],len = arg.length)}

  # check inputs A parameters
  checkmate::assert_numeric(B_SLOPE_DEGREE,lower = blnp[code == "B_SLOPE_DEGREE", value_min], upper = blnp[code == "B_SLOPE_DEGREE", value_max],len = arg.length)
  checkmate::assert_numeric(A_P_CC, lower = blnp[code == "A_P_CC", value_min], upper = blnp[code == "A_P_CC", value_max],len = arg.length)
  checkmate::assert_numeric(A_P_SG, lower = blnp[code == "A_P_SG", value_min], upper = blnp[code == "A_P_SG", value_max],len = arg.length)
  checkmate::assert_numeric(A_AL_OX, lower = blnp[code == "A_AL_OX", value_min], upper = blnp[code == "A_AL_OX", value_max],len = arg.length)
  checkmate::assert_numeric(A_FE_OX, lower = blnp[code == "A_FE_OX", value_min], upper = blnp[code == "A_FE_OX", value_max],len = arg.length)
  checkmate::assert_numeric(D_RO_R, lower =0, upper = 1,len = arg.length)
  checkmate::assert_numeric(D_SA_W, lower = 0, upper = 1, len = arg.length)


  # make internal table
  dt <- data.table(ID_FIELD = ID,
                   ID_CROP = 1:length(B_LU_BRP),
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
  dt[,B_SC_WENR := tolower(B_SC_WENR)]
  dt[grepl('bebouwing|water|glastuinbouw|^401$|^901$|^902$',B_SC_WENR),ngw_scr := 1]
  dt[grepl('zeer beperkt|^1$',B_SC_WENR), ngw_scr := 1]
  dt[grepl('^beperkt|^2$|^10$',B_SC_WENR), ngw_scr := 0.8]
  dt[grepl('matig|^3$',B_SC_WENR),ngw_scr := 0.6]
  dt[grepl('^groot|^4$',B_SC_WENR), ngw_scr := 0.4]
  dt[grepl('zeer groot|nature|^5$|^11$', B_SC_WENR), ngw_scr := 0.2]

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
  cols <- colnames(dt)[grepl('psw_|ID',colnames(dt))]

  # melt the data.table to simplify corrections
  dt.melt <- data.table::melt(dt[,mget(cols)], id.vars = c('ID_FIELD','ID_CROP'),variable.name = 'risk')

  # add correction factor based on risk itself
  dt.melt[,risk_cor := wf(value,type = "indicators",penalty = penalty)]

  # add groups of risk indicators
  dt.melt[,group := gsub('_[a-z]+$','',gsub('d_','',risk))]

  # add manual weighing factor for risks
  dt.melt[,mcf := 1]
  dt.melt[group=='psw' & grepl('_scr$|_ro$|_ws$',risk), mcf := 2]

  # add criteria properties as column (to use as filter)
  dt.melt[,ws := value[risk=='psw_ws'],by=c('ID_FIELD','ID_CROP')]
  dt.melt[,slope := value[risk=='psw_slope'],by=c('ID_FIELD','ID_CROP')]

  # ensure that the final risk after aggregation gets the value 0.1 or 0.01
  dt.melt[ws <= 0.2 & slope < 1 & group %in% c('psw'), c('mcf','risk_cor','value') :=  list(1,1000,0.1)]
  dt.melt[ws <= 0.1 & slope < 1 & group %in% c('psw'), c('mcf','risk_cor','value') :=  list(1,1000,0.01)]
  dt.melt[,c('ws','slope') := NULL]

  # calculate the mean aggregated risk indicators
  dt.ind <- dt.melt[,list(risk = sum(risk_cor * value * mcf)/sum(risk_cor * mcf)),by=c('ID_FIELD','ID_CROP','group')]
  dt.ind <- dcast(dt.ind,ID_FIELD + ID_CROP~group,value.var='risk')

  # sort output based on crop_id, being also the order of input variables
  setorder(dt.ind,ID_CROP)

  # add field indicator to the dt
  dt <- merge(dt,dt.ind,by='ID_CROP',all.x = TRUE)

  # estimate field score

  # correction when field is in a region with high target for N load reduction surface water
  dt[,cfpsw := pmax(0,pmin(1,B_CT_PSW / B_CT_PSW_MAX))]

  # replace to max critical limit when no information is ready
  dt[is.na(cfpsw), cfpsw := 1]

  # calculate the individual opportunity indexes
  dt[,d_opi_psw := (0.5 + cfpsw/2) * bln_evaluate_logistic(psw, b=6, x0=0.4, v=.7)]

  # update the field score with measures (assuming no measures to be taken)
  dt[,d_opi_psw := pmax(0,1 - pmax(0, d_opi_psw - 0))]

  # set bbwp psw score
  dt[,value := d_opi_psw]

  # extract value
  value <- dt[, value]

  # return value
  return(value)

}
