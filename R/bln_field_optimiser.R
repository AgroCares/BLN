#' Optimize the crop rotation plan for soil quality improvement
#'
#' This function estimates what crop rotation plan is mosts suited for the current soil quality and the associated contribution to ecysystem services
#'
#' @param ID (character) A fieldid
#' @param B_LU_BRP (numeric) a series with crop codes given the crop rotation plan (source: the BRP)
#' @param B_SC_WENR (character) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006).
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param B_HELP_WENR (character) The soil type abbreviation, derived from 1:50.000 soil map
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param B_GWL_GLG (numeric) The lowest groundwater level averaged over the most dry periods in 8 years in cm below ground level
#' @param B_GWL_GHG (numeric) The highest groundwater level averaged over the most wet periods in 8 years in cm below ground level
#' @param B_GWL_ZCRIT  (numeric) The distance between ground level and groundwater level at which the groundwater can supply the soil surface with 2mm water per day (in cm)
#' @param B_DRAIN (boolean) Are drains installed to drain the field (options: yes or no)
#' @param B_FERT_NORM_FR (numeric) The fraction of the application norm utilized
#' @param B_SLOPE_DEGREE (numeric) The slope of the field (degrees)
#' @param B_GWP (boolean) is the field located in a groundwater protected area (options: TRUE or FALSE)
#' @param B_AREA_DROUGHT (boolean) is the field located in an area with high risks for water deficiencies (options: TRUE or FALSE)
#' @param B_CT_PSW (numeric) the critical target for required reduction in P loss from agriculture (kg P / ha) to reach targets of KRW
#' @param B_CT_NSW (numeric) the critical target for required reduction in N loss from agriculture (kg N / ha) to reach targets of KRW
#' @param B_SOMERS_BC (integer) The base combination of SOMERS peat soil classification (varies between 1 and 290)
#' @param B_DRAIN_SP (numeric) the drooglegging of a field in summer (difference field height and ditch level, in meters)
#' @param B_DRAIN_WP (numeric) the drooglegging of a field in winter (difference field height and ditch level, in meters)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_SOM_LOI_MLMAX (numeric) The max. percentage organc matter estimated via machine learning model (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_DENSITY_SA (numeric) The bulk density of the soil (kg/m3)
#' @param A_FE_OX (numeric) The aluminium content of soil (mmol / kg)
#' @param A_AL_OX (numeric) The iron content of soil (mmol / kg)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' @param A_CN_FR (numeric) The carbon to nitrogen ratio (-)
#' @param A_S_RT (numeric) The total Sulfur content of the soil (in mg S per kg)
#' @param A_N_PMN (numeric) The potentially mineralizable N pool (mg N / kg soil)
#' @param A_P_AL (numeric) The P-AL content of the soil
#' @param A_P_CC (numeric) The plant available P content, extracted with 0.01M CaCl2 (mg / kg)
#' @param A_P_WA (numeric) The P-content of the soil extracted with water (mg P2O5 / 100 ml soil)
#' @param A_P_SG (numeric) The P-saturation index (\%)
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil (mmol+ / kg), analyzed via Cobalt-hexamine extraction
#' @param A_CA_CO_PO (numeric) The The occupation of the CEC with Ca (\%)
#' @param A_MG_CO_PO (numeric) The The occupation of the CEC with Mg (\%)
#' @param A_K_CO_PO (numeric) The occupation of the CEC with K (\%)
#' @param A_K_CC (numeric) The plant available K content, extracted with 0.01M CaCl2 (mg / kg)
#' @param A_MG_CC (numeric) The plant available Mg content, extracted with 0.01M CaCl2 (ug / kg)
#' @param A_MN_CC (numeric) The plant available Mn content, extracted with 0.01M CaCl2 (ug / kg)
#' @param A_ZN_CC (numeric) The plant available Zn content, extracted with 0.01M CaCl2 (ug / kg)
#' @param A_CU_CC (numeric) The plant available Cu content, extracted with 0.01M CaCl2 (ug / kg)
#' @param D_SA_W (numeric) The wet perimeter index of the field, fraction that field is surrounded by water
#' @param D_RO_R (numeric) The risk that surface water runs off the parcel
#' @param M_COMPOST (numeric) The frequency that compost is applied (optional, every x years)
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#' @param B_LSW_ID (character) An unique identifier for each Local Surface Water per field
#' @param LSW (data.table) The averaged soil properties (mean and sd) per Local Surface Water. Can be derived from bbwp_lsw_properties.
#' @param i_clim_rothc (numeric) the soil indicator for carbon saturation derived via rothc.
#' @param foptim (list) a list with options to guide the identification of suitable crop rotation schemes
#'
#' @details
#' the foptim is a list with parameters affecting the selection of best crop rotation plans.
#' the parameter outputtype gives the user the possibility to select specific outcomes:
#' scores (gives all scores per rotation)
#' indicators (gives the indicator values per rotation)
#' bottlenecks (gives the number of bottlenecks per rotation)
#' rotation (gives the best rotation for BLN total, per ESD and OBI)
#' all (all output options)
#' The parameter b_lu_brp allows the user to send an user defined crop rotation using BRP codes. This will replace the default scenarios as long as the parameter scenarios is NULL.
#' The parameter scenarios allows the user to select only specific crop rotation scenarios from the package table `bln_scen_croprotation`
#'
#'@export
bln_field_optimiser<-function(ID, B_LU_BRP,B_SC_WENR,B_GWL_CLASS,B_SOILTYPE_AGR,B_HELP_WENR,B_AER_CBS,
                              B_GWL_GLG,B_GWL_GHG,B_GWL_ZCRIT,B_DRAIN,B_FERT_NORM_FR,B_SLOPE_DEGREE,B_GWP,
                              B_AREA_DROUGHT,B_CT_PSW,B_CT_NSW,
                              B_SOMERS_BC,B_DRAIN_SP,B_DRAIN_WP,
                              A_SOM_LOI, A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_FE_OX,A_AL_OX,A_PH_CC,A_N_RT,
                              A_CN_FR,A_S_RT,A_N_PMN,A_P_AL,A_P_CC,A_P_WA,A_P_SG,A_CEC_CO,A_CA_CO_PO,A_MG_CO_PO,
                              A_K_CO_PO,A_K_CC,A_MG_CC,A_MN_CC,A_ZN_CC,A_CU_CC,
                              D_SA_W,D_RO_R,M_COMPOST,M_GREEN,
                              A_DENSITY_SA = NA_real_,
                              B_LSW_ID = NA_character_,LSW = NULL, i_clim_rothc = NA_real_,A_SOM_LOI_MLMAX = NA_real_,
                              foptim = list(scenarios = NULL, b_lu_brp = NULL, outputtype = 'rotation',mc = TRUE,runrothc = TRUE)){

  # add visial bindings
  scen = soiltype = . = b_aer_cbs = fieldid = b_lu_brp = B_CT_PSW_MAX = variable = indicator = s_bln_total = esd = NULL

  # load internal table with crop rotation scenarios
  bln_rot_scen <- BLN::bln_scen_croprotation

  # filter on crop rotation scenario options given
  if(!is.null(foptim$scenarios)){

    scen.options <- unique(foptim$scenarios)
    checkmate::assert_character(scen.options, min.len = 1)
    checkmate::assert_subset(scen.options, choices = unique(bln_rot_scen$scen), empty.ok = FALSE)
    bln_rot_scen <- bln_rot_scen[scen %in% scen.options]

  }

  # add user defined crop rotation scenario
  if(!is.null(foptim$b_lu_brp)){

    brp.options <- foptim$b_lu_brp
    checkmate::assert_subset(brp.options, choices = unique(BLN::bln_crops$crop_code), empty.ok = FALSE)
    tmp1 <- CJ(b_aer_cbs = unique(bln_rot_scen$b_aer_cbs),b_lu_brp = brp.options)
    tmp1[,year := 1:.N,by='b_aer_cbs']
    tmp1[,soiltype := 'all']
    tmp1[,scen := 'userinput']
  }

  # adjust scenario inputs given user inputs given
  if(!is.null(foptim$b_lu_brp) & !is.null(foptim$b_lu_brp)){

    bln_rot_scen <- rbind(bln_rot_scen,tmp1)

  }

  # check the input of foptim
  checkmate::assert_character(foptim$outputtype)
  checkmate::assert_subset(foptim$outputtype,choices = c('all','scores','indicators','bottlenecks','rotation'))

  # combine all inputs
  dt <- data.table(ID = ID,
                   B_LU_BRP = B_LU_BRP,
                   B_SC_WENR = B_SC_WENR,
                   B_GWL_CLASS = B_GWL_CLASS,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_HELP_WENR = B_HELP_WENR,
                   B_AER_CBS = B_AER_CBS,
                   B_GWL_GLG = pmax(0,B_GWL_GLG),
                   B_GWL_GHG = B_GWL_GHG,
                   B_GWL_ZCRIT = B_GWL_ZCRIT,
                   B_DRAIN = B_DRAIN,
                   B_FERT_NORM_FR = B_FERT_NORM_FR,
                   B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                   B_GWP = B_GWP,
                   B_AREA_DROUGHT = B_AREA_DROUGHT,
                   B_CT_PSW = B_CT_PSW,
                   B_CT_NSW = B_CT_NSW,
                   B_CT_PSW_MAX = 0.5,
                   B_CT_NSW_MAX = 5,
                   B_SOMERS_BC = B_SOMERS_BC,
                   B_DRAIN_SP = B_DRAIN_SP,
                   B_DRAIN_WP = B_DRAIN_WP,
                   A_SOM_LOI = A_SOM_LOI,
                   A_SOM_LOI_MLMAX = A_SOM_LOI_MLMAX,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_DENSITY_SA = A_DENSITY_SA,
                   A_FE_OX = A_FE_OX,
                   A_AL_OX = A_AL_OX,
                   A_PH_CC = A_PH_CC,
                   A_N_RT = A_N_RT,
                   A_CN_FR = A_CN_FR,
                   A_S_RT = A_S_RT,
                   A_N_PMN = A_N_PMN,
                   A_P_AL = A_P_AL,
                   A_P_CC = A_P_CC,
                   A_P_WA = A_P_WA,
                   A_P_SG = A_P_SG,
                   A_CEC_CO = A_CEC_CO,
                   A_CA_CO_PO = A_CA_CO_PO,
                   A_MG_CO_PO = A_MG_CO_PO,
                   A_K_CO_PO = A_K_CO_PO,
                   A_K_CC = A_K_CC,
                   A_MG_CC = A_MG_CC,
                   A_MN_CC = A_MN_CC,
                   A_ZN_CC = A_ZN_CC,
                   A_CU_CC = A_CU_CC,
                   D_SA_W = D_SA_W,
                   D_RO_R = D_RO_R,
                   M_COMPOST = M_COMPOST,
                   M_GREEN = M_GREEN,
                   B_LSW_ID = B_LSW_ID,
                   i_clim_rothc = i_clim_rothc)

  # set format b_aer_cbs to L-code
  dt[, B_AER_CBS := bln_format_aer(B_AER_CBS,type='code')]

  # estimate the mean soil properties per field
  cols <- colnames(dt[1 , .SD, .SDcols = is.numeric])
  cols <- cols[!grepl('LU_BRP|^M_G|^B_LSW_ID|^B_SC_',cols)]
  dt[, c(cols) := lapply(.SD,mean,na.rm=T),.SDcols = cols,by='ID']
  dt[, c(cols) := lapply(.SD,function(x) fifelse(!is.finite(x),NA_real_,x)),.SDcols = cols]

  # get current rotation
  crots.cur <- dt[,.(fieldid = ID,
                     b_aer_cbs = B_AER_CBS,
                     b_lu_brp = B_LU_BRP,
                     soiltype = B_SOILTYPE_AGR,
                     clay = A_CLAY_MI,
                     scen = 'current')]

  # subset bln_rot_scen for only relevant areas (to minimize RAM)
  bln_rot_scen <- bln_rot_scen[b_aer_cbs %in% unique(dt$B_AER_CBS)]

  # get all other crop rotations
  ufields <-  unique(dt$ID)
  crots.scen <- bln_rot_scen[rep(1:.N,length(ufields))]
  crots.scen[,fieldid := rep(ufields,each = nrow(bln_rot_scen))]

  # select the first row as being the latest measurement
  dt <- dt[,.SD[1],by='ID']

  # remove B_LU_BRP
  dt[,B_LU_BRP := NULL]

  # select only the relevant rotations in a given AER_CBS
  crots.scen <- merge(crots.scen,dt[,.(fieldid=ID,B_AER_CBS)],by='fieldid',all.x=TRUE)
  crots.scen <- crots.scen[b_aer_cbs == B_AER_CBS]

  # make final dataset with all crop rotation schemes (10 scenarios per field)
  crots.scen.final <- rbind(crots.scen[,.(fieldid,scen,b_lu_brp)],
                            crots.cur[,.(fieldid,scen,b_lu_brp)])

  # merge file
  dt2 <- merge(crots.scen.final,dt,by.x = 'fieldid',by.y='ID',all.x = TRUE)

  # calculate BLN indicators and scores per scenario
  cols <- c("ID","i_b_di","i_b_sf","i_c_k","i_c_mg","i_c_n","i_c_p","i_c_ph","i_c_s","i_clim_csat"  ,"i_clim_osb","i_clim_rothc", "i_gw_gwr","i_gw_ngw","i_gw_nlea","i_gw_nret","i_gw_pest",
            "i_gw_wb","i_nut_k","i_nut_n","i_nut_nue","i_nut_p","i_p_as","i_p_co","i_p_cr","i_p_ds","i_p_du","i_p_ro","i_p_se","i_p_whc","i_p_wo","i_p_ws","i_sw_nret","i_sw_nro",
            "i_sw_nsw","i_sw_psw",'s_bln_esd_clim','s_bln_esd_nut', 's_bln_esd_prod', 's_bln_esd_water', 's_bln_prod_b', 's_bln_prod_c', 's_bln_clim',
            's_bln_gw_quality', 's_bln_gw_quantity', 's_bln_nut', 's_bln_prod_p', 's_bln_sw_quality', 's_bln_total')

  # set outputs from BLN to default
  dt2[,c(cols[-1]) := 0]
  dt2[,ID := fieldid]

  # analysis soil quality per field and rotation scenario
  dt3 <- dt2[, BLN::bln_field(ID = fieldid,B_LU_BRP = b_lu_brp,B_SC_WENR = B_SC_WENR,B_GWL_CLASS = B_GWL_CLASS,B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                                  B_HELP_WENR = B_HELP_WENR,B_AER_CBS = B_AER_CBS,B_GWL_GLG = B_GWL_GLG,B_GWL_GHG = B_GWL_GHG,B_GWL_ZCRIT = B_GWL_ZCRIT,
                                  B_DRAIN = B_DRAIN,B_FERT_NORM_FR = B_FERT_NORM_FR,B_SLOPE_DEGREE = B_SLOPE_DEGREE,B_GWP = B_GWP,B_AREA_DROUGHT = B_AREA_DROUGHT,
                                  B_CT_PSW = B_CT_PSW,B_CT_NSW = B_CT_NSW, B_CT_PSW_MAX =B_CT_PSW_MAX,B_CT_NSW_MAX = B_CT_PSW_MAX,
                                  B_SOMERS_BC = B_SOMERS_BC,B_DRAIN_SP = B_DRAIN_SP,B_DRAIN_WP = B_DRAIN_WP,
                                  A_SOM_LOI = A_SOM_LOI,A_SOM_LOI_MLMAX = A_SOM_LOI_MLMAX,
                                  A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI,A_DENSITY_SA = A_DENSITY_SA,
                                  A_FE_OX = A_FE_OX,A_AL_OX = A_AL_OX,A_PH_CC = A_PH_CC,A_N_RT = A_N_RT,
                                  A_CN_FR = A_CN_FR,A_S_RT = A_S_RT,A_N_PMN = A_N_PMN,
                                  A_P_AL = A_P_AL,A_P_CC = A_P_CC,A_P_WA = A_P_WA,A_P_SG = A_P_SG,A_CEC_CO = A_CEC_CO,
                                  A_CA_CO_PO = A_CA_CO_PO,A_MG_CO_PO = A_MG_CO_PO,A_K_CO_PO = A_K_CO_PO,
                                  A_K_CC = A_K_CC,A_MG_CC = A_MG_CC,A_MN_CC = A_MN_CC,A_ZN_CC = A_ZN_CC,
                                  A_CU_CC = A_CU_CC,D_SA_W = D_SA_W,D_RO_R = D_RO_R,
                                  M_COMPOST = M_COMPOST,M_GREEN = M_GREEN,
                                  B_LSW_ID = B_LSW_ID,
                                  LSW = LSW,
                                  output ='all',
                                  runrothc = foptim$runrothc,
                                  mc = foptim$mc),by=c('scen')]

  # create output file and add requested summary stats per field
  out <- dt[,.(ID)]

  # count number of bottlenecks
  check <- max(0,sum(foptim$outputtype %in% c('cr_nbottlenecks','bottlenecks','all'),na.rm=T))

  if(check > 0){

    dt3.bc <- melt(dt3,id.vars=c('ID','scen'),value.name = 'indicator')
    dt3.bc <- dt3.bc[grepl('^i_',variable)]
    dt3.bc <- dt3.bc[,sum(indicator <= 0.7,na.rm=T),by=c('ID','scen')]
    dt3.bc[,scen := paste0(scen,'_bnc')]
    dt3.bc <- dcast(dt3.bc,ID~scen,value.var='V1')

    out <- merge(out,dt3.bc,by='ID',all.x=TRUE)

  }

  # select all ESD scores for BLN per rotation scenario
  check <- max(0,sum(foptim$outputtype %in% c('scores','all'),na.rm=T))

  if(check > 0){

    cols <- colnames(dt3)[grepl('ID|scen|^s_bln',colnames(dt3))]
    dt3.score.bln <- dt3[,mget(cols)]
    dt3.score.bln <- melt(dt3.score.bln,id.vars = c('scen','ID'),variable.name='bln_score',value.name='score')
    dt3.score.bln[,bln_score := gsub('s_bln_esd','s_esd',bln_score)]
    dt3.score.bln[,scen := paste0(scen,'_',bln_score,'_hs')]
    dt3.score.bln <- dcast(dt3.score.bln,ID~scen,value.var='score')

    out <- merge(out,dt3.score.bln,by='ID',all.x=TRUE)
  }

  # select all BLN indicator scores per rotation scenario
  check <- max(0,sum(foptim$outputtype %in% c('indicators','all'),na.rm=T))

  if(check > 0){

    cols <- colnames(dt3)[grepl('ID|scen|^i_',colnames(dt3))]
    dt3.indicator.bln <- dt3[,mget(cols)]
    dt3.indicator.bln <- melt(dt3.indicator.bln,id.vars = c('scen','ID'),variable.name='bln_indicator',value.name='score')
    dt3.indicator.bln[,scen := paste0(scen,'_',bln_indicator,'_hs')]
    dt3.indicator.bln <- dcast(dt3.indicator.bln,ID~scen,value.var='score')

    out <- merge(out,dt3.indicator.bln,by='ID',all.x=TRUE)
  }

  # select the most fitted crop rotation plan for soil quality per ESD and aggregated BLN score
  check <- max(0,sum(foptim$outputtype %in% c('rotation'),na.rm=T))

  if(check > 0){

    cols <- colnames(dt3)[grepl('ID|scen|^s_bln',colnames(dt3))]
    dt3.score.bln.blu <- dt3[,mget(cols)]
    dt3.score.bln.blu <- melt(dt3.score.bln.blu,id.vars=c('ID','scen'),value.name = 'value',variable.name = 'esd')

    setorder(dt3.score.bln.blu,ID,-value)
    dt3.score.bln.blu <- dt3.score.bln.blu[,.SD[1],by=c('ID','esd')]
    dt3.score.bln.blu[,esd := paste0(esd,'_blu')]
    dt3.score.bln.blu <- dcast(dt3.score.bln.blu,ID~esd,value.var = 'scen')

    out <- merge(out,dt3.score.bln.blu,by='ID',all.x=TRUE)
  }

  # return output
  return(out)
  }


