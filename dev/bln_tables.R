# make internal table for bulk density

  # load internal packages
  require(data.table)
  require(usethis)

# make a table for all soil properties being used for calculating the OSI

  # loaddata
  bln_parms <- fread('dev/bln_parameters.csv',encoding = 'UTF-8')
  bln_parms <- pandex::nmi_parameters[code %in% bln_parms$bln_parm_code]

  # Unpack options
  for(this.code in bln_parms[enum == TRUE, code]){
    if(grepl('GWL_CLASS$',this.code)){
      # omit '-' from choices
      bln_parms[code == this.code, choices := list(pandex::enum_opts("B_GWL_CLASS")[!pandex::enum_opts("B_GWL_CLASS") == '-'])]
    } else {
      bln_parms[code == this.code, choices := list(pandex::enum_opts(this.code))]
    }

  }

  # add id
  bln_parms[, id := 1:nrow(bln_parms)]

  # add type
  bln_parms[, type := 'measurement']
  bln_parms[grepl('_BCS$', code), type := 'visual soil assessment']
  bln_parms[grepl('^B_', code), type := 'field property']

  # select columns
  setnames(bln_parms, 'parameter', 'description')
  bln_parms <- bln_parms[,.(id, code , type, description, unit, value_min, value_max, data_type, enum, options, choices)]

  # save updated BLN parameter table
  usethis::use_data(bln_parms,overwrite = TRUE)

# make a table for all soil types available, being country dependent

  # loaddata
  bln_soiltype <- fread('dev/bln_soiltype.csv',encoding = 'UTF-8')

  # save updated crop table
  usethis::use_data(bln_soiltype,overwrite = TRUE)

# make a table for all soil indicator evaluation functions, being country and soil function dependent

  # load data
  bln_thresholds <- fread('dev/bln_thresholds.csv',encoding = 'UTF-8',dec=',')

  # save updated threshold table
  usethis::use_data(bln_thresholds,overwrite = TRUE)

# make crop table

  # load table with crop properties from pandex (private repo)
  bln_crops <- pandex::b_lu_brp[,.(crop_code = B_LU_BRP,
                                   crop_name = B_LU_NAME,
                                   crop_cat1 = B_LU_CULTCAT4,
                                   bln_country ="NL",
                                   B_LU = B_LU,
                                   B_LU_EOM,
                                   B_LU_EOM_RESIDUE,
                                   B_LU_HC,
                                   B_LU_WATERSTRESS_OBIC,
                                   B_LU_MAKKINK
                                   )]

  # replace löss with loess
  bln_crops[grepl('löss',crop_name), crop_name := gsub('löss','loess',crop_name)]

  # switch to english categories
  bln_crops[crop_cat1=='akkerbouw', crop_cat1 := 'arable']
  bln_crops[crop_cat1=='mais', crop_cat1 := 'maize']
  bln_crops[crop_cat1=='grasland', crop_cat1 := 'grassland']
  bln_crops[crop_cat1=='natuur', crop_cat1 := 'nature']

  # save updated crop table
  usethis::use_data(bln_crops,overwrite = TRUE)

# make makkink table

  # load csv file with makkink per crop per month
  dt <- fread('dev/bln_makkink.csv',header = TRUE)

  # colnames for the 12 months
  cols <- colnames(dt)[-1]

  # replace all missing values with the value 0.36 for braak soil
  dt[,c(cols) := lapply(.SD,function(x) fifelse(is.na(x),0.36,x)),.SDcols = cols]

  # write file to data
  bln_makkink <- copy(dt)

  # write file to data
  usethis::use_data(bln_makkink, overwrite = TRUE)

# make LSW table with the averaged properties across whole NL

  # set LSW to averaged values across whole NL
  bln_lsw <- data.table(B_LSW_ID = 'lsw_nlmean', B_SOM_LOI = 8.65,B_CLAY_MI = 15.8,B_SAND_MI = 60.5,
                        B_SILT_MI = 23.71,B_N_RT = 3834,B_P_AL = 49,B_P_CC = 2.71,B_P_WA = 40,B_P_SG = 22,
                        B_FE_OX = 83,B_AL_OX = 40,B_RO_R = 0.5,B_SA_W = 0.47,B_SOM_LOI_SD = 6.67,B_CLAY_MI_SD = 13.45,
                        B_SAND_MI_SD = 23.5,B_SILT_MI_SD = 11.7,B_N_RT_SD = 2928,B_P_AL_SD = 13.5,B_P_CC_SD = 1.51,
                        B_P_WA_SD = 15.6,B_P_SG_SD = 14,B_FE_OX_SD = 59,B_AL_OX_SD = 19,B_RO_R_SD = 0.3,B_SA_W_SD = 0.33)

  # save updated crop table
  usethis::use_data(bln_lsw,overwrite = TRUE)

# make SOMERS table with all information of each base combination

  # read in the somers csv file
  bln_somers <- fread('dev/bln_somers_v2_0.csv',skip=32)

  # overwrite names: basis combinatie, zomer drooglegging, winterdrooglegging
  setnames(bln_somers,
           old =c('basiscombinatie', 'zomerdrooglegging', 'winterdrooglegging'),
           new = c('B_SOMERS_BC','B_DRAIN_SP','B_DRAIN_WP'))

  # dcast the table
  bln_somers <- dcast(bln_somers,B_SOMERS_BC + B_DRAIN_SP + B_DRAIN_WP ~infiltratiemaatregel,
                      value.var = c('mediaan','minimum','maximum'))

  # select only the median values
  bln_somers <- bln_somers[,.(B_SOMERS_BC,B_DRAIN_SP,B_DRAIN_WP,
                              mediaan_AWIS,mediaan_PWIS,mediaan_ref)]

  # clean up names and set to lower case
  setnames(bln_somers,gsub('mediaan_','',colnames(bln_somers)))
  setnames(bln_somers,tolower(colnames(bln_somers)))

  # save updated SOMERS table for use in BLN package
  usethis::use_data(bln_somers,overwrite = TRUE)

# make table with crop rotation scenarios for optimisation land use

  # note that for each B_AER_CBS we assume one soil type as being the most dominant one

  # data source for crop rotation arable systems: https://edepot.wur.nl/463816
  # most common land use Noordelijk kleigebied: pootaardappel, wintertarwe, suikerbiet, wintertarwe
  dt.scen.bld <- data.table(b_aer_cbs = 'LG01', soiltype = 'clay',b_lu_brp = c(2015,233,256,233))
  # most common land use Oldambt: wintertarwe, wintertarwe, aardappel, wintertarwe
  dt.scen.bld <- rbind(dt.scen.bld,data.table(b_aer_cbs = 'LG02', soiltype = 'clay',b_lu_brp = c(233,233,2015,233)))
  # most common land use veenkolonien: zetmeelaardappel, wintertarwe, zetmeelaardappel, suikerbiet
  dt.scen.bld <- rbind(dt.scen.bld,data.table(b_aer_cbs = 'LG02', soiltype= 'sand',b_lu_brp = c(2017,233,2017,256)))
  # most common land use noordelijk zand en dal: consumptieaardappel, zomergerst, suikerbiet, wintertarwe
  dt.scen.bld <- rbind(dt.scen.bld,data.table(b_aer_cbs = 'LG03', soiltype= 'sand',b_lu_brp = c(1910,236,256,233)))
  # most common land use oostelijk zand: consumptieaardappel, zomergerst, suikerbiet, wintertarwe
  dt.scen.bld <- rbind(dt.scen.bld,data.table(b_aer_cbs = 'LG04', soiltype= 'sand',b_lu_brp = c(1910,236,256,233)))
  # most common land use centraal veehouderij: tijdelijk grasland en mais
  dt.scen.bld <- rbind(dt.scen.bld,data.table(b_aer_cbs = 'LG05', soiltype= 'sand',b_lu_brp = c(266,266,266,259)))
  # most common land use IJsselmeerpolders (en deels NH): aardappel - zomergerst - suikberbiet - wintertarwe
  dt.scen.bld <- rbind(dt.scen.bld,data.table(b_aer_cbs = 'LG06', soiltype= 'clay',b_lu_brp = c(1910,236,256,233)))
  dt.scen.bld <- rbind(dt.scen.bld,data.table(b_aer_cbs = 'LG07', soiltype= 'clay',b_lu_brp = c(2015,236,256,233)))
  dt.scen.bld <- rbind(dt.scen.bld,data.table(b_aer_cbs = 'LG08', soiltype= 'clay',b_lu_brp = c(2015,236,256,233)))
  # most common land use Utrechts weidegebied: consumptieaardappel, zomergerst, suikerbiet, wintertarwe
  dt.scen.bld <- rbind(dt.scen.bld,data.table(b_aer_cbs = 'LG09', soiltype= 'clay',b_lu_brp =  c(1910,236,256,233)))
  # most common land use rivierkleigebied: consumptieaardappel, zomergerst, suikerbiet, wintertarwe
  dt.scen.bld <- rbind(dt.scen.bld,data.table(b_aer_cbs = 'LG10', soiltype= 'sand',b_lu_brp = c(1910,236,256,233)))
  # most common land use zuidwestelijk klei / brabant: consumptieaardappel, zaauiui, suikerbiet, wintertarwe
  dt.scen.bld <- rbind(dt.scen.bld,data.table(b_aer_cbs = 'LG11', soiltype= 'clay',b_lu_brp = c(1910,262,256,233)))
  dt.scen.bld <- rbind(dt.scen.bld,data.table(b_aer_cbs = 'LG12', soiltype= 'clay',b_lu_brp = c(1910,262,256,233)))
  # most common land use zuidelijk zand en loss: consumptieaardappel, zomergerst, suikerbiet, wintertarwe
  dt.scen.bld <- rbind(dt.scen.bld,data.table(b_aer_cbs = 'LG13', soiltype= 'sand',b_lu_brp = c(1910,236,256,233)))
  dt.scen.bld <- rbind(dt.scen.bld,data.table(b_aer_cbs = 'LG14', soiltype= 'loess',b_lu_brp = c(1910,236,256,233)))
  dt.scen.bld[, scen := 'bld_arable_int']
  dt.scen.bld[,year := 1:.N,by=c('b_aer_cbs','soiltype')]

  # intensive arable crop rotation with additional winter cereal
  dt.scen.bld.ext <- copy(dt.scen.bld)
  dt.scen.bld.ext[, myear := max(year),by=c('b_aer_cbs','soiltype')]
  dt.scen.bld.ext <- rbind(dt.scen.bld.ext,dt.scen.bld.ext[year==1][,b_lu_brp := 233][,year:=myear+1])
  dt.scen.bld.ext[,scen := 'bld_arable_ext']
  dt.scen.bld.ext[,myear:=NULL]

  # intensive arable crop rotation with additional protein crops (1 x 5 year), soja boon
  dt.scen.bld.prot <- copy(dt.scen.bld)
  dt.scen.bld.prot[, myear := max(year),by=c('b_aer_cbs','soiltype')]
  dt.scen.bld.prot <- rbind(dt.scen.bld.prot,dt.scen.bld.prot[year==1][,b_lu_brp := 665][,year:=myear+1])
  dt.scen.bld.prot[,scen := 'bld_arable_prot']
  dt.scen.bld.prot[,myear:=NULL]

  # vollegrondsgroente
  # noordwest nederland klei: bloemkool, zomertarwe, consumptieaardappel | CA,grasklaver, bloomkool,peen,zomertarwe,bloemkool
  dt.scen.vgg <- CJ(b_aer_cbs = c('LG01','LG03','LG06','LG07','LG08','LG09'),
                    soiltype='clay',scen = 'bld_vegetable_int',b_lu_brp = c(2713, 234, 1910))
  dt.scen.vgg <- rbind(dt.scen.vgg,CJ(b_aer_cbs = c('LG01','LG03','LG06','LG07','LG08'),
                                      soiltype='clay',scen = 'bld_vegetable_ext',b_lu_brp = c(1910,2653,2713,2785,234,2713)))
  # zuid-oost nederland zandgrond: 4 x andijvie, prei | andijvie, grasklaver, prei, venkel, chinese kool, courgette
  dt.scen.vgg <- rbind(dt.scen.vgg,CJ(b_aer_cbs = c('LG02','LG04','LG05','LG13'),
                                      soiltype='sand',scen = 'bld_vegetable_int',b_lu_brp = c(2708,2708,2708,2708,2749)))
  dt.scen.vgg <- rbind(dt.scen.vgg,CJ(b_aer_cbs = c('LG02','LG04','LG05','LG13'),
                                      soiltype='sand',scen = 'bld_vegetable_ext',b_lu_brp = c(2708,2653,2749,2727,2721,2723)))
  # zuid-oost nederland loss: 4 x andijvie, prei | andijvie, grasklaver, prei, venkel, chinese kool, courgette
  dt.scen.vgg <- rbind(dt.scen.vgg,CJ(b_aer_cbs = c('LG14'),
                                      soiltype='loess',scen = 'bld_vegetable_int',b_lu_brp = c(2708,2708,2708,2708,2749)))
  dt.scen.vgg <- rbind(dt.scen.vgg,CJ(b_aer_cbs = c('LG14'),
                                      soiltype='loess',scen = 'bld_vegetable_ext',b_lu_brp = c(2708,2653,2749,2727,2721,2723)))
  # zuid-west nederland klei: CA, ijsbergsla, grasklaver,spruitkool,knolvenkel,zomertarwe
  dt.scen.vgg <- rbind(dt.scen.vgg,CJ(b_aer_cbs = c('LG11','LG10','LG12'),
                                      soiltype='clay',scen = 'bld_vegetable_int',b_lu_brp =  c(1910,2767,2653,2777,2727,234)))
  dt.scen.vgg[,year := 1:.N,by=c('scen','b_aer_cbs','soiltype')]

  # permanent grass
  dt.scen.s1 <- CJ(b_aer_cbs = c('LG01','LG02','LG03','LG04','LG05','LG06','LG07','LG08','LG09','LG10','LG11','LG12','LG13','LG14'),
                   soiltype=NA_character_,scen = 'gld_permanent',b_lu_brp =rep(265,5))
  dt.scen.s1[,year := 1:.N,by=c('b_aer_cbs')]

  # permanent maize
  dt.scen.s2 <- copy(dt.scen.s1)
  dt.scen.s2[, b_lu_brp := 259]
  dt.scen.s2[, scen := 'sms_permanent']

  # intensive cropland rotation
  dt.scen.s3 <- CJ(b_aer_cbs = c('LG01','LG02','LG03','LG04','LG05','LG06','LG07','LG08','LG09','LG10','LG11','LG12','LG13','LG14'),
                   soiltype=NA_character_,scen = 'bld_int',b_lu_brp =c(2017, 256, 2017,236,2017, 1002))
  dt.scen.s3[,year := 1:.N,by=c('b_aer_cbs')]

  # scenario with 3 cereals and 1 potato
  dt.scen.s4 <- CJ(b_aer_cbs = c('LG01','LG02','LG03','LG04','LG05','LG06','LG07','LG08','LG09','LG10','LG11','LG12','LG13','LG14'),
                   soiltype=NA_character_,scen = 'bld_cereals',b_lu_brp =c(233,233,2015,233))
  dt.scen.s4[,year := 1:.N,by=c('b_aer_cbs')]

  # scenario collaboration arable farming and husbandry
  dt.scen.s5 <- CJ(b_aer_cbs = c('LG01','LG02','LG03','LG04','LG05','LG06','LG07','LG08','LG09','LG10','LG11','LG12','LG13','LG14'),
                   soiltype=NA_character_,scen = 'bld_gld_collaboration',b_lu_brp =c(1910, 256, 262, 2785,1910, 256,266,266))
  dt.scen.s5[,year := 1:.N,by=c('b_aer_cbs')]

  # scenario with biodiverse arable farming
  dt.scen.s6 <- CJ(b_aer_cbs = c('LG01','LG02','LG03','LG04','LG05','LG06','LG07','LG08','LG09','LG10','LG11','LG12','LG13','LG14'),
                   soiltype=NA_character_,scen = 'bld_divers',b_lu_brp =c(233,256,262,2015,256,1004))
  dt.scen.s6[,year := 1:.N,by=c('b_aer_cbs')]

  # bln crop rotation scenarios
  bln_scen_croprotation <- rbind(dt.scen.bld,
                                 dt.scen.bld.ext,
                                 dt.scen.bld.prot,
                                 dt.scen.vgg,
                                 dt.scen.s1,
                                 dt.scen.s2,
                                 dt.scen.s3,
                                 dt.scen.s4,
                                 dt.scen.s5,
                                 dt.scen.s6)

  # replace NA soiltype with "all"
  bln_scen_croprotation[is.na(soiltype), soiltype := 'all']

  # save updated SOMERS table for use in BLN package
  usethis::use_data(bln_scen_croprotation,overwrite = TRUE)

# make table with parameter names of input variables

  # make vector with all input for bln_field
  colsp <- c('B_LU_BRP','B_SC_WENR','B_GWL_CLASS','B_SOILTYPE_AGR','B_HELP_WENR','B_AER_CBS','B_GWL_GLG','B_GWL_GHG',
             'B_GWL_ZCRIT','B_DRAIN','B_FERT_NORM_FR','B_SLOPE_DEGREE','B_GWP','B_AREA_DROUGHT',
             'B_CT_PSW','B_CT_NSW','B_CT_PSW_MAX','B_CT_NSW_MAX','B_SOMERS_BC','B_DRAIN_SP','B_DRAIN_WP',
             'A_SOM_LOI','A_SOM_LOI_MLMAX','A_CLAY_MI','A_SAND_MI','A_SILT_MI','A_DENSITY_SA',
             'A_FE_OX','A_AL_OX','A_PH_CC','A_N_RT','A_CN_FR','A_S_RT','A_N_PMN','A_P_AL','A_P_CC','A_P_WA',
             'A_P_SG','A_CEC_CO','A_CA_CO_PO','A_MG_CO_PO','A_K_CO_PO','A_K_CC','A_MG_CC','A_MN_CC',
             'A_ZN_CC','A_CU_CC','A_EW_BCS','A_SC_BCS','A_GS_BCS','A_P_BCS','A_C_BCS','A_RT_BCS','A_RD_BCS','A_SS_BCS','A_CC_BCS',
             'D_SA_W','D_RO_R','M_COMPOST','M_GREEN','M_NONBARE','M_EARLYCROP','M_SLEEPHOSE','M_DRAIN','M_DITCH',
             'M_UNDERSEED','M_LIME','M_NONINVTILL','M_SSPM','M_SOLIDMANURE','M_STRAWRESIDUE','M_MECHWEEDS',
             'M_PESTICIDES_DST')

  # select properties and description from pandex
  dp <- pandex::nmi_parameters
  dp <- dp[code %in% colsp,.(code,parameter,data_type,value_min,value_max)]

  # add missing ones (not yet in pandex)
  dp.missing <- data.table(code = c("B_DRAIN","B_SOMERS_BC","B_DRAIN_SP","B_DRAIN_WP","A_SOM_LOI_MLMAX"),
                           parameter = c('Are drains installed to drain the field',
                                         'The base combination of SOMERS peat soil classification',
                                         'The drooglegging of a field in summer (difference field height and ditch level, in meters)',
                                         'The drooglegging of a field in winter (difference field height and ditch level, in meters)',
                                         'The max. percentage organc matter estimated via machine learning model (/%)'),
                           data_type = c('bool','int','num','num','num'),
                           value_min = c(NA,1,0,0,0),
                           value_max = c(NA,290,3,3,100))
  dp <- rbind(dp,dp.missing)

  # add basic properties
  dp.missing <- data.table(code = c('ID','B_LSW_ID','LSW'),
                           parameter = c('field id, unique identifier for a field',
                                         'the id for the local surface water geometry where the field is located',
                                         'the LSW database with mean properties per Local Surface Water'),
                           data_type = c('char','char','data.table'),
                           value_min = c(NA,NA,NA),
                           value_max = c(NA,NA,NA))
  dp <- rbind(dp,dp.missing)

  # copy as bln_column_description
  bln_input_description <- copy(dp)

  # save updated SOMERS table for use in BLN package
  usethis::use_data(bln_input_description,overwrite = TRUE)

# make object with bln output variables

  cols <- c("i_b_di","i_b_sf","i_c_k","i_c_mg","i_c_n","i_c_p","i_c_ph","i_c_s",
            "i_p_as","i_p_co","i_p_cr","i_p_ds","i_p_du","i_p_ro","i_p_se","i_p_whc","i_p_wo","i_p_ws",
            "i_clim_csat"  ,"i_clim_osb","i_clim_rothc", "i_clim_somers",
            "i_gw_gwr","i_gw_ngw","i_gw_nlea","i_gw_nret","i_gw_pest","i_gw_wb",
            "i_nut_k","i_nut_n","i_nut_nue","i_nut_p",
            "i_sw_nret","i_sw_nro","i_sw_nsw","i_sw_psw",
            's_bln_esd_clim','s_bln_esd_nut', 's_bln_esd_prod', 's_bln_esd_water', 's_bln_prod_b', 's_bln_prod_c','s_bln_prod_p', 's_bln_clim',
            's_bln_gw_quality', 's_bln_gw_quantity', 's_bln_nut', 's_bln_sw_quality', 's_bln_total')

  colsnames <- c('The soil indicator value reflecting distance to target for disease supressiveness in view of crop production',
                'The soil indicator value reflecting distance to target for soil microbial activity in view of crop production',
                'The soil indicator value reflecting distance to target for potassium supply in view of crop production',
                'The soil indicator value reflecting distance to target for magnesium supply in view of crop production',
                'The soil indicator value reflecting distance to target for nitrogen supply in view of crop production',
                'The soil indicator value reflecting distance to target for phosphorus supply in view of crop production',
                'The soil indicator value reflecting distance to target for soil pH in view of crop production',
                'The soil indicator value reflecting distance to target for sulfur supply in view of crop production',
                'The soil indicator value reflecting distance to target for aggregate stability in view of crop production',
                'The soil indicator value reflecting distance to target for subsoil compaction in view of crop production',
                'The soil indicator value reflecting distance to target for crumbleability in view of crop production',
                'The soil indicator value reflecting distance to target for drought stress in view of crop production',
                'The soil indicator value reflecting distance to target for wind erodibility in view of crop production',
                'The soil indicator value reflecting distance to target for rootability in view of crop production',
                'The soil indicator value reflecting distance to target for topsoil sealing in view of crop production',
                'The soil indicator value reflecting distance to target for water retention in view of crop production',
                'The soil indicator value reflecting distance to target for workability in view of crop production',
                'The soil indicator value reflecting distance to target for water stress in view of crop production',
                'The soil indicator value reflecting distance to target for carbon saturation in mineral soils in view carbon sequestration potential using DL model',
                'The soil indicator value reflecting distance to target for carbon soil balance in view carbon sequestration potential',
                'The soil indicator value reflecting distance to target for carbon saturation in mineral soils in view carbon sequestration potential using RothC model',
                'The soil indicator value reflecting distance to target for carbon saturation in peat soils in view carbon sequestration potential using SOMERS model',
                'The soil indicator value reflecting distance to target for OBICs groundwater recharge in view of groundwater regulation and purification',
                'The soil indicator value reflecting distance to target for BBWPs nitrogen leaching risks in view of groundwater regulation and purification',
                'The soil indicator value reflecting distance to target for OBICs nitrogen leaching risks in view of groundwater regulation and purification',
                'The soil indicator value reflecting distance to target for OBICs topsoil nitrogen retention in view of groundwater regulation and purification',
                'The soil indicator value reflecting distance to target for OBICs pesticide leaching in view of groundwater regulation and purification',
                'The soil indicator value reflecting distance to target for BBWPs topsoil water retention in view of groundwater regulation and purification',
                'The soil indicator value reflecting distance to target for potassium use efficiency in view of nutrient recycling and reuse',
                'The soil indicator value reflecting distance to target for nitrogen use efficiency in view of nutrient recycling and reuse',
                'The soil indicator value reflecting distance to target for BBWPs nutrient use efficiency in view of nutrient recycling and reuse',
                'The soil indicator value reflecting distance to target for phosphorus use efficiency in view of nutrient recycling and reuse',
                'The soil indicator value reflecting distance to target for OBICs nitrogen runoff risks in view of surface water regulation and purification',
                'The soil indicator value reflecting distance to target for OBICs nitrogen retention in view of surface water regulation and purification',
                'The soil indicator value reflecting distance to target for BBWPs nitorgen runoff risk in view of surface water regulation and purification',
                'The soil indicator value reflecting distance to target for BBWPs phosphorus runoff risk in view of surface water regulation and purification',
                'The BLN soil quality score for the ecosystem service carbon sequestration and climate regulation',
                'The BLN soil quality score for the ecosystem service nutrient recycling and reuse',
                'The BLN soil quality score for the ecosystem service crop production',
                'The BLN soil quality score for the ecosystem service water regulation and purification',
                'The aggregated soil indicator value for biological soil functions supporting crop production',
                'The aggregated soil indicator value for chemical soil functions supporting crop production',
                'The aggregated soil indicator value for physical soil functions supporting crop production',
                'The aggregated soil indicator value for soil functions supporting carbon sequestration and climate regulation',
                'The aggregated soil indicator value for soil functions supporting groundwater regulation',
                'The aggregated soil indicator value for soil functions supporting groundwater purification',
                'The aggregated soil indicator value for soil functions supporting nutrient recycling and reuse',
                'The aggregated soil indicator value for soil functions supporting surface water purification',
                'The total BLN soil quality score')

  # make output description
  dp <- data.table(code = cols,
                   parameter = colsnames)

  # copy as bln_column_description
  bln_output_description <- copy(dp)

  # save updated SOMERS table for use in BLN package
  usethis::use_data(bln_output_description,overwrite = TRUE)
