# make internal table for bulk density

  # load internal packages
  require(data.table)
  require(usethis)

# make a table for all soil properties being used for calculating the OSI

  # loaddata
  bln_parms <- fread('dev/bln_parameters.csv',encoding = 'UTF-8')

  # remove prefix
  setnames(bln_parms,gsub('bln_parm_','',colnames(bln_parms)))

  # Unpack options
  for(this.code in bln_parms[enum == TRUE, code]){
    if(grepl('_HELP$',this.code)){
      bln_parms[code == this.code, choices := list(pandex::enum_opts('B_HELP_WENR'))]
    } else {
      bln_parms[code == this.code, choices := list(pandex::enum_opts(this.code))]
    }

  }
  bln_parms[code == 'B_GWL_CLASS', choices := list(paste0('Gt',pandex::enum_opts("B_GWL_CLASS")))]

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

  # load (for the moment a copy of OBIC crop list)
  bln_crops <- OBIC::crops.obic

  # select only selected categories
  bln_crops <- bln_crops[,.(crop_code,crop_name,crop_cat1 = crop_category)]

  # replace löss with loess
  bln_crops[grepl('löss',crop_name), crop_name := gsub('löss','loess',crop_name)]

  # switch to english categories
  bln_crops[crop_cat1=='akkerbouw', crop_cat1 := 'arable']
  bln_crops[crop_cat1=='mais', crop_cat1 := 'maize']
  bln_crops[crop_cat1=='grasland', crop_cat1 := 'grassland']
  bln_crops[crop_cat1=='natuur', crop_cat1 := 'nature']

  # add country
  bln_crops[,bln_country :='NL']

  # save updated crop table
  usethis::use_data(bln_crops,overwrite = TRUE)

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

