# make internal table for bulk density

  # load internal packages
  require(data.table)
  require(usethis)

# make a table for all soil properties being used for calculating the OSI

  # loaddata
  bln_parms <- fread('dev/bln_parameters.csv',encoding = 'UTF-8')

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

