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


