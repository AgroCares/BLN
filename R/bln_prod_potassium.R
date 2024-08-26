#' Calculate the K availability index for the Netherlands
#'
#' This function calculates the K availability of a soil, using the agronomic index used in the Netherlands.
#'
#' @param B_LU_BRP (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil (mmol+ / kg), analyzed via Cobalt-hexamine extraction
#' @param A_K_CO_PO (numeric) The occupation of the CEC with potassium (\%)
#' @param A_K_CC (numeric) The plant available potassium, extracted with 0.01M CaCl2 (mg / kg),
#'
#' @import data.table
#'
#' @examples
#' bln_c_potassium(B_LU_BRP = 265, B_SOILTYPE_AGR = 'dekzand',
#' A_SOM_LOI = 4, A_CLAY_MI = 11,A_PH_CC = 5.4, A_CEC_CO = 125,
#' A_K_CO_PO = 8.5, A_K_CC = 145)
#' bln_c_potassium(265, 'dekzand',4, 11,5.4,  125,8.5, 145)
#' bln_c_potassium(c(265,1019), rep('dekzand',2),c(4,6), c(11,14),
#' c(5.4,5.6),  c(125,145),c(8.5,3.5), c(145,180))
#'
#' @return
#' The capacity of the soil to supply and buffer potassium, evaluated given an optimum threshold for yield. A numeric value.
#'
#' @export
bln_c_potassium <- function(B_LU_BRP, B_SOILTYPE_AGR,A_SOM_LOI, A_CLAY_MI,A_PH_CC,
                            A_CEC_CO, A_K_CO_PO, A_K_CC) {

  # add visual bindings
  id = crop_category = soiltype.n = crop_code = soiltype = NULL
  b = cF = kindex1 = kindex2 = A_PH_KCL = A_K_CO = NULL
  bln_country = bln_indicator = crop_cat1 = bln_soil_cat1 = bln_soil_cat2 = value = NULL
  bln_threshold_cropcat = bln_threshold_soilcat = i_c_k = bln_st_c1 = bln_st_c2 = bln_st_c3 = NULL

  # Load in the datasets
  dt.crops <- as.data.table(bln_crops[bln_country=='NL'])
  dt.soils <- as.data.table(bln_soiltype[bln_country=='NL'])

  # Load in the thresholds
  dt.thresholds <- as.data.table(bln_thresholds[bln_country=='NL'& bln_indicator == 'i_c_k'])

  # Check inputs
  arg.length <- max(length(A_PH_CC), length(A_SOM_LOI), length(A_CEC_CO), length(A_K_CO_PO),
                    length(A_K_CC), length(A_CLAY_MI), length(B_SOILTYPE_AGR), length(B_LU_BRP))

  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(dt.soils$bln_soil_cat1), empty.ok = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 10, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_K_CC, lower = 0, upper = 800, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_K_CO_PO, lower = 0.1, upper = 50, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CEC_CO, lower = 1, upper = 1000, any.missing = FALSE, len = arg.length)
  checkmate::assert_data_table(dt.thresholds,max.rows = 6, min.rows = 6)

  # Collect the data
  dt <- data.table(id = 1:arg.length,
                   B_LU_BRP = B_LU_BRP,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC,
                   A_CEC_CO = A_CEC_CO,
                   A_K_CO_PO = A_K_CO_PO,
                   A_K_CC = A_K_CC,
                   value = NA_real_
                   )

  # merge with crop and soil classification tables
  dt <- merge(dt, dt.crops[, list(crop_code, crop_cat1)],
              by.x = "B_LU_BRP", by.y = "crop_code", all.x = TRUE)
  dt <- merge(dt, dt.soils[, list(bln_soil_cat1, bln_soil_cat2)],
              by.x = "B_SOILTYPE_AGR", by.y = "bln_soil_cat1",all.x = TRUE)

  # Calculate the K availability for grassland (CBGV, 2019)
  dt.grass <- dt[crop_cat1 == 'grassland']

  # add K-index where CEC is maximized at 400 mmol+ / kg
  dt.grass[A_CEC_CO > 400, A_CEC_CO := 400]
  dt.grass[,value := 4 - exp(-0.08551 * A_K_CC + 0.5264 * log(A_K_CC) - 0.001607 * A_CEC_CO +
                               0.1275 * log(A_CEC_CO) + 0.010836 * A_K_CC * log(A_CEC_CO))]

  # Calculate the K availability for maize (CBGV, 2019)
  dt.maize <- dt[crop_cat1 == 'maize']
  dt.maize[,value := (1 - (120 - A_K_CC) / 120) * 2.5]

  # Calculate the K availability for arable crops (Ros & Bussink, 2011)
  dt.arable <- dt[crop_cat1 == 'arable']

  # derive b-factor, texture dependent correction
  dt.arable[grepl('duin|rivier|maas|klei',B_SOILTYPE_AGR) & A_SOM_LOI <= 10 & A_CLAY_MI <= 11, b := 1.513]
  dt.arable[grepl('duin|rivier|maas|klei',B_SOILTYPE_AGR) & A_SOM_LOI <= 10 & A_CLAY_MI > 11, b := 0.60226 + 1.27576 /(1 + 0.09891 * A_CLAY_MI)]
  dt.arable[grepl('duin|rivier|maas|klei',B_SOILTYPE_AGR) & A_SOM_LOI > 10 & A_CLAY_MI <= 11, b := 1.513] # SHOULD BE CHANGED; UNKNOWN FROM FACTSHEETS
  dt.arable[grepl('duin|rivier|maas|klei',B_SOILTYPE_AGR) & A_SOM_LOI > 10 & A_CLAY_MI > 11, b := 0.60226 + 1.27576 /(1 + 0.09891 * A_CLAY_MI)] # SHOULD BE CHANGED; UNKNOWN FROM FACTSHEETS
  dt.arable[grepl('zeeklei',B_SOILTYPE_AGR) & A_SOM_LOI > 10 & A_CLAY_MI <= 5, b := 1.513]
  dt.arable[grepl('zeeklei',B_SOILTYPE_AGR) & A_SOM_LOI > 10 & A_CLAY_MI > 5, b := 0.60226 + 1.27576 /(1 + 0.09891 * A_CLAY_MI)]
  dt.arable[grepl('zeeklei',B_SOILTYPE_AGR) & A_SOM_LOI <= 10 & A_CLAY_MI <= 5, b := 1.513]  # SHOULD BE CHANGED; UNKNOWN FROM FACTSHEETS
  dt.arable[grepl('zeeklei',B_SOILTYPE_AGR) & A_SOM_LOI <= 10 & A_CLAY_MI > 5, b := 0.60226 + 1.27576 /(1 + 0.09891 * A_CLAY_MI)]  # SHOULD BE CHANGED; UNKNOWN FROM FACTSHEETS
  dt.arable[grepl('loess',B_SOILTYPE_AGR) & A_CLAY_MI <= 11, b := 1.513]
  dt.arable[grepl('loess',B_SOILTYPE_AGR) & A_CLAY_MI > 11, b := 1.75 - 0.04 * 2 * A_CLAY_MI + 0.00068 * (2 * A_CLAY_MI)^2 - 0.0000041 * (2 * A_CLAY_MI)^3]

  # pH-KCl needed (not higher than pH is 7)
  dt.arable[,A_PH_KCL := pmin(7,(A_PH_CC - 0.5262)/0.9288)]

  # correction factor for texture and OS (the so called F-factor)
  dt.arable[grepl('zand|dal|veen',B_SOILTYPE_AGR), cF := 20 / (10 + A_SOM_LOI)]
  dt.arable[grepl('duin|rivier|maas|klei|loess',B_SOILTYPE_AGR) & A_SOM_LOI <= 10, cF := b /(0.15 * A_PH_KCL-0.05)]
  dt.arable[grepl('duin|rivier|maas|klei|loess',B_SOILTYPE_AGR) & A_SOM_LOI > 10, cF := b /(0.15 * A_PH_KCL-0.05)] # SHOULD BE CHANGED; UNKNOWN FROM FACTSHEETS
  dt.arable[grepl('zeeklei',B_SOILTYPE_AGR) & A_SOM_LOI > 10, cF := b]
  dt.arable[grepl('zeeklei',B_SOILTYPE_AGR) & A_SOM_LOI <= 10, cF := b] # SHOULD BE CHANGED; UNKNOWN FROM FACTSHEETS

  # calculate K-COHEX as mg K per kg soil
  dt.arable[,A_K_CO := A_K_CO_PO * A_CEC_CO * 0.01 * 39.098]

  # calculate K-index based on CaCl2-extracble K as wel ass K-CEC
  dt.arable[grepl('zand|dal|veen',B_SOILTYPE_AGR),kindex1 := A_K_CC * cF * 0.12046]
  dt.arable[grepl('zand|dal|veen',B_SOILTYPE_AGR),kindex2 := (A_K_CO - 0.17 * A_CEC_CO) * cF * 0.12046]
  dt.arable[grepl('klei',B_SOILTYPE_AGR),kindex1 := (1.56 * A_K_CC - 17 + 0.29 * A_CEC_CO) * cF * 0.12046]
  dt.arable[grepl('klei',B_SOILTYPE_AGR),kindex2 := A_K_CO * cF * 0.12046]

  # calculate K-HCL (mg K2O/ 100 g) for loess, assuming Cohex similar to HCl extraction
  dt.arable[grepl('loess',B_SOILTYPE_AGR), c('kindex1','kindex2') := A_K_CO * 1.2047 * 0.1]

  # add check for K-CEC derived Kindex: should not below zero
  dt.arable[kindex2 < 0, kindex2 := kindex1]
  dt.arable[kindex1 < 0, kindex1 := kindex2]
  dt.arable[,value := 0.5 * (kindex1 + kindex2)]

  # replace negative values by zero
  dt.arable[value < 0, value := 0]

  # Calculate the K availability for nature
  dt.nature <- dt[crop_cat1 == 'nature']
  dt.nature[,value := 0]

  # score the K index given threshold for agronomic production / product quality

    # subset
    dths <- dt.thresholds[bln_threshold_cropcat == 'grassland']

    # evaluate grassland
    dt.grass[, i_c_k := bln_evaluate_logistic(value, b = dths[,bln_st_c1], x0 = dths[,bln_st_c2],v = dths[,bln_st_c3])]

    # subset and evaluate for maize
    dths <- dt.thresholds[bln_threshold_cropcat == 'maize']
    dt.grass[, i_c_k := bln_evaluate_logistic(value, b = dths[,bln_st_c1], x0 = dths[,bln_st_c2],v = dths[,bln_st_c3])]

    # subset and evaluate for arable sandy soils
    dths <- dt.thresholds[bln_threshold_cropcat == 'arable' & bln_threshold_soilcat == 'sand']
    dt.arable[grepl('zand|dal',B_SOILTYPE_AGR), i_c_k := bln_evaluate_logistic(value, b = dths[,bln_st_c1], x0 = dths[,bln_st_c2],v = dths[,bln_st_c3])]

    # subset and evaluate for arable peat soils
    dths <- dt.thresholds[bln_threshold_cropcat == 'arable' & bln_threshold_soilcat == 'peat']
    dt.arable[grepl('peat',B_SOILTYPE_AGR), i_c_k := bln_evaluate_logistic(value, b = dths[,bln_st_c1], x0 = dths[,bln_st_c2],v = dths[,bln_st_c3])]

    # subset and evaluate for arable clay soils
    dths <- dt.thresholds[bln_threshold_cropcat == 'arable' & bln_threshold_soilcat == 'clay']
    dt.arable[grepl('klei',B_SOILTYPE_AGR), i_c_k := bln_evaluate_logistic(value, b = dths[,bln_st_c1], x0 = dths[,bln_st_c2],v = dths[,bln_st_c3])]

    # subset and evaluate for arable loess soils
    dths <- dt.thresholds[bln_threshold_cropcat == 'arable' & bln_threshold_soilcat == 'loess']
    dt.arable[grepl('loess',B_SOILTYPE_AGR), i_c_k := bln_evaluate_logistic(value, b = dths[,bln_st_c1], x0 = dths[,bln_st_c2],v = dths[,bln_st_c3])]

    # evaluate nature soils
    dt.nature[, i_c_k := 1]

  # Combine both tables and extract values
  dt <- rbindlist(list(dt.arable,dt.grass, dt.maize,dt.nature), fill = TRUE)
  setorder(dt, id)

  # select the output variable
  out <- dt[,i_c_k]

  # return the OSI score
  return(out)
}
