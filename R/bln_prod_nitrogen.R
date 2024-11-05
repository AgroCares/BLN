#' Calculate the soil nitrogen supplying capacity in the Netherlands
#'
#' This function calculates the NLV (nitrogen producing capacity) for the soil
#'
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' @param A_CN_FR (numeric) The carbon to nitrogen ratio (-)
#'
#' @import data.table
#'
#' @examples
#' bln_c_nitrogen(B_LU_BRP = 256, B_SOILTYPE_AGR = 'dekzand',A_SOM_LOI = 4.5,A_N_RT = 2500)
#' bln_c_nitrogen(1019,'dekzand',5.5,2315)
#'
#' @return
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#'
#' @export
bln_c_nitrogen <- function(B_LU_BRP, B_SOILTYPE_AGR,A_SOM_LOI,A_N_RT,A_CN_FR = NA_real_) {

  # add visual bindings
  bln_country = bln_indicator = crop_code = crop_cat1 = bln_threshold_cropcat = NULL
  D_BDS = D_RD = D_OC = D_GA = id = value = i_c_n = bln_st_c1 = code = value_min = value_max = NULL

  # Load in the crops data set and the parms dataset
  dt.crops <- as.data.table(BLN::bln_crops)
  dt.parms <- as.data.table(BLN::bln_parms)
  dt.thresholds <- as.data.table(BLN::bln_thresholds)
  blnp <- BLN::bln_parms

  # subset thresholds to Dutch situation for phosphorus
  dt.thresholds <- dt.thresholds[bln_country=='NL' & bln_indicator=='i_c_n']

  # check length and of arguments
  arg.length <- max(length(A_N_RT), length(A_SOM_LOI),length(B_LU_BRP), length(B_SOILTYPE_AGR),length(A_CN_FR))
  checkmate::assert_numeric(A_N_RT, lower = 0.1, upper = 30000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(BLN::bln_soiltype$bln_soil_cat1), empty.ok = FALSE)
  checkmate::assert_data_table(dt.thresholds,max.rows = 2,min.rows = 2)

  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU_BRP = B_LU_BRP,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   A_CN_FR = A_CN_FR,
                   value = NA_real_
                  )

  # add CN ratio and run checks
  dt[is.na(A_CN_FR), A_CN_FR := A_SOM_LOI *10 * 0.5 *1000/ A_N_RT]
  checkmate::assert_numeric(dt$A_CN_FR, lower = blnp[code == "A_CN_FR", value_min], upper = blnp[code == "A_CN_FR", value_max])

  # merge with crop_category
  dt <- merge(dt, dt.crops[, list(crop_code, crop_cat1)],
              by.x = "B_LU_BRP", by.y = "crop_code", all.x = TRUE)

  # calculate derivative supporting soil properties
  dt[, D_BDS := OBIC::calc_bulk_density(B_SOILTYPE_AGR,A_SOM_LOI)]
  dt[, D_RD := OBIC::calc_root_depth(B_LU_BRP = B_LU_BRP)]
  dt[, D_OC := OBIC::calc_organic_carbon(A_SOM_LOI, D_BDS, D_RD)]
  dt[, D_GA := OBIC::calc_grass_age(id, B_LU_BRP = B_LU_BRP)]

  # calculate the N supplying capacity for the Netherlands using the Dutch OBIC
  dt[,value := OBIC::calc_nlv(B_LU_BRP = B_LU_BRP, B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                          A_N_RT = A_N_RT, A_CN_FR = A_CN_FR, D_OC = D_OC, D_BDS = D_BDS,
                          D_GA = D_GA)]

  # convert to OSI score

    # subset and evaluate for arable soils
    dths <- dt.thresholds[bln_threshold_cropcat == 'arable']
    dt[grepl('arable|maize',crop_cat1), i_c_n := bln_evaluate_parabolic(value, x.top = dths[,bln_st_c1])]

    # subset and evaluate for grassland soils
    dths <- dt.thresholds[bln_threshold_cropcat == 'grassland']
    dt[grepl('grassland',crop_cat1), i_c_n := bln_evaluate_parabolic(value, x.top = dths[,bln_st_c1])]

    # set OSI score for others
    dt[grepl('nature',crop_cat1), i_c_n := 1]

  # select output variable
  out <- dt[,i_c_n]

  # return value
  return(out)
}
