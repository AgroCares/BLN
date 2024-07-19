#' Calculate and evaluate the indicator for wind erodibility in view of food production
#'
#' This function calculates and evaluates the risk for wind erodibility of soils, derived from Van Kerckhoven et al. (2009) and Ros & Bussink (2013)
#'
#' @param B_LU_BRP (numeric) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#'
#' @import OBIC
#'
#' @return
#' The vulnerability of the soil for wind erosion. A numeric value.
#'
#' @export
bln_p_windererosion <- function(B_LU_BRP,A_CLAY_MI,A_SILT_MI) {

  # add visual bindings
  id = crop_code = crop_cat1 = loam = NULL

  # Load in the datasets
  dt.crops <- as.data.table(BLN::bln_crops)

  # Check inputs
  arg.length <- max(length(A_CLAY_MI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(dt.crops$crop_code), empty.ok = FALSE)

  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   B_LU_BRP = B_LU_BRP,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_
                  )

  # add crop names
  dt <- merge(dt, dt.crops[, list(crop_code, crop_cat1)], by.x = "B_LU_BRP", by.y = "crop_code",all.x = TRUE)

  # calculate silt + clay = loam content
  dt[,loam := A_CLAY_MI + A_SILT_MI]

  # WEF function applicable over range 3-100%
  dt[loam <= 3,loam := 3]

  # Evaluate the wind erodibility factor (WEF)
  dt[,value := -0.286 * log(loam) + 1.3264]

  # set WEF on zero for all non-arable crops
  dt[crop_cat1 != "arable",value := 0]

  # restrict values between 0 and 1
  dt[, value := pmax(pmin(value, 1), 0)]

  # Evaluate the wind erodibility factor (WEF)
  dt[,value := 1 - value]

  # return Wind Erodibility Factor
  setorder(dt, id)
  value <- dt[, value]

  # return
  return(value)

}
