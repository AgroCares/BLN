#' Function to calculate and evaluate the phosphorus use efficiency in view of the soils' function to improve nutrient recycling
#'
#' @param B_LU_BRP (numeric) The crop code
#' @param A_P_AL (numeric) The P-content of the soil extracted with ammonium lactate
#' @param A_P_CC (numeric) The P-content of the soil extracted with CaCl2
#' @param A_P_WA (numeric) The P-content of the soil extracted with water
#'
#' @import data.table
#'
#' @examples
#' bln_nut_phosphorus(B_LU_BRP = 265, A_P_AL = 45, A_P_CC = 2.5)
#' bln_nut_phosphorus(B_LU_BRP = c(265,1019),A_P_AL = c(35,54),A_P_CC = c(2.5,4.5), A_P_WA = c(35,65))
#'
#' @export
bln_nut_phosphorus <- function(B_LU_BRP, A_P_AL = NA_real_, A_P_CC = NA_real_, A_P_WA = NA_real_) {

  # set visual bindings
  i_c_p = bln_country = bln_indicator = id = crop_cat1 = NULL

  # Load in the crops data set and the parms dataset
  dt.crops <- as.data.table(BLN::bln_crops)
  dt.parms <- as.data.table(BLN::bln_parms)
  dt.thresholds <- as.data.table(BLN::bln_thresholds)

  # subset thresholds to Dutch situation for phosphorus
  dt.thresholds <- dt.thresholds[bln_country=='NL' & bln_indicator=='i_c_p']

  # Check length of desired input
  arg.length <- max(length(B_LU_BRP),length(A_P_AL),length(A_P_CC),length(A_P_WA))

  # check the values (update the limits later via dt.parms)
  checkmate::assert_numeric(A_P_AL, lower = 1, upper = 250, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(A_P_CC, lower = 0.1, upper = 100, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(A_P_WA, lower = 1, upper = 250, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(dt.crops$crop_code), empty.ok = FALSE)

  # check that there is only 1 scoring function for P
  checkmate::assert_data_table(dt.thresholds,max.rows = 1)

  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   A_P_AL = A_P_AL,
                   A_P_CC = A_P_CC,
                   A_P_WA = A_P_WA,
                   B_LU_BRP = B_LU_BRP,
                   value = NA_real_
  )

  dt <- merge(dt,dt.crops,by.x = 'B_LU_BRP', by.y = 'crop_code',all.x=TRUE)

  # set the order to the original inputs
  setorder(dt, id)

  # Calculate the phosphate availability for grass (PBI)
  dt[grepl("gras",crop_cat1), value := pmax(0,log(A_P_CC) * (-0.0114 * A_P_AL + 2.5) + 0.0251 * A_P_CC + 2)]

  # Calculate the phosphate availability for maize (PBI)
  dt[grepl("maize",crop_cat1), value := A_P_CC + 0.05 * (A_P_AL / A_P_CC)]

  # calculate the P-availability for arable systems, normalized to a scale with maximum around 6
  dt[grepl("arable",crop_cat1), value := A_P_WA * 0.1]

  # calculate the P-availability for nature
  dt[grepl("nature",crop_cat1), value := 0]

  # convert to the bln score
  dt[, i_nut_p := bln_evaluate_logistic_gaus_down(x = value, b = 1.3, x0 = 1.3, v = 0.35, optimum = 5, optimum_ofset = 2)]

  # return value
  value <- dt[, i_nut_p]

  return(value)

}
