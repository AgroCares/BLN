#' Calculate the phosphate availability index in the Netherlands
#'
#' This function calculates the phosphate availability.
#'
#' @param B_LU (numeric) The crop code
#' @param A_P_AL (numeric) The P-content of the soil extracted with ammonium lactate
#' @param A_P_CC (numeric) The P-content of the soil extracted with CaCl2
#' @param A_P_WA (numeric) The P-content of the soil extracted with water
#'
#' @import data.table
#'
#' @examples
#' bln_c_posphor(B_LU = 265, A_P_AL = 45, A_P_CC = 2.5)
#' bln_c_posphor(B_LU = c(265,1019),A_P_AL = c(35,54),A_P_CC = c(2.5,4.5), A_P_WA = c(35,65))
#'
#' @details
#' The category a cultivation supplied by B_LU belongs to determines which P
#' extraction parameter are required. Not supplying the correct P parameter for
#' a cultivation results in the function returning NA. The cultivation categories
#' can be found in table bln_crops, column crop_cat1. Column crop_code in this
#' table should correspond to the B_LU input of bln_c_posphor.
#'
#' There are four cultivation categories: grass, maize, arable, and nature. Grass
#' and maize classified cultivations require the parameters A_P_AL and A_P_CC,
#' arable cultivations require A_P_WA, and nature does not require any P parameter.
#' The phosphate availability index for nature defaults to 0.
#'
#' @return
#' The phosphate availability index in the Netherlands estimated from extractable soil P fractions. A numeric value.
#'
#' @export
bln_c_posphor <- function(B_LU, A_P_AL = NA_real_, A_P_CC = NA_real_, A_P_WA = NA_real_) {

  # set visual bindings
  i_c_p = bln_country = bln_indicator = id = crop_cat1 = NULL

  # Load in the crops data set and the parms dataset
  dt.crops <- as.data.table(BLN::bln_crops)
  dt.parms <- as.data.table(BLN::bln_parms)
  dt.thresholds <- as.data.table(BLN::bln_thresholds)

  # subset thresholds to Dutch situation for phosphorus
  dt.thresholds <- dt.thresholds[bln_country=='NL' & bln_indicator=='i_c_p']

  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_P_AL),length(A_P_CC),length(A_P_WA))

  # check the values (update the limits later via dt.parms)
  checkmate::assert_numeric(A_P_AL, lower = 1, upper = 250, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(A_P_CC, lower = 0.1, upper = 100, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(A_P_WA, lower = 1, upper = 250, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)

  # check that there is only 1 scoring function for P
  checkmate::assert_data_table(dt.thresholds,max.rows = 1)

  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   A_P_AL = A_P_AL,
                   A_P_CC = A_P_CC,
                   A_P_WA = A_P_WA,
                   B_LU = B_LU,
                   value = NA_real_
                  )

  dt <- merge(dt,dt.crops,by.x = 'B_LU', by.y = 'crop_code',all.x=TRUE)

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
  dt[,i_c_p := bln_evaluate_logistic(x = value, b= dt.thresholds$bln_st_c1,x0 = dt.thresholds$bln_st_c2,v = dt.thresholds$bln_st_c3)]

  # return value
  value <- dt[, i_c_p]

  return(value)

}

