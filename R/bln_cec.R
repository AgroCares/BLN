#' Calculate and evaluate the soil fertility based on the CEC
#'
#' This function calculates the capacity of the soil to buffer cations
#'
#' @param A_CEC_CO (numeric) The cation exchange capacity (mmol+ / kg)
#'
#' @import data.table
#'
#' @return
#' The capacity of the soil to buffer cations. A numeric value.
#'
#' @export
bln_c_cec <- function(A_CEC_CO) {

  # Check inputs
  arg.length <- max(length(A_CEC_CO))
  checkmate::assert_numeric(A_CEC_CO, lower = 0, upper = 1000, any.missing = FALSE, len = arg.length)

  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   A_CEC_CO = A_CEC_CO
                  )

  # Evaluate the CEC for agricultural production, given impact soil cation buffer capacity (Goselink & Van Erp, 1999)
  dt[,i_c_cec := pmin(A_CEC_CO * 0.01, 1)]

  # select output value
  value <- dt[,i_c_cec]

  # return
  return(value)
}
