#' Calculate and evaluates the aggregate stability index based on occupation CEC
#'
#' This function calculates and evaluates an aggregate stability index given the CEC and its occupation with major cations.
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param A_SOM_LOI (numeric) The organic matter content of soil in percentage
#' @param A_K_CO_PO (numeric) The occupation of the CEC with K (\%)
#' @param A_CA_CO_PO (numeric) The occupation of the CEC with Ca (\%)
#' @param A_MG_CO_PO (numeric) The occupation of the CEC with Mg (\%)
#'
#' @import data.table
#' @import OBIC
#'
#' @return
#' The aggregate stability index of a soil given the Cation Exchange Capacity and its composition with major cations. A numeric value.
#'
#' @export
bln_p_aggstability <- function(B_SOILTYPE_AGR,A_SOM_LOI,A_K_CO_PO,A_CA_CO_PO,A_MG_CO_PO) {

  # Check inputs
  arg.length <- max(length(B_SOILTYPE_AGR), length(A_SOM_LOI),length(A_K_CO_PO), length(A_CA_CO_PO), length(A_MG_CO_PO))
  checkmate::assert_numeric(A_K_CO_PO, lower = 0, upper = 50, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CA_CO_PO, lower = 0, upper = 400, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_MG_CO_PO, lower = 0, upper = 50, any.missing = FALSE, len = arg.length)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices =  unique(BLN::bln_soiltype$bln_soil_cat1), empty.ok = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)

  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                    A_SOM_LOI = A_SOM_LOI,
                    A_K_CO_PO = A_K_CO_PO,
                    A_CA_CO_PO = A_CA_CO_PO,
                    A_MG_CO_PO = A_MG_CO_PO,
                    value = NA_real_
                  )

  # estimate the aggregate stability (AS)
  dt[, D_AS := OBIC::calc_aggregatestability(B_SOILTYPE_AGR,A_SOM_LOI,A_K_CO_PO,A_CA_CO_PO,A_MG_CO_PO)]

  # calculate and evaluate the AS
  dt[, i_p_as := OBIC::ind_aggregatestability(D_AS)]

  # select output value
  value <- dt[,i_p_as]

  # return
  return(value)
}
