#' Determine groundwater table class based on highest and lowest mean groundwater levels
#'
#' @param B_GWL_GHG (numeric) The average highest groundwater level in cm below field level (Gemiddeld Hoogste Grondwaterstand)
#' @param B_GWL_GLG (numeric) The average lowest groundwater level in cm below field level (Gemiddeld Laagste Grondwaterstand)
#'
#' @import data.table
#'
#' @examples
#' bln_format_gtclass(B_GWL_GHG = 35, B_GWL_GLG = 55)
#' bln_format_gtclass(B_GWL_GHG = 70, B_GWL_GLG = 155)
#'
#' @return
#' A standardized B_GWL_CLASS value as required for BLN functions. A character string.
#'
#' @export
bln_format_gtclass <- function(B_GWL_GHG, B_GWL_GLG) {

  # Add visual bindings
  B_GWL_CLASS = NULL

  # Check arguments
  arg.length <- max(length(B_GWL_GLG), length(B_GWL_GHG))
  checkmate::assert_numeric(B_GWL_GLG, lower = 0, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_GWL_GHG, lower = 0, any.missing = FALSE, len = arg.length)
  checkmate::assert_true(all(B_GWL_GHG < B_GWL_GLG))

  # Make internal table
  dt = data.table(B_GWL_GHG = B_GWL_GHG, B_GWL_GLG = B_GWL_GLG)

  # Classification is based on the article:
  # Knotters, M., Walvoort., D., Brouwer, .F, Stuyt, L., Okx, J. (2018). Landsdekkende, actuele informatie
  # over grondwatertrappen digitaal beschikbaar. KNW, Den Haag.

  # GW classes range from I to VIII, with 'a' and 'b' as a sub categorization.

  dt[, B_GWL_CLASS := '-']

  dt[B_GWL_GHG < 40 & B_GWL_GLG < 50, B_GWL_CLASS := 'I']

  dt[B_GWL_GHG < 25 & B_GWL_GLG >= 50 & B_GWL_GLG < 80, B_GWL_CLASS := 'II']
  dt[B_GWL_GHG >= 25 & B_GWL_GHG < 40 & B_GWL_GLG >= 50 & B_GWL_GLG < 80, B_GWL_CLASS := 'IIb']

  dt[B_GWL_GHG < 40 & B_GWL_GLG >= 80 & B_GWL_GLG < 120, B_GWL_CLASS := 'III']
    dt[B_GWL_GHG < 25 & B_GWL_GLG >= 80 & B_GWL_GLG < 120, B_GWL_CLASS := 'IIIa']
    dt[B_GWL_GHG >= 25 & B_GWL_GHG < 40 & B_GWL_GLG >= 80 & B_GWL_GLG < 120, B_GWL_CLASS := 'IIIb']

  dt[B_GWL_GHG >= 40 & B_GWL_GLG >= 80 & B_GWL_GLG < 120, B_GWL_CLASS := 'IV']

  dt[B_GWL_GHG < 40 & B_GWL_GLG >= 120, B_GWL_CLASS := 'V']
    dt[B_GWL_GHG < 25 & B_GWL_GLG >= 120, B_GWL_CLASS := 'Va']
    dt[B_GWL_GHG >= 25 & B_GWL_GHG < 40 & B_GWL_GLG >= 120, B_GWL_CLASS := 'Vb']

  dt[B_GWL_GHG >= 40 & B_GWL_GHG < 80 & B_GWL_GLG >= 120, B_GWL_CLASS := 'VI']

  dt[B_GWL_GHG >= 80 & B_GWL_GHG < 140 & B_GWL_GLG >= 120, B_GWL_CLASS := 'VII']

  dt[B_GWL_GHG >= 140 & B_GWL_GLG >= 120, B_GWL_CLASS := 'VIII']

  value = dt[, B_GWL_CLASS]
  return(value)

}
