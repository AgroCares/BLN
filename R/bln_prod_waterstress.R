#' Calculate and evaluate the yield depression due to drought or wetness in view of the capacity of soils to produce food
#'
#' This function calculates the Water Stress Index (estimating the yield depression as a function of water deficiency or surplus)
#'
#' @param B_HELP_WENR (character) The soil type abbreviation, derived from 1:50.000 soil map
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param WSI (character) The type of Water Stress Index is required. Options: droughtstress, wetnessstress and the (combined) waterstress
#'
#' @references STOWA (2005) Uitbreiding en Actualisering van de HELP-tabellen ten behoeve van het Waternood instrumentarium
#'
#' @import data.table
#' @import OBIC
#'
#' @return
#' The yield depression (in \%) through wetness or drought stress (depending on the WSI selected). Numeric value.
#'
#' @export
bln_p_waterstress <- function(B_HELP_WENR, B_LU_BRP, B_GWL_CLASS, WSI = 'waterstress') {

  # Check input
  arg.length <- max(length(B_HELP_WENR), length(B_LU_BRP), length(B_GWL_CLASS))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(BLN::bln_crops$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_GWL_CLASS,any.missing = FALSE, len = arg.length)
  checkmate::assert_character(B_HELP_WENR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_HELP_WENR, choices = c('unknown',unique(OBIC::waterstress.obic$soilunit)), empty.ok = FALSE)
  checkmate::assert_character(WSI, any.missing = FALSE, min.len = 1, len = 1)
  checkmate::assert_subset(WSI, choices = c('droughtstress','wetnessstress','waterstress'), empty.ok = FALSE)

  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   B_HELP_WENR = B_HELP_WENR,
                   B_LU_BRP = B_LU_BRP,
                   B_GWL_CLASS = B_GWL_CLASS
                  )

  # calculate the waterstress index
  dt[, D_WSI := OBIC::calc_waterstressindex(B_HELP_WENR, B_LU_BRP, B_GWL_CLASS, WSI = WSI)]

  # evaluate the waterstress index
  dt[, value := OBIC::ind_waterstressindex(D_WSI)]

  # extract value
  value <- dt[,value]

  # return value
  return(value)
}

#' Calculate and evaluate the yield depression due to drought in view of the capacity of soils to produce food
#'
#' This function calculates the Water Stress Index (estimating the yield depression as a function of water deficiency or surplus)
#'
#' @param B_HELP_WENR (character) The soil type abbreviation, derived from 1:50.000 soil map
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param WSI (character) The type of Water Stress Index is required. Options: droughtstress, wetnessstress and the (combined) waterstress
#'
#' @references STOWA (2005) Uitbreiding en Actualisering van de HELP-tabellen ten behoeve van het Waternood instrumentarium
#'
#' @import data.table
#' @import OBIC
#'
#' @return
#' The yield depression (in \%) through drought stress (depending on the WSI selected). Numeric value.
#'
#' @export
bln_p_droughtstress <- function(B_HELP_WENR, B_LU_BRP, B_GWL_CLASS, WSI = 'droughtstress') {

  # Check input
  arg.length <- max(length(B_HELP_WENR), length(B_LU_BRP), length(B_GWL_CLASS))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(BLN::bln_crops$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_GWL_CLASS,any.missing = FALSE, len = arg.length)
  checkmate::assert_character(B_HELP_WENR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_HELP_WENR, choices = c('unknown',unique(OBIC::waterstress.obic$soilunit)), empty.ok = FALSE)
  checkmate::assert_character(WSI, any.missing = FALSE, min.len = 1, len = 1)
  checkmate::assert_subset(WSI, choices = c('droughtstress','wetnessstress','waterstress'), empty.ok = FALSE)

  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   B_HELP_WENR = B_HELP_WENR,
                   B_LU_BRP = B_LU_BRP,
                   B_GWL_CLASS = B_GWL_CLASS
  )

  # calculate the waterstress index
  dt[, D_WSI := OBIC::calc_waterstressindex(B_HELP_WENR, B_LU_BRP, B_GWL_CLASS, WSI = WSI)]

  # evaluate the waterstress index
  dt[, value := OBIC::ind_waterstressindex(D_WSI)]

  # extract value
  value <- dt[,value]

  # return value
  return(value)
}

#' Calculate and evaluate the yield depression due to wet conditions in view of the capacity of soils to produce food
#'
#' This function calculates the Water Stress Index (estimating the yield depression as a function of water deficiency or surplus)
#'
#' @param B_HELP_WENR (character) The soil type abbreviation, derived from 1:50.000 soil map
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param WSI (character) The type of Water Stress Index is required. Options: droughtstress, wetnessstress and the (combined) waterstress
#'
#' @references STOWA (2005) Uitbreiding en Actualisering van de HELP-tabellen ten behoeve van het Waternood instrumentarium
#'
#' @import data.table
#' @import OBIC
#'
#' @return
#' The yield depression (in \%) through wetness stress (depending on the WSI selected). Numeric value.
#'
#' @export
bln_p_wetnessstress <- function(B_HELP_WENR, B_LU_BRP, B_GWL_CLASS, WSI = 'wetnessstress') {

  # Check input
  arg.length <- max(length(B_HELP_WENR), length(B_LU_BRP), length(B_GWL_CLASS))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(BLN::bln_crops$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_GWL_CLASS,any.missing = FALSE, len = arg.length)
  checkmate::assert_character(B_HELP_WENR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_HELP_WENR, choices = c('unknown',unique(OBIC::waterstress.obic$soilunit)), empty.ok = FALSE)
  checkmate::assert_character(WSI, any.missing = FALSE, min.len = 1, len = 1)
  checkmate::assert_subset(WSI, choices = c('droughtstress','wetnessstress','waterstress'), empty.ok = FALSE)

  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   B_HELP_WENR = B_HELP_WENR,
                   B_LU_BRP = B_LU_BRP,
                   B_GWL_CLASS = B_GWL_CLASS
  )

  # calculate the waterstress index
  dt[, D_WSI := OBIC::calc_waterstressindex(B_HELP_WENR, B_LU_BRP, B_GWL_CLASS, WSI = WSI)]

  # evaluate the waterstress index
  dt[, value := OBIC::ind_waterstressindex(D_WSI)]

  # extract value
  value <- dt[,value]

  # return value
  return(value)
}
