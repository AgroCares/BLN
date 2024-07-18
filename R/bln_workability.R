#' Calculate and evaluate the soil health for workability
#'
#' This function calculates the workability of soils, given as a value of relative season length between 0 and 1.
#' A relative season length of 1 indicates that the water table is sufficiently low for the soil to be workable for the entire growing season required by the crop.
#' The required ground water table for workability is determined by soil type and soil properties. Hydrological variables determine the groundwater table for each day of the year.
#' The option calcyieldloss allows for calculation of yield loss based on the relative season length, differentiating in yield loss between six groups of crops
#' Based on Huinink (2018)
#'
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param B_GWL_GLG (numeric) The lowest groundwater level averaged over the most dry periods in 8 years in cm below ground level
#' @param B_GWL_GHG (numeric) The highest groundwater level averaged over the most wet periods in 8 years in cm below ground level
#' @param B_GWL_ZCRIT  (numeric) The distance between ground level and groundwater level at which the groundwater can supply the soil surface with 2mm water per day (in cm)
#' @param calcyieldloss (boolean) whether the function includes yield loss, options: TRUE or FALSE (default).
#'
#' @import data.table
#'
#' @references Huinink (2018) Bodem/perceel geschiktheidsbeoordeling voor Landbouw, Bosbouw en Recreatie. BodemConsult-Arnhem
#'
#' @examples
#' bln_p_workability(A_CLAY_MI = 18,A_SILT_MI = 25,B_LU_BRP = 265,
#' B_SOILTYPE_AGR = 'dekzand',B_GWL_GLG = 145,B_GWL_GHG = 85,B_GWL_ZCRIT = 400,
#' calcyieldloss = FALSE)
#' bln_p_workability(18,25,265,'dekzand',145,85,400,FALSE)
#'
#' @return
#' The workability of a soil, expressed as a numeric value representing the relative season length that the soil can be managed by agricultural activities.
#'
#' @export
bln_p_workability <- function(A_CLAY_MI, A_SILT_MI, B_LU_BRP, B_SOILTYPE_AGR,
                             B_GWL_GLG, B_GWL_GHG, B_GWL_ZCRIT,
                             calcyieldloss = FALSE) {

  # Check inputs
  arg.length <- max(length(A_CLAY_MI), length(A_SILT_MI), length(B_LU_BRP), length(B_SOILTYPE_AGR),
                    length(B_GWL_GLG), length(B_GWL_GHG), length(B_GWL_ZCRIT))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(BLN::bln_crops$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(BLN::bln_soiltype$soiltype), empty.ok = FALSE)
  checkmate::assert_numeric(B_GWL_GLG, lower = 0, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_GWL_GHG, lower = 0, any.missing = FALSE, len = arg.length)
  checkmate::assert_true(all(B_GWL_GHG < B_GWL_GLG))
  checkmate::assert_numeric(B_GWL_ZCRIT, lower = 0, upper = 400, any.missing = FALSE, len = arg.length)

  # Collect in data table
  dt <- data.table(id = 1:arg.length,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   B_LU_BRP = B_LU_BRP,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_GWL_GLG = B_GWL_GLG,
                   B_GWL_GHG = B_GWL_GHG,
                   B_GWL_ZCRIT = B_GWL_ZCRIT)

  # estimate the workability
  dt[, D_WO := OBIC::calc_workability(A_CLAY_MI, A_SILT_MI, B_LU_BRP,
                                      B_SOILTYPE_AGR, B_GWL_GLG, B_GWL_GHG, B_GWL_ZCRIT)]

  # evaluate the workability as BLN indicator
  dt[, i_p_wo := ind_workability(D_WO, B_LU_BRP)]

  # extract the BLN indicator
  value <- dt[, i_p_wo]

  # return value
  return(value)
}

#' Calculate indicator for workability
#'
#' This function calculates the indicator for the workability of the soil expressed as the period in which the soil can be worked without
#' inflicting structural damage that cannot be restored by the regular management on the farm.
#'
#' @param D_WO (numeric) The value of the relative (workable) season length calculated by \code{\link{calc_workability}}
#' @param B_LU_BRP (numeric) The crop code from the BRP
#'
#' @examples
#' ind_workability(D_WO = 0.85,B_LU_BRP = 256)
#' ind_workability(D_WO = c(0.15,0.6,0.9),B_LU_BRP = c(256,1019,1019))
#'
#' @return
#' The evaluated score for the soil function to allow the soil to be managed by agricultural activities. A numeric value between 0 and 1.
#'
#' @export
ind_workability <- function(D_WO, B_LU_BRP) {

  # add visual bindings
  id = arg.length = crop_code = crop_season = rsl = . = NULL

  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)

  # length of inputs
  arg.length <- max(length(D_WO), length(B_LU_BRP))

  # Check inputs
  checkmate::assert_numeric(D_WO, lower = 0, upper = 1, any.missing = FALSE)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)

  # Form data table
  dt <- data.table(id = 1:arg.length,
                   rsl = D_WO,
                   B_LU_BRP = B_LU_BRP)

  # Merge crop_season into data table
  dt <- merge.data.table(dt, crops.obic[,.(crop_code, crop_season)], by.x = 'B_LU_BRP', by.y = 'crop_code')

  # evaluate relative season length
  dt[!grepl('^beweid', crop_season),score := evaluate_logistic(x = rsl, b = 15, x0 = 0.75, v = 1, increasing = TRUE)]
  dt[grepl('^beweid', crop_season),score := evaluate_logistic(x = rsl, b = 9, x0 = 0.5, v = 1, increasing = TRUE)]

  # overwrite score when rsl = 1
  dt[rsl == 1, score := 1]

  # setorder
  setorder(dt, id)

  # Return score
  score <- dt[,score]

  return(score)
}
