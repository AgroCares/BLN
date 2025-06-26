#' Function to evaluate the carbon saturation via max C saturation potential estimated via ML modelling
#'
#' @param B_LU_BRP (numeric) value of the BRP crop code
#' @param A_SOM_LOI (numeric) value for the soil organic matter content of the soil
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SOM_LOI_MLMAX (numeric) maximum value for the soil organic matter content of the soil estimated with ML model of Ros et al. (2024)
#
#' @import data.table
#'
#' @examples
#' bln_clim_csat(
#' A_SOM_LOI = 7.92,
#' A_CLAY_MI = 3.5,
#' B_LU_BRP = 3732,
#' A_SOM_LOI_MLMAX = 14
#' )
#'
#' @returns A value between 0 and 1 which represents how close to the current
#' soil organic matter content is to carbon saturation. Where 1 is the point of carbon saturation.
#'
#' @export
bln_clim_csat <- function(B_LU_BRP,A_SOM_LOI,A_CLAY_MI,A_SOM_LOI_MLMAX = NA_real_){

  # add visual bindings
  . = crop_code = crop_cat1 = cfcrop = NULL

  # Check inputs
  arg.length <- max(length(B_LU_BRP), length(A_SOM_LOI), length(A_CLAY_MI), length(A_SOM_LOI_MLMAX))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(BLN::bln_crops$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0.1, upper = 100, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI_MLMAX, lower = 0.1, upper = 100, any.missing = TRUE, min.len = 1)
  if(length(A_SOM_LOI_MLMAX)>1){checkmate::assert_numeric(A_SOM_LOI_MLMAX,lower = 0.1, upper = 100,len = arg.length)}

  # make internal table
  dt <- data.table(B_LU_BRP = B_LU_BRP,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SOM_LOI_MLMAX= A_SOM_LOI_MLMAX,
                   value = NA_real_)

  # merge with crop category
  dt <- merge(dt,BLN::bln_crops[,.(crop_code,crop_cat1)],by.x = 'B_LU_BRP',by.y='crop_code',all.x=TRUE)

  # make a very rough estimate via a linear regression model trained on C-saturation data in case that C-max unknown
  dt[crop_cat1 %in% c('arable','maize'), cfcrop := 0.13569913]
  dt[crop_cat1 == 'grassland', cfcrop := 0.01793042]
  dt[crop_cat1 == 'nature', cfcrop := 0.05885480 ]
  dt[is.na(A_SOM_LOI_MLMAX), A_SOM_LOI_MLMAX := 2.6018 + 0.8525 * A_SOM_LOI + 0.003067 * A_SOM_LOI^2 + A_CLAY_MI * cfcrop]

  # calculate the distance to target
  dt[,value := pmin(1,A_SOM_LOI/A_SOM_LOI_MLMAX)]

  # return value for i_clim_csat
  value <- dt[, value]

  # return value
  return(value)

}
