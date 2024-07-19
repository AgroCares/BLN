#' Function to evaluate the carbon saturation via max C saturation potential estimated via ML modelling
#'
#' @param B_LU_BRP (numeric) value of the BRP crop code
#' @param A_SOM_LOI (numeric) value for the soil organic matter content of the soil
#' @param A_SOM_LOI_MLMAX (numeric) maximum value for the soil organic matter content of the soil estimated with ML model of Ros et al. (2024)
#
#' @import data.table
#'
#' @export
bln_clim_csat <- function(B_LU_BRP,A_SOM_LOI,A_SOM_LOI_MLMAX = NA_real_){

  # make internal table
  dt <- data.table(B_LU_BRP = B_LU_BRP,
                   A_SOM_LOI = A_SOM_LOI,
                   A_SOM_LOI_MLMAX= A_SOM_LOI_MLMAX
  )

  # merge with crop category
  dt <- merge(dt,bln_crops[,.(crop_code,crop_cat1)],by.x = 'B_LU_BRP',by.y='crop_code',all.x=TRUE)

  # make a very rough estimate via a linear regression model trained on C-saturation data in case that C-max unknown
  dt[crop_cat1 %in% c('arable','maize'), cfcrop := 0.13569913]
  dt[crop_cat1 == 'grassland', cfcrop := 0.01793042]
  dt[crop_cat1 == 'nature', cfcrop := 0.05885480 ]
  dt[is.na(A_SOM_LOI_MLMAX), A_SOM_LOI_MLMAX := 2.6018 + 0.8525 * A_SOM_LOI + 0.003067 * A_SOM_LOI^2 + A_CLAY_MI * cfcrop]

  # calculate the distance to target
  dt.cs[,i_clim_csat := pmin(1,A_SOM_LOI/A_SOM_LOI_MLMAX)]

  # return value
  value <- dt[, i_clim_csat]

  return(value)

}
