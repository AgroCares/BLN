#' Function to calculate and evaluate the groundwater recharche in view of the soils' function to retain water
#'
#' @param B_LU_BRP (numeric) The crop code
#' @param B_DRAIN (boolean) Are drains installed to drain the field (options: yes or no)
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_SC_WENR (character) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#'
#' @import data.table
#' @import OBIC
#'
#' @export
bln_wat_groundwater_recharge <- function(B_LU_BRP,B_SC_WENR,B_GWL_CLASS,B_DRAIN,
                                         A_CLAY_MI,A_SAND_MI, A_SILT_MI, A_SOM_LOI,M_GREEN){

  # make internal table
  dt <- data.table(id = 1:length(B_LU_BRP),
                   B_LU_BRP = B_LU_BRP,
                   B_SC_WENR=as.character(B_SC_WENR),
                   B_GWL_CLASS=B_GWL_CLASS,
                   B_DRAIN=B_DRAIN,
                   A_CLAY_MI=A_CLAY_MI,
                   A_SAND_MI=A_SAND_MI,
                   A_SILT_MI=A_SILT_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   M_GREEN = M_GREEN)

  ### format inputs for OBIC
  dt[, B_SC_WENR := OBIC::format_soilcompaction(B_SC_WENR)]
  dt[, B_GWL_CLASS := OBIC::format_gwt(B_GWL_CLASS)]

  # estimate derivatives: sealing risk, precipitaiton surplus and saturated permeability
  dt[, D_SE := OBIC::calc_sealing_risk(A_SOM_LOI, A_CLAY_MI)]
  dt[, D_PSP := OBIC::calc_psp(B_LU_BRP,M_GREEN), by = id]
  dt[, D_WRI_K := OBIC::calc_permeability(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI)]

  # estimate distance to target for soil compaction and seasling
  dt[, I_P_CO := OBIC::ind_compaction(B_SC_WENR)]
  dt[, I_P_SE := OBIC::ind_sealing(D_SE, B_LU_BRP)]

  # calculate indicator for groundwater recharge
  dt[, I_H_GWR := OBIC::ind_gw_recharge(B_LU_BRP, D_PSP, D_WRI_K, I_P_SE, I_P_CO, B_DRAIN, B_GWL_CLASS)]

  # extract value
  value <- dt[, I_H_GWR]

  # return value
  return(value)

}
