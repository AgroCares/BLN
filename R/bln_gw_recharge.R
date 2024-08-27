#' Function to calculate and evaluate the groundwater recharche in view of the soils' function to retain water
#'
#' @param ID (character) A field id
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
bln_wat_groundwater_recharge <- function(ID,B_LU_BRP,B_SC_WENR,B_GWL_CLASS,B_DRAIN,
                                         A_CLAY_MI,A_SAND_MI, A_SILT_MI, A_SOM_LOI,M_GREEN){

  # add visual bindings
  bln_crops = code = choices = value_min = value_max = D_SE = D_PSP = FIELD_ID = D_WRI_K = I_P_CO = I_P_SE = NULL
  # make internal copy
  blnp <- BLN::bln_parms

  # length of inpurt arguments
  arg.length <- max(length(B_LU_BRP),length(B_SC_WENR),length(B_GWL_CLASS),length(B_DRAIN),
                    length(A_CLAY_MI),length(A_SAND_MI),length(A_SILT_MI),
                    length(A_SOM_LOI),length(M_GREEN))

  checkmate::assert_integerish(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(BLN::bln_crops$crop_code), empty.ok = FALSE)
  checkmate::assert_subset(B_SC_WENR, choices = unlist(blnp[code == "B_SC_WENR", choices]))
  checkmate::assert_integerish(B_SC_WENR, len = arg.length)
  checkmate::assert_subset(B_GWL_CLASS, choices = unlist(blnp[code == "B_GWL_CLASS", choices]))
  checkmate::assert_character(B_GWL_CLASS, len = arg.length)
  checkmate::assert_logical(B_DRAIN,len = arg.length)

  # check inputs A parameters
  checkmate::assert_numeric(A_CLAY_MI, lower = blnp[code == "A_CLAY_MI", value_min], upper = blnp[code == "A_CLAY_MI", value_max],len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = blnp[code == "A_SAND_MI", value_min], upper = blnp[code == "A_SAND_MI", value_max],len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = blnp[code == "A_SILT_MI", value_min], upper = blnp[code == "A_SILT_MI", value_max],len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = blnp[code == "A_SOM_LOI", value_min], upper = blnp[code == "A_SOM_LOI", value_max],len = arg.length)
  checkmate::assert_logical(M_GREEN, any.missing = FALSE, len = arg.length)

  # make internal table
  dt <- data.table(FIELD_ID = ID,
                   id = 1:length(B_LU_BRP),
                   B_LU_BRP = B_LU_BRP,
                   B_SC_WENR=as.character(B_SC_WENR),
                   B_GWL_CLASS=B_GWL_CLASS,
                   B_DRAIN=B_DRAIN,
                   A_CLAY_MI=A_CLAY_MI,
                   A_SAND_MI=A_SAND_MI,
                   A_SILT_MI=A_SILT_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   M_GREEN = M_GREEN,
                   value = NA_real_)

  ### format inputs for OBIC
  dt[, B_SC_WENR := OBIC::format_soilcompaction(B_SC_WENR)]
  dt[, B_GWL_CLASS := OBIC::format_gwt(B_GWL_CLASS)]

  # estimate derivatives: sealing risk, precipitaiton surplus and saturated permeability
  dt[, D_SE := OBIC::calc_sealing_risk(A_SOM_LOI, A_CLAY_MI)]
  dt[, D_PSP := OBIC::calc_psp(B_LU_BRP,M_GREEN), by = FIELD_ID]
  dt[, D_WRI_K := OBIC::calc_permeability(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI)]

  # estimate distance to target for soil compaction and seasling
  dt[, I_P_CO := OBIC::ind_compaction(B_SC_WENR)]
  dt[, I_P_SE := OBIC::ind_sealing(D_SE, B_LU_BRP)]

  # calculate indicator for groundwater recharge
  dt[, value := OBIC::ind_gw_recharge(B_LU_BRP, D_PSP, D_WRI_K, I_P_SE, I_P_CO, B_DRAIN, B_GWL_CLASS)]

  # extract value I_H_GWR
  value <- dt[, value]

  # return value
  return(value)

}
