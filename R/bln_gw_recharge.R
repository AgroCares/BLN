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
  dt[, D_PSP := bln_calc_psp(ID = FIELD_ID, B_LU_BRP, M_GREEN)]
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


#' Calculate the precipitation surplus
#'
#' This function calculates the precipitation surplus (in mm / ha) given the crop rotation plan. Is identical to OBIC function calc_psp but much faster to facilitate national and regional calculations
#'
#' @param ID (character) A field id
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, options: TRUE, FALSE)
#'
#' @examples
#' bln_calc_psp(B_LU_BRP = 265, M_GREEN = TRUE)
#' bln_calc_psp(B_LU_BRP = c(265,1019,265,1019), M_GREEN = rep(TRUE,4))
#'
#' @return
#' The estimated precipitation surplus (in mm / ha) depending on averaged precipitation and evaporation. A numeric value.
#'
#' @export
bln_calc_psp <- function(ID, B_LU_BRP, M_GREEN){

  # set visual bindings
  crop_code = crop_name = crop_makkink = psp = A_PREC_MEAN = A_ET_MEAN = mcf = NULL

  # Check input
  arg.length <- max(length(B_LU_BRP), length(M_GREEN),length(ID))

  # check inputs
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(OBIC::crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_logical(M_GREEN,any.missing = FALSE, len = arg.length)

  # Load in the datasets
  dt.weather <- OBIC::weather.obic
  crops.obic <- as.data.table(OBIC::crops.obic)
  crops.makkink <- as.data.table(OBIC::crops.makkink)
  setnames(crops.makkink,old = c('crop_makkink',1:12),new=c('crop_makkink',paste0('m',1:12)))

  # Collect input data in a table
  dt <- data.table(ID = ID,B_LU_BRP = B_LU_BRP,M_GREEN = M_GREEN,oid = 1:arg.length)

  # merge with obic crop
  dt <- merge(dt, crops.obic[, list(crop_code, crop_name, crop_makkink)], by.x = "B_LU_BRP", by.y = "crop_code")

  # merge with makkink fractions
  dt <- merge(dt, crops.makkink, by = "crop_makkink")

  # add year
  dt[,year:= 1:.N,by=ID]

  # Order by year
  setorder(dt,ID,year)

  # Select years with wintercereals and update makkink factor for months 10-12 in previous years
  dt[,year_wc := fifelse(B_LU_BRP %in% c(233,234,235),1,0)]
  dt[,year_wc := shift(year_wc,n=-1,fill=0)]
  dt[year_wc==1, c('m10','m11','m12') := list(0.5,0.6,0.6)]

  # Set M_GREEN to TRUE for maize and potato cultivation
  dt[grepl('mais|aardappel',crop_name) & year_wc ==0, M_GREEN := TRUE]
  dt[year_wc == 1, M_GREEN := FALSE]

  # select year before M_GREEN is TRUE
  dt[,year_wc2 := fifelse(M_GREEN==TRUE,1,0)]
  dt[,year_wc2 := shift(year_wc2,n=-1,fill=0)]
  dt[year_wc2==1 & year_wc==0, c('m10','m11','m12') := list(0.74,0.64,0.6)]
  dt[M_GREEN == TRUE, c('m1','m2','m3') := list(0.6,0.6,0.6)]

  # reformat and merge with weather
  dt2 <- melt(dt,id.vars=c('ID','year','oid'),measure.vars = paste0('m',1:12),variable.name = 'month',value.name = 'mcf')
  dt2[,month := as.integer(gsub('m','',month))]
  dt2 <- merge(dt2,dt.weather,by='month')

  # calculate precipitation surplus
  dt2[, psp := A_PREC_MEAN - A_ET_MEAN * mcf]

  # calculate the precipitation surplus per year
  out <- dt2[,list(psp = sum(psp)),by = c("oid")]

  # reset order to input order
  setorder(out,oid)

  # return value
  D_PSP <- out[,psp]

  # return
  return(D_PSP)

}
