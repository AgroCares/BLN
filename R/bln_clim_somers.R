#' Function to evaluate the carbon saturation via SOMERS simulation on peat soils
#'
#' @param ID (character) A field id
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) value for the soil organic matter content of the soil
#' @param B_SOMERS_BC (integer) The base combination of SOMERS peat soil classification (varies between 1 and 290)
#' @param B_DRAIN_SP (numeric) the drooglegging of a field in summer (difference field height and ditch level, in meters)
#' @param B_DRAIN_WP (numeric) the drooglegging of a field in winter (difference field height and ditch level, in meters)
#' @param B_DRAIN_SP_CHANGE (numeric) the decrease in drooglegging of a field in summer (in meters). Allowed decrease varies from 0 to 0.5m.
#'
#' @import data.table
#' @import carboncastr
#'
#' @export
bln_clim_somers <- function(ID,B_SOILTYPE_AGR,A_SOM_LOI,B_SOMERS_BC,B_DRAIN_SP,B_DRAIN_WP, B_DRAIN_SP_CHANGE = 0.2){

  # add visual bindings
  bln_country = bln_soil_cat1 = b_drain_sp = b_drain_wp = . = b_somers_bc = ref = awis = pwis = NULL
  ref0 = oid = NULL

  # load internal package
  bln_som <- BLN::bln_somers

  # Check inputs
  arg.length <- max(length(B_SOILTYPE_AGR),length(A_SOM_LOI), length(B_SOMERS_BC),
                    length(B_DRAIN_SP), length(B_DRAIN_WP))
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = BLN::bln_soiltype[bln_country=='NL',bln_soil_cat1], empty.ok = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0.1, upper = 100, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_integerish(B_SOMERS_BC, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOMERS_BC, choices = c(NA,unique(BLN::bln_somers$b_somers_bc)))
  checkmate::assert_numeric(B_DRAIN_SP, lower = 0, upper = 1.2, min.len = 1, len = arg.length)
  checkmate::assert_numeric(B_DRAIN_WP, lower = -0.2, upper = 1.2, min.len = 1, len = arg.length)
  checkmate::assert_numeric(B_DRAIN_SP_CHANGE, lower = 0, upper = 0.5)

  # make internal table
  dt <- data.table(ID = ID,
                   oid = 1:arg.length,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_SOM_LOI = as.numeric(A_SOM_LOI),
                   b_somers_bc = as.integer(B_SOMERS_BC),
                   B_DRAIN_SP = as.numeric(B_DRAIN_SP),
                   B_DRAIN_WP = as.numeric(B_DRAIN_WP) - as.numeric(B_DRAIN_SP),
                   B_DRAIN_SP_CHANGE = B_DRAIN_SP_CHANGE)

  # convert drainage level to unique values in meters, to allow merge with bln_somers
  dt[, b_drain_sp := pmin(1.2,pmax(0.2,round(B_DRAIN_SP,1)))]
  dt[, b_drain_wp := pmin(0,pmax(-0.2,round(B_DRAIN_WP,1)))]

  # add year number
  dt[, year := 1:.N,by='ID']

  # subset only a single year for peaty soils
  dt2 <- dt[year==1 & (A_SOM_LOI >= 20 | B_SOILTYPE_AGR == 'veen')]

  # check if there are peat soils present
  if(nrow(dt2) >0 ){

    # merge with the reference situation from internal SOMERS table with fields table
    dt2 <- merge(dt2,
                 bln_som[,.(b_somers_bc, b_drain_sp, b_drain_wp,ref0 = ref)],
                 by = c('b_somers_bc','b_drain_sp','b_drain_wp'),
                 all.x=TRUE)

    # update groundwater level in summer with 20 cm and leave same change between summer and winter
    dt2[, b_drain_sp := pmin(1.2,pmax(0.2,round(B_DRAIN_SP - B_DRAIN_SP_CHANGE,1)))]

    # merge with the drained situation from internal SOMERS table with fields table
    dt2 <- merge(dt2,
                 bln_som[,.(b_somers_bc, b_drain_sp, b_drain_wp,ref,awis,pwis)],
                 by = c('b_somers_bc','b_drain_sp','b_drain_wp'),
                 all.x=TRUE)

    # calculate the mean maximum C storage distance to target (1 is C saturated)
    dt2[, value := 1 - pmax(0,pmin(1,pmin(awis,pwis,ref,na.rm=T) / ref0))]

    # merge with original input
    out <- merge(dt,dt2[,.(ID,value)],by='ID',all.x=TRUE)

  } else {

    out <- copy(dt)
    out[, value := NA_real_]
  }

  # set indicator to 1 when value is NA (non-peat)
  # out[is.na(value), value := 1]

  # sort the output table according to input
  setorder(out,oid)

  # return value i_clim_somers
  value <- out[, value]

  return(value)

}
