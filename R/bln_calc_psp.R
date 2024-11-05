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
