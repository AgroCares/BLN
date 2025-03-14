#' Estimate Makkink correction factors and crop cover to crop rotation table
#'
#' This function adds Makkink correction factors for ET and crop cover for the crop rotation table
#'
#' @param crops (data.table) Table with crop rotation, cultivation measures, year and potential Carbon inputs.
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @export
bln_rothc_makkink <- function(crops, A_CLAY_MI){

  # add visual bindings
  M_GREEN_TIMING = M_RENEWAL = cf_yield = code = value_min = value_max = cc.crops.blu = NULL
  B_LU = B_LU_NAME = B_LU_MAKKINK = crop_makkink = mcf = crop_cover = crflt = NULL

  # check input
  ncrops <- nrow(crops)

  # get internal copies of package tables
  cc.crops <- as.data.table(carboncastr::cc.crops)
  dt.mak <- as.data.table(carboncastr::cc.makkink)
  dt.mak <- melt(dt.mak,id.vars = 'B_LU_MAKKINK', variable.name = 'month',value.name = "mcf",variable.factor = FALSE)
  dt.mak[,month := as.integer(month)]

  # check crops measure
  checkmate::assert_data_table(crops)
  checkmate::assert_true(sum(c('M_GREEN_TIMING','M_IRRIGATION','M_CROPRESIDUE') %in% colnames(crops)) == 3)
  checkmate::assert_numeric(crops$cf_yield,lower = 0, upper = 2.0, any.missing = FALSE)
  checkmate::assert_character(crops$M_GREEN_TIMING, any.missing = FALSE, len = ncrops)
  checkmate::assert_subset(crops$M_GREEN_TIMING,pandex::enum_opts('M_GREEN_TIMING'))
  checkmate::assert_logical(crops$M_CROPRESIDUE,any.missing = FALSE, len = ncrops)
  checkmate::assert_logical(crops$M_IRRIGATION,any.missing = FALSE, len = ncrops)

  # make internal copies of input
  dt <- copy(crops)

  # add crop name to input
  dt <- merge(dt,
              carboncastr::cc.crops.blu[, list(B_LU, B_LU_NAME, B_LU_MAKKINK)],
              by = 'B_LU',
              all.x = TRUE)

  # update input for mandatory catch crops (after maize and potato on sandy soils) and set NA for grassland
  if(A_CLAY_MI <20){ dt[grepl('mais|aardappel',B_LU_NAME) & grepl('^nl_',B_LU), M_GREEN_TIMING := 'october'] }
  dt[grepl('gras|bieten, suiker|bieten, voeder',B_LU_NAME) & grepl('^nl_',B_LU), M_GREEN_TIMING := 'never']


  # extend the crop table with months
  dt <- dt[rep(1:.N,12)]
  dt[,month := 1:.N,by='year']

  # add makkink correction factor
  dt <- merge(dt,dt.mak, by = c('B_LU_MAKKINK','month'),all.x = TRUE)

  # add median Makkink correction factor for non-Dutch crops
  dt[is.na(mcf) & month %in% c(1:3,10:12), mcf := 0.36]
  dt[is.na(mcf) & month == 4, mcf := 0.4]
  dt[is.na(mcf) & month == 5, mcf := 0.66]
  dt[is.na(mcf) & month %in% 6:7, mcf := 1.0]
  dt[is.na(mcf) & month == 8, mcf := 1.06]
  dt[is.na(mcf) & month == 9, mcf := 0.95]

  # add crop cover (crops crowing = 1, no crop = 0)
  dt[,crop_cover := fifelse(mcf>0.36,1,0)]

  # column names to be updated
  cols <- c("B_LU_NAME","crop_cover","mcf")

  # select the years before winter cereals are grown as main crop or when catch crop grown in spring
  year_wc <- dt[grepl('winter',B_LU_NAME) & grepl('tarwe|gerst',B_LU_NAME),unique(pmax(1,year - 1))]
  year_cc <- dt[M_GREEN_TIMING != 'never',unique(pmin(year + 1,max(1,year)))]

  # update B_LU_NAME, crop_cover and mcf for winter crops and catch crops

  # update winter crop in winter months
  dt[year %in% year_wc & month == 10, c(cols) := list("winter cereal", 1, 0.5)]
  dt[year %in% year_wc & month > 10, c(cols) := list("winter cereal", 1, 0.6)]

  # update catch crop in winter months
  dt[M_GREEN_TIMING != 'never', crflt := 1]
  dt[crflt == 1 & month == 10, c(cols):=list("catch crop",1,0.74)]
  dt[crflt == 1 & month == 11,c(cols):=list("catch crop",1,0.64)]
  dt[crflt == 1 & month == 12,c(cols):=list("catch crop",1,0.60)]
  dt[,crflt := NULL]

  # update catch crop in next year
  dt[month < 4 & year %in% year_cc,c(cols) := list("catch crop",1,0.6)]


  # adapt mcf when crop yield is different than average
  dt[!grepl('catch crop',B_LU_NAME), mcf := mcf * cf_yield]

  # sort dt on the original order
  setorder(dt,year,month)

  # add time
  dt[, time := year + (month - 1) / 12 - 1]

  # select relevant outputs
  dt <- dt[,list(time,mcf,crop_cover,M_RENEWAL)]

  # return
  return(dt)
}
