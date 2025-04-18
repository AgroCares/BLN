#'Function to calculate and evaluate the water buffering capacity of soils in view of water retention in soil
#'
#' This function gives the WB score of the BBWP framework
#'
#' @param ID (character) A field id
#' @param B_LU_BRP (numeric) The crop code
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_HELP_WENR (character) The soil type abbreviation, derived from 1:50.000 soil map
#' @param B_AREA_DROUGHT (boolean) is the field located in an area with high risks for water deficiencies (options: TRUE or FALSE)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param penalty (boolean) the option to apply a penalty for high risk BBWP field indicators. Default is TRUE.
#'
#' @import data.table
#' @import OBIC
#'
#' @examples
#' bln_bbwp_bw(
#' ID = 15,
#' B_LU_BRP = c(233,259,2014,308),
#' B_HELP_WENR = c('gMn53C', 'Mn25C','U0102nr108','nSn13A'),
#' B_GWL_CLASS = rep('GtVI',4),
#' B_AREA_DROUGHT = rep(TRUE,4),
#' A_CLAY_MI = rep(20,4),
#' A_SAND_MI = rep(15,4),
#' A_SILT_MI = rep(10,4),
#' A_SOM_LOI = c(2,3,5,8),
#' penalty = TRUE
#' )
#'
#' @export
bln_bbwp_bw <- function(ID,B_LU_BRP,B_HELP_WENR,B_GWL_CLASS,B_AREA_DROUGHT,A_CLAY_MI, A_SAND_MI, A_SILT_MI, A_SOM_LOI,penalty = TRUE){

  # add visual bindings
  bln_country = code = crop_code = choices = value_min = value_max = . = crop_cat1 = wue_wwri = wue_wdri = wue_whc = risk_cor = NULL
  group = risk = mcf = CROP_ID = cfwb = d_opi_wb = wue = NULL

  # load internal table
  dt.crop <- BLN::bln_crops[bln_country=='NL']

  # make internal copy
  blnp <- BLN::bln_parms

  # check inputs B parameters
  arg.length <- max(length(B_LU_BRP),length(B_HELP_WENR), length(B_GWL_CLASS),length(B_AREA_DROUGHT),
                    length(A_CLAY_MI),length(A_SAND_MI),length(A_SILT_MI),
                    length(A_SOM_LOI))
  checkmate::assert_subset(B_LU_BRP, choices = unlist(BLN::bln_crops$crop_code))
  checkmate::assert_integerish(B_LU_BRP, len = arg.length)
  checkmate::assert_subset(B_HELP_WENR, choices = unlist(blnp[code == "B_HELP_WENR", choices]))
  checkmate::assert_character(B_HELP_WENR, len = arg.length)
  checkmate::assert_subset(B_GWL_CLASS, choices = unlist(blnp[code == "B_GWL_CLASS", choices]))
  checkmate::assert_character(B_GWL_CLASS, len = arg.length)
  checkmate::assert_logical(B_AREA_DROUGHT,len = arg.length)
  checkmate::assert_logical(penalty,len = 1)

  # check inputs A parameters
  checkmate::assert_numeric(A_CLAY_MI, lower = blnp[code == "A_CLAY_MI", value_min], upper = blnp[code == "A_CLAY_MI", value_max],len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = blnp[code == "A_SAND_MI", value_min], upper = blnp[code == "A_SAND_MI", value_max],len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = blnp[code == "A_SILT_MI", value_min], upper = blnp[code == "A_SILT_MI", value_max],len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = blnp[code == "A_SOM_LOI", value_min], upper = blnp[code == "A_SOM_LOI", value_max],len = arg.length)

  # make internal table
  dt <- data.table(FIELD_ID = ID,
                   CROP_ID = 1:length(B_LU_BRP),
                   B_LU_BRP = B_LU_BRP,
                   B_HELP_WENR = B_HELP_WENR,
                   B_GWL_CLASS = B_GWL_CLASS,
                   B_AREA_DROUGHT = B_AREA_DROUGHT,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   value = NA_real_
                   )

  # merge with crop category
  dt <- merge(dt,dt.crop[,.(crop_code,crop_cat1)],by.x='B_LU_BRP',by.y='crop_code',all.x = TRUE)

  # Replace '-' with 'unknown'
  dt[! B_GWL_CLASS %in% c('GtI','GtII','GtIII','GtIV','GtV', 'GtVI','GtVII','GtVIII'), B_GWL_CLASS := '-']

  # estimate field properties

    # calculate the OBIC water risk index for wetstress (% yield reduction)
    dt[, wue_wwri := 1] # When B_HELP_WENR is `unknown`
    if (nrow(dt[B_HELP_WENR != 'unknown',]) > 0) {
      dt[B_HELP_WENR != 'unknown', wue_wwri := OBIC::calc_waterstressindex(
        B_HELP_WENR = B_HELP_WENR,
        B_LU_BRP = B_LU_BRP,
        B_GWL_CLASS = B_GWL_CLASS,
        WSI = 'wetnessstress'
      ) * 0.01]
    }

    # calculate the OBIC water risk index for droughtstress (% yield reduction)
    dt[, wue_wdri := 1] # When B_HELP_WENR is `unknown`
    if (nrow(dt[B_HELP_WENR != 'unknown',]) > 0) {
      dt[B_HELP_WENR != 'unknown', wue_wdri := OBIC::calc_waterstressindex(
        B_HELP_WENR = B_HELP_WENR,
        B_LU_BRP = B_LU_BRP,
        B_GWL_CLASS = B_GWL_CLASS,
        WSI = 'droughtstress'
      ) * 0.01]
    }

    # calculate the possibility to store water (water holding capacity)
    dt[,wue_whc := OBIC::calc_waterretention(A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI,A_SOM_LOI = A_SOM_LOI,
                                             type = 'water holding capacity'
                                             )]

    # transform wue_whc to an index between 0 and 1
    dt[,wue_whc := 1 - evaluate_logistic(wue_whc, b = 25, x0 = 0.4,v = 0.35)]

  # estimate field indicator

    # columns to be selected
    cols <- colnames(dt)[grepl('wue_|ID',colnames(dt))]

    # melt the data.table to simplify corrections
    dt.melt <- data.table::melt(dt[,mget(cols)], id.vars = c('FIELD_ID','CROP_ID'),variable.name = 'risk')

    # add correction factor based on risk itself
    dt.melt[,risk_cor := wf(value,type = "indicators",penalty = penalty)]

    # add groups of risk indicators
    dt.melt[,group := gsub('_[a-z]+$','',gsub('d_','',risk))]

    # add manual weighing factor for risks
    dt.melt[,mcf := 1]
    dt.melt[group=='wue' & grepl('_whc$',risk), mcf := 2]

    # calculate the mean aggregated risk indicators
    dt.ind <- dt.melt[,list(risk = sum(risk_cor * value * mcf)/sum(risk_cor * mcf)),by=c('FIELD_ID','CROP_ID','group')]
    dt.ind <- dcast(dt.ind,FIELD_ID+CROP_ID~group,value.var='risk')

    # sort output based on crop_id, being equal to length of input variables
    setorder(dt.ind,CROP_ID)

    # add field indicator to the dt
    dt <- merge(dt,dt.ind,by='CROP_ID')

  # estimate field score

    # correction when field is in a region with high water deficiency risks
    dt[,cfwb := fifelse(B_AREA_DROUGHT, 1, 0.5)]

    # calculate the individual opportunity indexes
    dt[,d_opi_wb := (0.5 + cfwb/2) * bln_evaluate_logistic(wue, b=6, x0=0.4, v=.7)]

    # update the field score with measures (assume no measures to be taken)
    dt[,d_opi_wb := pmax(0,1 - pmax(0, d_opi_wb - 0))]

    # Convert form 0-1 to 0-100
    dt[,value := d_opi_wb]

  # return value s_bbwp_wb
  value <- dt[, value]

  return(value)

}

