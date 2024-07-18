#' Calculate indicators for water holding capacity in the topsoil
#'
#' This function calculates different kind of Water Retention Indices given the continuous pedotransferfunctions of Wosten et al. (2001)
#' These include : 'water holding capacity','plant available water' and 'Ksat'
#'
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param type (character) The type of water retention index. Options include c('water holding capacity', 'whc','plant available water','paw','Ksat' or 'ksat')
#' @param ptf (character) Pedotransfer functions to calculate van Genuchten parameters. Options include c('Wosten1999', 'Wosten2001', 'Klasse')
#'
#' @references Wosten et al. (2001) Pedotransfer functions: bridging the gap between available basic soil data and missing hydraulic characteristics. Journal of Hydrology 251, p123.
#'
#' @import data.table
#' @import OBIC
#'
#' @examples
#' bln_p_whc(A_CLAY_MI = 20.5,A_SAND_MI = 65,A_SILT_MI = 14.5,A_SOM_LOI = 3.5)
#' bln_p_whc(A_CLAY_MI = 5,A_SAND_MI = 15,A_SILT_MI = 80,A_SOM_LOI = 6.5)
#' bln_p_whc(A_CLAY_MI = 5,A_SAND_MI = 15,A_SILT_MI = 80,A_SOM_LOI = 6.5,
#' type = 'water holding capacity')
#'
#' @return
#' The function returns by default the water holding capacity ('whc') of the top soil (in mm). A numeric value.
#' Other outputs include 'plant available water' (or 'paw') or 'ksat' being the saturated permeability.
#' Soil functions are evaluated given a threshold value and expressed as a distance to target.
#'
#' @export
bln_p_whc <- function(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI,type = 'whc', ptf = 'Wosten1999') {

  # Add visual bindings
  id = thetaS = thetaR = alfa = n = fc = wp = whc = paw = ksat = density = Pleem = mineral = NULL
  bln_country = bln_indicator = bln_st_c1 = bln_st_c2 = bln_st_c3 = NULL

  # Load in the thresholds
  dt.thresholds <- as.data.table(BLN::bln_thresholds)
  dt.thresholds <- dt.thresholds[bln_country == 'EU' & bln_indicator %in%  c('i_p_whc','i_p_paw','i_p_ksat')]

  # Check inputs
  arg.length <- max(length(A_CLAY_MI), length(A_SAND_MI),length(A_SILT_MI), length(A_SOM_LOI))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_character(type, any.missing = FALSE, min.len = 1, len = 1)
  checkmate::assert_subset(type, choices = c('water holding capacity','plant available water','Ksat','whc','paw','ksat'), empty.ok = FALSE)
  checkmate::assert_character(ptf, any.missing = FALSE, min.len = 1, len = 1)
  checkmate::assert_subset(ptf, choices = c('Wosten1999', 'Wosten2001', 'Klasse'), empty.ok = FALSE)
  checkmate::assert_data_table(dt.thresholds,max.rows = 3, min.rows = 3)

  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_LOAM_MI = (A_CLAY_MI + A_SILT_MI),
                   A_SOM_LOI = A_SOM_LOI,
                   value = NA_real_
                  )
  # settings
  p.topsoil = 1
  p.fieldcapacity = 2
  p.wiltingpoint = 4.2
  p.depth = 0.3
  M50 = 150
  Bovengrond = 1

  # express soil texture as fraction of total mineral part (if needed)
  dt[,mineral := A_CLAY_MI + A_SAND_MI + A_SILT_MI]
  dt[,A_CLAY_MI := A_CLAY_MI * 100 / mineral]
  dt[,A_SAND_MI := A_SAND_MI * 100 / mineral]
  dt[,A_SILT_MI := A_SILT_MI * 100 / mineral]
  dt[, Pleem    := A_CLAY_MI + A_SILT_MI]

  # retreive properties of pF curve, given different types of pedotransfer functions

    # calculate water retention parameters given Wosten (1999), based on HYPRES
    if (ptf == "Wosten1999"){
      dt[, c("Dichtheid", "thetaR", "thetaS", "alfa", "n", "ksat") := OBIC::pFpara_ptf_Wosten1999(A_CLAY_MI, A_SILT_MI, A_SOM_LOI, Bovengrond)]}

    # calculate water retention parameters given Wosten (2001)
    if (ptf == "Wosten2001"){
      dt[,  c("Dichtheid", "thetaR", "thetaS", "alfa", "n", "ksat", "l") := OBIC::pFpara_ptf_Wosten2001(A_CLAY_MI, Pleem, A_SOM_LOI, M50, Bovengrond)]}

    # class-translation function Staringreeks
    if (ptf == "Klasse"){
      dt[,  c("thetaR", "thetaS", "alfa", "n", "ksat") := OBIC::pFpara_class(A_CLAY_MI, Pleem, A_SOM_LOI, M50)]}

  # retrieve moisture content at certain pF values
  dt[,wp := pF_curve(-1 * 10^p.wiltingpoint, thetaR, thetaS, alfa, n)]
  dt[,fc := pF_curve(-1 * 10^p.fieldcapacity, thetaR, thetaS, alfa, n)]
  dt[,whc := pF_curve(-1 * 10^0, thetaR, thetaS, alfa, n)]
  dt[,paw := abs(fc - wp) * p.depth * 1000]

  # convert from % to mm (wp) and mm (fc)
  dt[,wp := wp * p.depth * 1000]
  dt[,fc := fc * p.depth * 1000]

  # select Water Retention index, and convert to a soil quality index

    # subset and evaluate for water holding capcity
    if(type %in% c('water holding capacity','whc')){

      dths <- dt.thresholds[bln_indicator == 'i_p_whc']
      dt[, value := bln_evaluate_logistic(whc, b = dths[,bln_st_c1], x0 = dths[,bln_st_c2],v = dths[,bln_st_c3])]

    }

    # subset and evaluate for plant available water
    if(type %in% c('plant available water','paw')){

      dths <- dt.thresholds[bln_indicator == 'i_p_paw']
      dt[, value := bln_evaluate_logistic(paw, b = dths[,bln_st_c1], x0 = dths[,bln_st_c2],v = dths[,bln_st_c3])]

    }

    # subset and evaluate for plant available water
    if(type %in% c('Ksat','ksat')){

      dths <- dt.thresholds[bln_indicator == 'i_p_ksat']
      dt[, value := bln_evaluate_logistic(ksat, b = dths[,bln_st_c1], x0 = dths[,bln_st_c2],v = dths[,bln_st_c3])]

    }

  # return selected Water Retention index
  value <- dt[, value]

  # return
  return(value)

}






