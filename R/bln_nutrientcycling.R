#' Function to calculate and evaluate the NUE
#'
#' This is the NUE calculation as being used in the BBWP framework
#'
#' @param B_LU_BBWP (character) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_HELP_WENR (character) The soil type abbreviation, derived from 1:50.000 soil map
#' @param A_P_AL (numeric) The P-AL content of the soil
#' @param A_P_WA (numeric) The P-content of the soil extracted with water (mg P2O5 / 100 ml soil)
#' @param A_P_CC (numeric) The plant available P content, measured via 0.01M CaCl2 extraction (mg / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil (mg N / kg)
#' @param B_N_RT (numeric) The mean organic nitrogen content of the soil in the LSW region (mg N / kg). Optional.
#' @param B_N_RT_SD (numeric) The variance in organic nitrogen content of the soil in the LSW region (standard deviation) (mg N / kg). Optional.
#' @param penalty (boolean) the option to apply a penalty for high risk BBWP field indicators. Default is TRUE.
#'
#' @import data.table
#' @import OBIC
#'
#' @export
bln_nut_nue <- function(B_LU_BRP,B_HELP_WENR,B_GWL_CLASS,A_P_AL,A_P_CC,A_P_WA,
                        A_N_RT,B_N_RT = NA_real_,B_N_RT_SD = NA_real_,penalty = TRUE){

  # load internal table
  dt.lsw <- BLN::bln_lsw[B_LSW_ID == 'lsw_nlmean']
  dt.crop <- BLN::bln_crops[bln_country=='NL']

  # make internal table
  dt <- data.table(id = 1:length(B_LU_BRP),
                   B_LU_BRP = B_LU_BRP,
                   B_HELP_WENR = B_HELP_WENR,
                   B_GWL_CLASS = B_GWL_CLASS,
                   A_N_RT = A_N_RT,
                   A_P_AL= A_P_AL,
                   A_P_WA = A_P_WA,
                   A_P_CC = A_P_CC,
                   B_N_RT = B_N_RT,
                   B_N_RT_SD = B_N_RT_SD
                   )

  # replace missing LSW properties
  dt[is.na(B_N_RT), B_N_RT := dt.lsw$B_N_RT]
  dt[is.na(B_N_RT_SD), B_N_RT_SD := dt.lsw$B_N_RT_SD]

  # merge with crop category
  dt <- merge(dt,dt.crop[,.(crop_code,crop_cat1)],by.x='B_LU_BRP',by.y='crop_code')

  # Replace '-' with 'unknown'
  dt[! B_GWL_CLASS %in% c('GtI','GtII','GtIII','GtIV','GtV', 'GtVI','GtVII','GtVIII'), B_GWL_CLASS := '-']

  # estimate field properties

    # rank the nitrogen content as an estimate of total N content: a high value means high risk for N leaching
    dt[,ngw_nlv := pnorm(q = A_N_RT, mean = B_N_RT, sd = B_N_RT_SD)]

    # rank the risk for N pool in soil: higher NLV is associated to increased risks for N runoff
    dt[,nsw_nlv := ngw_nlv]

    # do nlv correction for grassland
    dt[grepl('grass',crop_cat1), nsw_nlv := pmax(0, ngw_nlv - 0.5)]

    # calculate the OBIC water risk index for combined drought and wetstress (% yield reduction)
    dt[, npe_wri := 1] # When B_HELP_WENR is `unknown`
    if (nrow(dt[B_HELP_WENR != 'unknown',]) > 0) {
      dt[B_HELP_WENR != 'unknown', npe_wri := OBIC::calc_waterstressindex(
        B_HELP_WENR = B_HELP_WENR,
        B_LU_BRP = B_LU_BRP,
        B_GWL_CLASS = B_GWL_CLASS,
        WSI = 'waterstress'
      ) * 0.01]
    }

    # calculate the P-availability-index (P fertilizer is more efficient on low PBI)
    dt[,npe_pbi := OBIC::calc_phosphate_availability(B_LU_BRP = B_LU_BRP,A_P_AL = A_P_AL,A_P_CC = A_P_CC,A_P_WA = A_P_WA)]

    # transform npe_pbi to an index between 0 and 1
    dt[,npe_pbi := OBIC::ind_phosphate_availability(npe_pbi)]

    # calculate the drought stress, as factor controlling N-efficiency on grassland
    dt[, npe_wdri := 1] # When B_HELP_WENR is `unknown`
    if (nrow(dt[B_HELP_WENR != 'unknown',]) > 0) {
      dt[B_HELP_WENR != 'unknown', npe_wdri := calc_waterstressindex(
        B_HELP_WENR = B_HELP_WENR,
        B_LU_BRP = B_LU_BRP,
        B_GWL_CLASS = B_GWL_CLASS,
        WSI = 'droughtstress'
      ) * 0.01]
    }

    # rank the risk for N efficiency : low A_N_RT means high potential for improvement NUE
    dt[,npe_nlv := 1 - nsw_nlv]

  # estimate field indicator

    # columns to be selected
    cols <- colnames(dt)[grepl('npe_|id',colnames(dt))]

    # melt the data.table to simplify corrections
    dt.melt <- data.table::melt(dt[,mget(cols)], id.vars = 'id',variable.name = 'risk')

    # add correction factor based on risk itself
    dt.melt[,risk_cor := wf(value,type = "indicators",penalty = penalty)]

    # add groups of risk indicators
    dt.melt[,group := gsub('_[a-z]+$','',gsub('d_','',risk))]

    # add manual weighing factor for risks
    dt.melt[,mcf := 1]
    dt.melt[group=='npe' & grepl('_pbi$',risk), mcf := 2]

    # calculate the mean aggregated risk indicators
    dt.ind <- dt.melt[,list(risk = sum(risk_cor * value * mcf)/sum(risk_cor * mcf)),by=c('id','group')]
    dt.ind <- dcast(dt.ind,id~group,value.var='risk')

    # sort output based on id
    setorder(dt.ind,id)

    # add field indicator to the dt
    dt <- merge(dt,dt.ind,by='id')

  # estimate field score

    # correction for need for increased nutrient use efficiency
    dt[,cfnue := 0.5]

    # calculate the individual opportunity indexes
    dt[,d_opi_nue := (0.5 + cfnue/2) * bln_evaluate_logistic(npe, b=6, x0=0.4, v=.7)]

    # update the field score with measures (assume no measures to be taken)
    dt[,d_opi_nue := pmax(0,1 - pmax(0, d_opi_nue - 0))]

    # Convert form 0-1 to 0-100
    dt[,s_bbwp_nue := 100 * d_opi_nue]

  # return value
  value <- dt[, round(s_bbwp_nue,0)]

  return(value)

}

#' Function to calculate and evaluate the nitrogen use efficiency in view of the soils' function to improve nutrient recycling
#'
#' @param B_LU_BBWP (character) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil (mg N / kg)
#'
#' @import data.table
#' @import OBIC
#'
#' @export
bln_nut_nitrogen <- function(B_LU_BRP,B_SOILTYPE_AGR,A_SOM_LOI,A_N_RT){

  # load internal table
  dt.lsw <- BLN::bln_lsw[B_LSW_ID == 'lsw_nlmean']
  dt.crop <- BLN::bln_crops[bln_country=='NL']

  # make internal table
  dt <- data.table(id = 1:length(B_LU_BRP),
                   B_LU_BRP = B_LU_BRP,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT)

  # add CN ratio
  dt[, A_CN_FR := A_SOM_LOI *10 * 0.5 *1000/ A_N_RT]

  # calculate derivative supporting soil properties
  dt[, D_BDS := OBIC::calc_bulk_density(B_SOILTYPE_AGR,A_SOM_LOI)]
  dt[, D_RD := OBIC::calc_root_depth(B_LU_BRP)]
  dt[, D_OC := OBIC::calc_organic_carbon(A_SOM_LOI, D_BDS, D_RD)]
  dt[, D_GA := OBIC::calc_grass_age(ID=id, B_LU_BRP)]

  # estimate the N supply
  dt[, D_NLV := OBIC::calc_nlv(B_LU_BRP, B_SOILTYPE_AGR, A_N_RT, A_CN_FR, D_OC, D_BDS, D_GA)]

  # estimate the indicator for N efficiency
  dt[, i_nut_n := bln_evaluate_parabolic(x = D_NLV, x.top = 100, must.plateau = FALSE)]

  # return value
  value <- dt[, i_nut_n]

  return(value)

}



#' Function to calculate and evaluate the phosphorus use efficiency in view of the soils' function to improve nutrient recycling
#'
#' @param B_LU_BRP (numeric) The crop code
#' @param A_P_AL (numeric) The P-content of the soil extracted with ammonium lactate
#' @param A_P_CC (numeric) The P-content of the soil extracted with CaCl2
#' @param A_P_WA (numeric) The P-content of the soil extracted with water
#'
#' @import data.table
#'
#' @examples
#' bln_nut_phosphorus(B_LU_BRP = 265, A_P_AL = 45, A_P_CC = 2.5)
#' bln_nut_phosphorus(B_LU_BRP = c(265,1019),A_P_AL = c(35,54),A_P_CC = c(2.5,4.5), A_P_WA = c(35,65))
#'
#' @export
bln_nut_phosphorus <- function(B_LU_BRP, A_P_AL = NA_real_, A_P_CC = NA_real_, A_P_WA = NA_real_) {

  # set visual bindings
  i_c_p = bln_country = bln_indicator = id = crop_cat1 = NULL

  # Load in the crops data set and the parms dataset
  dt.crops <- as.data.table(BLN::bln_crops)
  dt.parms <- as.data.table(BLN::bln_parms)
  dt.thresholds <- as.data.table(BLN::bln_thresholds)

  # subset thresholds to Dutch situation for phosphorus
  dt.thresholds <- dt.thresholds[bln_country=='NL' & bln_indicator=='i_c_p']

  # Check length of desired input
  arg.length <- max(length(B_LU_BRP),length(A_P_AL),length(A_P_CC),length(A_P_WA))

  # check the values (update the limits later via dt.parms)
  checkmate::assert_numeric(A_P_AL, lower = 1, upper = 250, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(A_P_CC, lower = 0.1, upper = 100, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(A_P_WA, lower = 1, upper = 250, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(dt.crops$crop_code), empty.ok = FALSE)

  # check that there is only 1 scoring function for P
  checkmate::assert_data_table(dt.thresholds,max.rows = 1)

  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   A_P_AL = A_P_AL,
                   A_P_CC = A_P_CC,
                   A_P_WA = A_P_WA,
                   B_LU_BRP = B_LU_BRP,
                   value = NA_real_
  )

  dt <- merge(dt,dt.crops,by.x = 'B_LU_BRP', by.y = 'crop_code',all.x=TRUE)

  # set the order to the original inputs
  setorder(dt, id)

  # Calculate the phosphate availability for grass (PBI)
  dt[grepl("gras",crop_cat1), value := pmax(0,log(A_P_CC) * (-0.0114 * A_P_AL + 2.5) + 0.0251 * A_P_CC + 2)]

  # Calculate the phosphate availability for maize (PBI)
  dt[grepl("maize",crop_cat1), value := A_P_CC + 0.05 * (A_P_AL / A_P_CC)]

  # calculate the P-availability for arable systems, normalized to a scale with maximum around 6
  dt[grepl("arable",crop_cat1), value := A_P_WA * 0.1]

  # calculate the P-availability for nature
  dt[grepl("nature",crop_cat1), value := 0]

  # convert to the bln score
  dt[, i_nut_p := bln_evaluate_logistic_gaus_down(x = value, b = 1.3, x0 = 1.3, v = 0.35, optimum = 5, optimum_ofset = 2)]

  # return value
  value <- dt[, i_nut_p]

  return(value)

}


#' Function to calculate and evaluate the potassium use efficiency in view of the soils' function to improve nutrient recycling
#'
#' @param B_LU_BRP (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil (mmol+ / kg), analyzed via Cobalt-hexamine extraction
#' @param A_K_CO_PO (numeric) The occupation of the CEC with potassium (\%)
#' @param A_K_CC (numeric) The plant available potassium, extracted with 0.01M CaCl2 (mg / kg),
#'
#' @import data.table
#'
#' @export
bln_nut_potassium <- function(B_LU_BRP, B_SOILTYPE_AGR,A_SOM_LOI, A_CLAY_MI,A_PH_CC,
                               A_CEC_CO, A_K_CO_PO, A_K_CC) {

  # add visual bindings
  id = crop_category = soiltype.n = crop_code = soiltype = NULL
  b = cF = kindex1 = kindex2 = A_PH_KCL = A_K_CO = NULL
  bln_country =bln_indicator = crop_cat1 =bln_soil_cat1 =bln_soil_cat2 = value = NULL
  bln_threshold_cropcat =bln_threshold_soilcat = i_c_k =bln_st_c1 =bln_st_c2 =bln_st_c3 = NULL

  # Load in the datasets
  dt.crops <- as.data.table(bln_crops[bln_country=='NL'])
  dt.soils <- as.data.table(bln_soiltype[bln_country=='NL'])

  # Check inputs
  arg.length <- max(length(A_PH_CC), length(A_SOM_LOI), length(A_CEC_CO), length(A_K_CO_PO),
                    length(A_K_CC), length(A_CLAY_MI), length(B_SOILTYPE_AGR), length(B_LU_BRP))

  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(dt.soils$osi_soil_cat1), empty.ok = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 10, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_K_CC, lower = 0, upper = 800, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_K_CO_PO, lower = 0.1, upper = 50, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CEC_CO, lower = 1, upper = 1000, any.missing = FALSE, len = arg.length)

  # Collect the data
  dt <- data.table(id = 1:arg.length,
                   B_LU_BRP = B_LU_BRP,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC,
                   A_CEC_CO = A_CEC_CO,
                   A_K_CO_PO = A_K_CO_PO,
                   A_K_CC = A_K_CC,
                   value = NA_real_
  )

  # merge with crop and soil classification tables
  dt <- merge(dt, dt.crops[, list(crop_code, crop_cat1)],
              by.x = "B_LU_BRP", by.y = "crop_code", all.x = TRUE)
  dt <- merge(dt, dt.soils[, list(bln_soil_cat1,bln_soil_cat2)],
              by.x = "B_SOILTYPE_AGR", by.y = "bln_soil_cat1",all.x = TRUE)

  # calculate the K-availability index
  dt[, D_K := OBIC::calc_potassium_availability(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC,
                                                A_CEC_CO, A_K_CO_PO, A_K_CC)]

  #  evaluate nutrient cycling indicators for K
  dt[grepl('gras|maiz',crop_cat1),i_nut_k := bln_evaluate_logistic_gaus_down(D_K, b = 8, x0 = 2.5, v = 8, optimum = 20, optimum_ofset = 1.5)]
  dt[grepl('arabl',crop_cat1) & grepl("zand|dal|veen",B_SOILTYPE_AGR),i_nut_k := bln_evaluate_logistic_gaus_down(D_K, b = 0.3, x0 = 9, v = 1.1, optimum = 20, optimum_ofset = 1.5)]
  dt[grepl('arabl',crop_cat1) & grepl("klei",B_SOILTYPE_AGR) & A_SOM_LOI > 10,i_nut_k := bln_evaluate_logistic_gaus_down(D_K, b = 0.4, x0 = 11.5, v = 1.1, optimum = 20, optimum_ofset = 1.5)]
  dt[grepl('arabl',crop_cat1) & (grepl("loess",B_SOILTYPE_AGR)|grepl("klei",B_SOILTYPE_AGR) & A_SOM_LOI <= 10),i_nut_k := bln_evaluate_logistic_gaus_down(D_K, b = 0.5, x0 = 11.5, v = 1.1, optimum = 20, optimum_ofset = 1.5)]
  dt[is.na(i_nut_k),i_nut_k := bln_evaluate_logistic_gaus_down(D_K, b = 8, x0 = 2.5, v = 8, optimum = 20, optimum_ofset = 1.5)]

  # select the output variable
  out <- dt[,i_nut_k]

  # return the OSI score
  return(out)
}

