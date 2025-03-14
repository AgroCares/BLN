#' Simulate SOC evolution using Roth-C for the Netherlands
#'
#' This function calculates the change in carbon stock or C pools (in kg C per ha) based on organic matter amendments, crop rotation, and long-term averaged weather conditions.
#'
#' @param crops (data.table) Table with crop rotation details and crop management actions that have been taken. Includes also crop inputs for carbon. See details for desired format.
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_DEPTH (numeric) Depth for which soil sample is taken (m). Default set to 0.3.
#' @param B_DEPTH (numeric) Depth of the cultivated soil layer (m), simulation depth. Default set to 0.3.
#' @param M_TILLAGE_SYSTEM (character) gives the tillage system applied. Options include NT (no-till), ST (shallow-till), CT (conventional-till) and DT (deep-till).
#' @param weather (data.table) Table with mean monthly temperatures (W_TEMP_MEAN_MONTH, degrees), precipitation (W_PREC_MEAN_MONTH, mm) and actual evapo-transpiration (W_ET_ACT_MONTH, mm), used in Roth-C
#' Optional input might include the potential evapo-transpiration (W_ET_POT_MONTH, mm). Default value for dt is NULL.
#' @param cf_yield (numeric) A relative yield correction factor (fraction) if yield is higher than regional average
#' @param rothc_amendment (data.table) A table with the following column names: year, month, P_NAME, P_DOSE, P_HC, P_OM, and p_p2o5, where month is optional.
#' @param rothc_parms (list) A list with simulation parameters controlling the dynamics of RothC Model. Default is NULL. For more information, see details.
#'
#' @details
#' This function simulates the fate of soil-C given the impact of soil properties, weather and management.
#' The following inputs are mandatory: crops, A_SOM_LOI (\%), and A_CLAY_MI (\%). All other data is optional.
#' When no weather inputs are given, these are estimated from long-term average weather conditions in the Netherlands.
#'
#' The organic amendment table includes the columns year, month, P_NAME, P_DOSE, P_HC, P_OM, and p_p2o5, where month is optional.
#' P_NAME is the fertilizer name, P_DOSE has units (kg / ha), P_HC is the humification coefficient (fraction), P_OM is the organic matter content (%) and p_p2o5 is the phosphate content (%)
#'
#' The crop table includes the columns: year, B_LU (a crop id), B_LU_NAME (a crop name), B_LU_EOM_CROP (the effective organic matter content, kg/ha), B_LU_EOM_CROPRESIDUE (the effective organic matter content for crop residues, kg/ha), and the B_LU_HC (the humification coeffient,-).
#' The crop table might include the measures M_GREEN, M_GREEN_PROPERLY, M_CROPRESIDUE, M_IRRIGATION and M_RENEWAL, all in upper case.
#' * M_GREEN_TIMING (character) the month in which the catch crop is sown, options: (august,september,october,november,never)
#' * M_CROPRESIDUE (boolean) gives whether crop residues are amended to the soil after harvest.
#' * M_IRRIGATION (boolean) gives whether the crop is irrigated.
#' * M_RENEWAL (boolean) gives whether the grassland is renewed (only applicable for grassland)
#'
#' The simulation of C via the RothC model can be adapted by the following parameters: initialize, c_fractions, dec_rates, simyears and unit.
#' These have to be in a list called 'rothc_parms'. The list can be generated with the function \link{ccr_update_parms_nl}. For more details on the arguments and assumptions made, see the aforementioned link.
#'
#' @export
cc_rothc_sim_nl <- function(crops,
                            A_SOM_LOI,
                            A_CLAY_MI,
                            A_DEPTH = 0.3,
                            B_DEPTH = 0.3,
                            cf_yield = 1,
                            M_TILLAGE_SYSTEM = 'CT',
                            weather = NULL,
                            rothc_amendment = NULL,
                            rothc_parms = NULL
){

  # add farm management measures
  checkmate::assert_character(M_TILLAGE_SYSTEM, any.missing = FALSE)
  checkmate::assert_subset(M_TILLAGE_SYSTEM,choices = c('NT','ST','CT','DT'), empty.ok = FALSE)

  crot <- BLN::bln_scen_croprotation[b_aer_cbs=='LG01' & soiltype=='clay' & scen=='bld_arable_int']
  i <- 'BAU'
  crops = crot$b_lu_brp
  this.som = 4.5
  this.clay = 7.5
  A_DEPTH = 0.3
  B_DEPTH = 0.3

  # make input files crops and amendment for scenario
  scen.inp <- rothc_scenario(B_LU_BRP = crot$b_lu_brp, scen = i)
  rotation <- scen.inp$rotation
  amendment <- scen.inp$amendment

  # add generic settings
  cf_yield = if(i=='BAU') {1} else {1.05}
  simyears = 50; init = FALSE;spinup = 10
  M_TILLAGE_SYSTEM = "CT"
  weather = NULL
  rothc_amendment = amendment
  pool_fractions <- rothc_initialise(B_LU_BRP = crot$b_lu_brp,
                                     A_SOM_LOI = this.som,A_CLAY_MI = this.clay)

  rothc_parms = list(simyears = simyears + spinup,
                     c_fractions = pool_fractions,
                     initialize = init)

  # make weather database
  dt.weather <- data.table(month = 1:12,
                  temp = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2),
                  prec = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
                  et_pot = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5),
                  et_act = NA_real_)


  # checks and update the input tables for crop rotation and related management

    # update some basic properties
    dt.crop <- copy(rotation)
    if(!'M_GREEN_TIMING' %in% colnames(dt.crop)){dt.crop[,M_GREEN_TIMING := 'never']}
    if(!'M_CROPRESIDUE' %in% colnames(dt.crop)){dt.crop[,M_CROPRESIDUE := FALSE]}
    if(!'M_IRRIGATION' %in% colnames(dt.crop)){dt.crop[,M_IRRIGATION := FALSE]}
    if(!'M_RENEWAL' %in% colnames(dt.crop)){dt.crop[,M_RENEWAL := FALSE]}
    if(!'CF_YIELD' %in% colnames(dt.crop)){dt.crop[,CF_YIELD := cf_yield[1]]}

    # Set years to 1:x
    dt.crop[,YEAR := year - min(year) + 1]

    # Update EOM input from temporary grassland
    dt.crop[,crft := fifelse(grepl('nl_266',B_LU),1,0)]

    # Calculate consecutive temporary grassland years
    for(i in 1:nrow(dt.crop)){if(i == 1){dt.crop[,crft := 0 + crft]}else{dt.crop[i,crft := fifelse(crft == 0, 0,crft + dt.crop[i - 1, crft])]}}

    # Update B_LU_EOM based on grass age
    dt.crop[crft == 1, B_LU_EOM := 1175]
    dt.crop[crft == 2, B_LU_EOM := 2575]
    dt.crop[crft >= 3, B_LU_EOM := 3975]
    dt.crop[,crft := NULL]

    # add dpm-rmp ratio
    dt.crop[,fr_dpm_rpm := fifelse(B_LU_HC < 0.92, -2.174 * B_LU_HC + 2.02, 0)]

    # replace dpm-rmp ratio with defaults when information hc is missing
    dt.crop[is.na(fr_dpm_rpm), fr_dpm_rpm := 1.44]

    # estimate total Carbon input per crop and year (kg C / ha)
    dt.crop[, cin_crop := B_LU_EOM * 0.5 / B_LU_HC]
    dt.crop[, cin_res := B_LU_EOM_RESIDUE *0.5 / B_LU_HC]

    # estimate averaged C input for DPM, RDM and HUM pool (kg C / ha)
    dt.crop[,cin_crop_dpm := cin_crop * fr_dpm_rpm / (1 + fr_dpm_rpm)]
    dt.crop[,cin_crop_rpm := cin_crop * 1 / (1 + fr_dpm_rpm)]
    dt.crop[,cin_res_dpm := cin_res * fr_dpm_rpm / (1 + fr_dpm_rpm)]
    dt.crop[,cin_res_rpm := cin_res * 1 / (1 + fr_dpm_rpm)]

    # set residue to zero when residues are removed from the field
    dt.crop[M_CROPRESIDUE == FALSE,cin_res_dpm := 0]
    dt.crop[M_CROPRESIDUE == FALSE,cin_res_rpm := 0]

    # select only relevant columns with C input (kg C/ ha)
    dt.crop <- dt.crop[,list(year = YEAR,B_LU,cin_crop_dpm, cin_crop_rpm,cin_res_dpm,cin_res_rpm,
                             M_GREEN_TIMING,M_IRRIGATION,M_CROPRESIDUE,M_RENEWAL,
                             cf_yield = CF_YIELD)]
    # add tillage system
    dt.crop[,M_TILLAGE_SYSTEM := M_TILLAGE_SYSTEM]

  # checks and update the input tables for organic amendments

    # make local copy
    dt.org <- copy(rothc_amendment)

    # Set years to 1:x
    dt.org[,year := year - min(year) + 1]

    # add month = NA when no input given
    if(!'month' %in% colnames(dt.org)){dt.org[,month := NA_real_]}

    # add dpm-rmp ratio
    dt.org[,fr_dpm_rpm := fifelse(P_HC < 0.92, -2.174 * P_HC + 2.02, 0)]

    # estimate total Carbon input per crop and year (kg product * % organic matter * C-fraction = kg C / ha)
    dt.org[, cin_tot := P_DOSE * P_OM * 0.01 * 0.5]

    # estimate C input for DPM, RDM and HUM pool
    dt.org[, cin_hum := 0.02 * cin_tot]
    dt.org[, cin_dpm := (1 - 0.02) * cin_tot * fr_dpm_rpm/ (1 + fr_dpm_rpm)]
    dt.org[, cin_rpm := (1 - 0.02) * cin_tot - cin_dpm]

    # select only relevant columns
    dt.org <- dt.org[,list(p_name = P_NAME, year, cin_tot, cin_hum, cin_dpm, cin_rpm, fr_eoc_p = P_OM * P_HC * 0.5 / p_p2o5)]

  # check and update the default simulation inputs for model parameters RothC

    # add checks
    if(is.null(rothc_parms$initialize)){initialize <- TRUE}
    if(is.null(rothc_parms$simyears)){simyears <- 50}
    if(is.null(rothc_parms$unit)){unit <- 'A_SOM_LOI'}
    if(is.null(rothc_parms$poutput)){poutput <- 'year'}

    # set values for decomposition rates
    k1 = 10; k2 = 0.3; k3 = 0.66; k4 = 0.02
    if('k1' %in% c(names(rothc_parms$dec_rates),colnames(rothc_parms$dec_rates))){k1 <- rothc_parms$dec_rates[['k1']]}
    if('k2' %in% c(names(rothc_parms$dec_rates),colnames(rothc_parms$dec_rates))){k2 <- rothc_parms$dec_rates[['k2']]}
    if('k3' %in% c(names(rothc_parms$dec_rates),colnames(rothc_parms$dec_rates))){k3 <- rothc_parms$dec_rates[['k3']]}
    if('k4' %in% c(names(rothc_parms$dec_rates),colnames(rothc_parms$dec_rates))){k4 <- rothc_parms$dec_rates[['k4']]}

    # set values for C distribution
    fr_IOM = fr_DPM = fr_RPM = fr_BIO = NULL
    if('fr_IOM' %in% c(names(rothc_parms$c_fractions),colnames(rothc_parms$c_fractions))){fr_IOM <- rothc_parms$c_fractions[['fr_IOM']]}
    if('fr_DPM' %in% c(names(rothc_parms$c_fractions),colnames(rothc_parms$c_fractions))){fr_DPM <- rothc_parms$c_fractions[['fr_DPM']]}
    if('fr_RPM' %in% c(names(rothc_parms$c_fractions),colnames(rothc_parms$c_fractions))){fr_RPM <- rothc_parms$c_fractions[['fr_RPM']]}
    if('fr_BIO' %in% c(names(rothc_parms$c_fractions),colnames(rothc_parms$c_fractions))){fr_BIO <- rothc_parms$c_fractions[['fr_BIO']]}

    # update
    rp <- list(initialize = initialize,
               dec_rates = c(k1 = k1, k2 = k2, k3 = k3, k4 = k4),
               c_fractions = c(fr_IOM, fr_DPM, fr_RPM, fr_BIO),
               simyears = simyears,
               unit = unit,
               poutput = poutput)

  # prepare a list with RothC model inputs

    # adapt decomposition rate when ploughing is reduced
    if('NT' %in% dt.crop$M_TILLAGE_SYSTEM){rp$dec_rates['k4'] = 0.8 * rp$dec_rates['k4']}
    if('ST' %in% dt.crop$M_TILLAGE_SYSTEM){rp$dec_rates['k4'] = 0.9 * rp$dec_rates['k4']}

    # select crop cover and Makkink correction factor
    dt <- bln_rothc_makkink(crops = dt.crop, A_CLAY_MI = A_CLAY_MI)

    # what years is grassland renewed
    renew.year <- unique(floor(dt[M_RENEWAL == TRUE, time])) + 1

    # estimate rate modifying factors
    years <- 1:(nrow(dt)/12)

    d2 <- copy(dt.weather)
    d2[,B_DEPTH := B_DEPTH]
    d2[,A_CLAY_MI := A_CLAY_MI]

    # add correction factor for temperature
    d2[, cf_temp :=  47.9/(1+exp(106/(temp + 18.3)))]

    # add maximal top soil moisture deficit (TSMD) and bare soil moisture deficit
    d2[, tsmdmax := -(20 + 1.3 * A_CLAY_MI - 0.01 * (A_CLAY_MI^2)) * B_DEPTH / 0.23]

    # combine rotation data in one table with monthly means, and ensure that mcf is smaller than 2
    dt.rot <- data.table(time = dt$time,
                         year = floor(dt$time),
                         month =  round(12 * (dt$time - floor(dt$time)) + 1),
                         crop_cover = dt$crop_cover,
                         mcf = pmin(2,dt$mcf))

    # add weather data and reorder on time
    d2 <- merge(dt.rot,d2,by='month')
    setorder(d2,time)

    # correct for bare SMD when there is no crop growing
    d2[, tsmdmax_cor := fifelse(crop_cover==1,tsmdmax,tsmdmax/1.8)]

    # add actual evapo-transpiration and soil moisture deficit
    d2[is.na(et_act),et_act := et_pot * mcf]
    d2[,smd := prec- et_act]

    # add accumulated soil moisture deficit, uncorrected
    d2[, hv := pmin(1,cumsum(fifelse(smd<0,1,0))),by='year']
    d2[, acc_smd := hv * smd]

    # helper function to estimate soil moisture deficit per year
    hfun <- function(x,xmax){o=x;for(i in 2:length(x)){o[i] <- min(0,max(xmax[i], x[i] + x[i-1]))};return(abs(o))}

    # estimate soil moisture deficit, corrected
    d2[,acc_smd2 := hfun(acc_smd,tsmdmax_cor),by='year']
    d2[,tsmdmax := abs(tsmdmax)]

    # add rate modifying factor for moisture
    d2[,cf_moist := fifelse(acc_smd2 < 0.444 * tsmdmax,1, pmax(0.2,1 - 0.8 * (acc_smd2 - 0.44*tsmdmax)/ (tsmdmax - 0.444*tsmdmax)))]

    # add rate modifying factor for soil cover
    d2[,cf_soilcover := fifelse(crop_cover==1,0.6,1)]

    # add rate modifying factor for grassland renewal
    d2[,cf_renewal := fifelse(year %in% renew.year & month == 1,1,0)]

    # add combined rate modifying factor
    d2[,cf_combi := cf_temp * cf_moist * cf_soilcover]

    # order the output on time
    setorder(d2,time)

    # format output
    rothc.mf <- dt[,list(time = time,a = cf_temp, b = cf_moist, c = cf_soilcover, d = cf_renewal, abc = cf_combi)]

    # extend duration of event block and correction factors

    # add an unique ID
    rothc.mf[,id := .I]

    ## extend crop table for the number of years
    rothc.mf <- rothc.mf[rep(id, each = ceiling(simyears / max(time)))]

    ## update the time for all repetitions of rotation block
    rothc.mf[,yr_rep := 1:.N, by = id]
    rothc.mf[,year := (yr_rep - 1) * ceiling(max(time)), by = yr_rep]
    rothc.mf[,time := year + time]

    # filter only the years for simulation
    rothc.mf <- rothc.mf[round(time) <= simyears]

    # remove helper columns
    rothc.mf[,c('id','year','yr_rep') := NULL]

    # order
    setorder(rothc.mf,time)

    # Update time to match events
    rothc.mf[,time := time + 1/12]

    # calculate interpolation for correction factors
    abc <- stats::approxfun(x = rothc.mf$time,y = rothc.mf$abc, method = "linear",rule=2)
    d <- stats::approxfun(x = rothc.mf$time,y = rothc.mf$d, method = "constant",f=1,rule=2)

    # calculate correction factor for soil structure
    R1 = 1/((1.67*(1.85+1.6*exp(-0.0786*A_CLAY_MI)))+1)

    # combine RothC input parameters
    rothc.inputs <- as.list(c(rp$dec_rates, R1 = R1, abc = abc, d = d))

  # prepare EVENT database with all C inputs over time
    rothc.event <- bln_rothc_event(crops = dt.crop,
                                   amendment = dt.org,
                                   A_CLAY_MI = A_CLAY_MI,
                                   simyears = rp$simyears)

  # Correct A_SOM_LOI for sampling depth
  if(A_DEPTH < 0.3 & A_CLAY_MI <= 10){A_SOM_LOI <- A_SOM_LOI * (1 - 0.19 * ((0.20 - (pmax(0.10, A_DEPTH) - 0.10))/ 0.20))}
  if(A_DEPTH < 0.3 & A_CLAY_MI >  10){A_SOM_LOI <- A_SOM_LOI * (1 - 0.33 * ((0.20 - (pmax(0.10, A_DEPTH) - 0.10))/ 0.20))}

  # initialize SOM pools (kg C / ha)
  rothc.ini <- cc_rothc_cpools_nl(B_LU = crops$B_LU,
                                  A_SOM_LOI = A_SOM_LOI,
                                  A_CLAY_MI = A_CLAY_MI,
                                  A_DEPTH = A_DEPTH,
                                  B_DEPTH = B_DEPTH,
                                  initialize = rp$initialize,
                                  c_fractions = rp$c_fractions,
                                  c_inputs = rothc.event,
                                  abc_inputs = rothc.inputs$abc,
                                  rp_inputs = rp$dec_rates
  )

  # run RothC model
  rothc.soc <- cc_rothc_calc_nl(pool_size = rothc.ini,
                                event = rothc.event,
                                parms = rothc.inputs,
                                simyears = rp$simyears,
                                unit = 'all',
                                poutput = rp$poutput)

  # estimate bulk density
  density = cc_bulk_density(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI)

  # change unit depending on requested unit
  if(rp$unit == 'A_SOM_LOI'){

    rothc.soc[,A_SOM_LOI := soc * 100 * 2 / (density * B_DEPTH * 100 * 100)]
    rothc.soc[,A_SOM_LOI := ccr_correction_sampling_depth(A_SOM_LOI,A_CLAY_MI,A_DEPTH, simulation = 'after')]
    out <- rothc.soc[,list(year,A_SOM_LOI)]

  } else if (rp$unit == 'omb'){

    out <- cc_rothc_omb(crops = crops,
                        amendment = rothc_amendment,
                        cf_yield = cf_yield,
                        A_CLAY_MI = A_CLAY_MI,
                        simyears = rothc_parms$simyears,
                        rothc.simulation = rothc.soc)

  } else if (rp$unit == 'psoc'){

    rothc.soc[,psoc := soc * 100 / (density * B_DEPTH * 100 * 100)]
    out <- rothc.soc[,list(year,psoc)]

  } else if (rp$unit =='cstock'){

    out <- rothc.soc[,list(year,cstock = soc)]

  } else if (rp$unit=='psomperfraction'){

    # do unit conversion for all pools, convert to %SOM
    cols <- c('soc','CDPM','CRPM','CBIO','CHUM','CIOM')
    rothc.soc[,c(cols) := lapply(.SD,function(x) x * 100 * 2 / (density * B_DEPTH * 100 * 100)),.SDcols = cols]
    rothc.soc[,soc := ccr_correction_sampling_depth(soc,A_CLAY_MI,A_DEPTH, simulation = 'after')]
    out <- rothc.soc
    setnames(out,'soc','A_SOM_LOI')
  }


  # update year
  out[,year := year + crops[1,year] - 1]

  # return output
  return(out)
}
