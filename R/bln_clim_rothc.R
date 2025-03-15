#' Function to evaluate the carbon saturation via RothC simulation
#'
#' @param ID (character) A field id
#' @param B_LU_BRP (numeric) value of the BRP crop code
#' @param B_GWL_GLG (numeric) The lowest groundwater level averaged over the most dry periods in 8 years in cm below ground level
#' @param A_SOM_LOI (numeric) value for the soil organic matter content of the soil
#' @param A_CLAY_MI (numeric) value for the clay content of the soil
#' @param quiet (boolean) showing progress bar for calculation RothC C-saturation for each field
#' @param mc (boolean) option to run rothc in parallel on multicores
#'
#' @import data.table
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#'
#' @export
bln_clim_rothc <- function(ID,B_LU_BRP,B_GWL_GLG,A_SOM_LOI,A_CLAY_MI,quiet = FALSE, mc = TRUE){

  # add visual bindings
  i_clim_rothc =A_SOM_LOI_BAU = A_SOM_LOI_ALL = NULL
  id = stype = fr_smv = . = NULL

  # Check inputs
  arg.length <- max(length(B_LU_BRP), length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(BLN::bln_crops$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0.1, upper = 100, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_numeric(B_GWL_GLG, lower = 0, any.missing = FALSE, len = arg.length)
  if(length(ID)>1){checkmate::assert_true(length(ID) == arg.length)}
  checkmate::assert_logical(quiet)

  # make internal table (set clay to 75 max given checkmate)
  dt <- data.table(id = 1:length(ID),
                   ID = ID,
                   B_LU_BRP = B_LU_BRP,
                   B_GWL_GLG = B_GWL_GLG,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI= pmin(75,A_CLAY_MI))

  # add variable to distinghuish peat from mineral soils
  dt[, stype := fifelse(sum(A_SOM_LOI > 20) >= sum(A_SOM_LOI <= 20),'peat','mineral'),by='ID']

  # estimate BLN carbon pools for peat soils via INITIATOR approach (de Vries et al., 2022)

    # subset the input data
    dt.peat <- dt[stype=='peat']

    # lowering surface due to oxidation (Beuving and van den Akker, 1996)
    fsmv <- stats::approxfun(x = seq(0,130,10),
                             y = c(0,0.008,0.017,0.026,0.038,0.051,0.074,0.098,
                                   0.122,0.145,0.169,0.192,0.216,0.239),
                             rule = 2)

    # estimate relative carbon storage compared to GLG with 45 cm higher water level
    dt.peat[, fr_smv := pmin(1,(fsmv(B_GWL_GLG) - pmax(0,fsmv(B_GWL_GLG-45)))/fsmv(B_GWL_GLG)),by=ID]

    # estimate A_SOM_BAU and A_SOM_ALL
    dt.peat[,A_SOM_LOI_BAU := A_SOM_LOI * fr_smv]
    dt.peat[, A_SOM_LOI_ALL := A_SOM_LOI]

    # estimate mean per ID
    dt.peat <- dt.peat[,lapply(.SD,mean),.SDcols = c('A_SOM_LOI_ALL','A_SOM_LOI_BAU'),by=ID]


  # subset the mineral soils
  dt.min <- dt[stype == 'mineral']


  # run RothC to estimate max carbon storage (averaged per field)
  if(mc & nrow(dt.min) >0){

    # calculation multicore
    dt.cs <- BLN::bln_rothc_multicore(ID = dt.min$ID,
                                      B_LU_BRP = dt.min$B_LU_BRP,
                                      B_GWL_GLG = dt.min$B_GWL_GLG,
                                      A_SOM_LOI = dt.min$A_SOM_LOI,
                                      A_CLAY_MI = dt.min$A_CLAY_MI,
                                      scen = c('BAU','ALL'),
                                      simyears = 100,
                                      quiet = quiet)

    # sink() to get back to console

  } else if(mc == FALSE & nrow(dt.min) > 0){

    # add progress bar
    if(!quiet) {pb = txtProgressBar(min = 0, max = length(unique(dt$ID)), initial = 0)}

    # set initial settings
    stepi <- 0 ; out <- list()


    # run RothC to simulate current and potential SOC per field
    for(i in unique(dt.min$ID)){

      # subset the dataset
      stepi = stepi + 1
      this.brp <- dt.min[ID==i,B_LU_BRP]
      this.som <- dt.min[ID==i,A_SOM_LOI]
      this.clay <- dt.min[ID==i,A_CLAY_MI]

      # run the RothC model for mineral soils using fixed scenarios BAU and ALL
      dt.rothc <- bln_rothc_field(this.brp, this.som, this.clay,
                                  simyears = 100,init = FALSE,spinup = 10,
                                  scen = c('BAU','ALL'))

      # take the mean of the last 10 years
      dt.prc <- dt.rothc[year > max(year) - 10,lapply(.SD,mean)]

      # save in list
      out[[stepi]] <- data.table(ID=i,
                                 A_SOM_LOI_BAU = round(dt.prc$A_SOM_LOI_BAU,4),
                                 A_SOM_LOI_ALL = round(dt.prc$A_SOM_LOI_ALL,4))
      # show progressbar
      if(!quiet) {setTxtProgressBar(pb,stepi)}
    }

    # close progress bar
    if(!quiet) {close(pb)}

    # combine all sites in a data.table again
    dt.cs <- rbindlist(out)

  } else {

    # if not mineral soils present
    dt.cs <- data.table(ID=-999,A_SOM_LOI_ALL=0,A_SOM_LOI_BAU = 0)
  }


  # rbind the peat and mineral soils
  dt.combi <- rbind(dt.cs[,.(ID,A_SOM_LOI_ALL, A_SOM_LOI_BAU)],
                    dt.peat[,.(ID,A_SOM_LOI_ALL, A_SOM_LOI_BAU)])

  # calculate the distance to target
  dt.combi[,i_clim_rothc := round(pmin(1,A_SOM_LOI_BAU/A_SOM_LOI_ALL),3)]

  # merge with the internal dt
  dt <- merge(dt[,.(id,ID,B_LU_BRP)],dt.combi,by='ID',all.x=TRUE)

  # set infinite to NA_real
  dt[!is.finite(i_clim_rothc), i_clim_rothc := NA_real_]

  # setorder
  setorder(dt,id)

  # return value
  value <- dt[, i_clim_rothc]

  return(value)

}
#' Function for simple RothC simulation for grassland and cropland field
#'
#' This function calculates the optimal sequestration scenario for cropland and grassland with the RothC model over the course of 50 years
#' For cropland this function converts the crop to winter wheat where crop residue is left behind, grassland field are converted to permanent grassland
#' The function assumes that the P-status of the soil is neutral according to the Dutch fertilization regulation and applies the maximal permitted P-dose in the form of cattle slurry
#'
#' @param B_LU_BRP (numeric) value of the BRP crop code
#' @param A_SOM_LOI (numeric) value for the soil organic matter content of the soil
#' @param A_CLAY_MI (numeric) value for the clay content of the soil
#' @param simyears (integer) value for the amount of years to simulate, default is 50 years
#' @param init (boolean) use internal analytical solution for initialisation RothC
#' @param scen (character) scenarios to be simulated. Options include BAU, BAUIMPR,CLT and ALL.
#' @param spinup (numeric) the spinup period that is used for initialisation model
#'
#' @import data.table
#'
#' @export
bln_rothc_field <- function(B_LU_BRP, A_SOM_LOI, A_CLAY_MI, simyears = 50, init = FALSE,
                            scen = c('BAU','ALL'),spinup = 10){

  # check on inputs
  checkmate::assert_numeric(simyears,len=1,lower=15)
  checkmate::assert_character(scen)
  checkmate::assert_subset(scen,choices = c('BAU','BAUIMPR','CLT','ALL'))

  # a single field can only have one value for SOM and clay
  this.som <- mean(A_SOM_LOI)
  this.clay <- mean(A_CLAY_MI)

  # initialize RothC with BAU for 150 years
  pool_fractions <- rothc_initialise(B_LU_BRP = B_LU_BRP,A_SOM_LOI = this.som,A_CLAY_MI = this.clay)

  # run simulations for the desire scenarios
  sim <- list(); count <- 0

  # run the RothC model for two scenarios by default: BAU and ALL
  for(i in scen){

    # add counter
    count <- count + 1

    # prepare input for the scenarios
    scen.inp <- rothc_scenario(B_LU_BRP = B_LU_BRP, scen = i)

    # retreive the rotation and amendment input data.tables
    rotation <- scen.inp$rotation
    amendment <- scen.inp$amendment

    # set seed
    set.seed(123)

    # Run simulation
    result <- bln_rothc_sim(A_SOM_LOI = this.som,
                            A_CLAY_MI = this.clay,
                            A_DEPTH = 0.3,
                            cf_yield = if(i=='BAU') {1} else {1.05},
                            M_TILLAGE_SYSTEM = "CT",
                            rothc_rotation = rotation,
                            rothc_amendment = amendment,
                            rothc_parms = list(simyears = simyears + spinup,
                                               c_fractions = pool_fractions,
                                               initialize = init))
    # set startyear to zero
    result[,year := year - min(year)]

    # do a manual scaling for the spin-up period
    result[,A_SOM_LOI := A_SOM_LOI * this.som / A_SOM_LOI[spinup]]

    # remove the years before spinput
    result <- result[year >= spinup - 1]
    result[,year := year - min(year)]

    # save output in a list
    sim[[count]] <- data.table(result,scenario = i)

  }

  # combine scenarios
  dt.sim <- rbindlist(sim,fill=T)
  dt.sim <- dcast(dt.sim,year~scenario,value.var ='A_SOM_LOI')

  # update names
  setnames(dt.sim,old = scen,new = paste0('A_SOM_LOI_',scen),skip_absent=TRUE)

  # Return result
  return(dt.sim)

}

#' Function for initializing the RothC model
#'
#' @param B_LU_BRP (numeric) value of the BRP crop code
#' @param A_SOM_LOI (numeric) value for the soil organic matter content of the soil
#' @param A_CLAY_MI (numeric) value for the clay content of the soil
#'
#' @import data.table
#'
#' @export
rothc_initialise <- function(B_LU_BRP,A_SOM_LOI,A_CLAY_MI){

  # add visual bindings
  . = CIOM = CDPM = CRPM = CBIO = NULL

  # Prepare input for scenario Business As Usual
  scen.inp <- rothc_scenario(B_LU_BRP = B_LU_BRP, scen = 'BAU')
  rotation <- scen.inp$rotation
  amendment <- scen.inp$amendment

  # Set model parameters
  parms <- list(simyears = 150,unit = 'psomperfraction', initialize = TRUE)

  # Run initialization run for 30 years
  this.result <- bln_rothc_sim(A_SOM_LOI = A_SOM_LOI,
                               A_CLAY_MI = A_CLAY_MI,
                               rothc_rotation = rotation,
                               rothc_amendment = amendment,
                               rothc_parms = parms)

  # take last two rotations
  this.result.fin <- this.result[year > max(year)-2*nrow(rotation),lapply(.SD,mean)]

  fractions <- this.result.fin[,.(fr_IOM = CIOM / (A_SOM_LOI),
                                  fr_DPM = CDPM / A_SOM_LOI,
                                  fr_RPM = CRPM / A_SOM_LOI,
                                  fr_BIO = CBIO / A_SOM_LOI)]


  # unlist fractions
  fractions <- unlist(fractions)

  # Return output
  return(fractions)

}

#' Function to prepare inputs for various scenarios with RothC
#' @param B_LU_BRP (numeric) value of the BRP crop code
#' @param scen (character) possible scenarios. Include BAU, CLT, BAUIMPR and ALL
#'
#' @export
rothc_scenario <- function(B_LU_BRP, scen){

  # add visual bindings
  . = B_LU_NAME = B_LU_EOM = B_LU_EOM_RESIDUE = B_LU_HC = B_LU_WATERSTRESS_OBIC = NULL
  B_LU = gld = cereal = nat = bld = M_GREEN_TIMING = M_CROPRESIDUE = man_name = NULL
  P_OM = P_HC = P_p2o5 = P_DOSE = P_NAME = p_p2o5 = crop_code = crop_name = NULL

  # composition table for cattle slurry and compost
  dtcm <- data.table(man_name = c('cattle_slurry','green_compost'),
                     P_OM = c(7.1,17.9),
                     P_HC = c(0.7,0.9),
                     p_p2o5 = c(0.15,0.22))

  # combine input data
  dt <- data.table(B_LU_BRP = B_LU_BRP,year = 1:length(B_LU_BRP))
  dt <- merge(dt,
              BLN::bln_crops[,.(B_LU_BRP = crop_code,B_LU_NAME = crop_name,B_LU_EOM,B_LU_EOM_RESIDUE,
                                B_LU_HC,B_LU_WATERSTRESS_OBIC)],
              by = 'B_LU_BRP',
              all.x=TRUE)

  dt[, cat := fifelse(grepl('grasland',B_LU_WATERSTRESS_OBIC),1,0)]
  setorder(dt,year)

  # what is the main crop use
  if(sum(dt$cat)/nrow(dt) > 0.5){ luse <- 'GLD' }else{ luse <- 'BLD'}

  # Scenario BAU

  # Set crop rotation
  rotation <- copy(dt)
  rotation <- rotation[,.(year,B_LU_BRP,B_LU_NAME,B_LU_EOM,B_LU_EOM_RESIDUE,B_LU_HC)]

  # set categories
  rotation[, gld := fifelse(grepl('gras',B_LU_NAME),1,0)]
  rotation[, cereal := fifelse(grepl('gerst|tarwe|rogge|haver|granen',B_LU_NAME),1,0)]
  rotation[, nat := fifelse(grepl('bomen|struiken|heesters|contain|wijn|definitief|fauna|boomkwe|natuur|boomgr|scheerheg|hakhout|wandelp|landschaps|zandwal|boom|bufferstr|^rand',B_LU_NAME),1,0)]
  rotation[, bld := 1 - gld - cereal - nat]

  # set default green manure
  rotation[grepl('mais|aardappel',B_LU_NAME), M_GREEN_TIMING := 'october']
  rotation[is.na(M_GREEN_TIMING), M_GREEN_TIMING := 'never']

  # set default crop residue
  rotation[, M_CROPRESIDUE := FALSE]

  # Set amendment
  amendment <- dt[,.(year,cat,B_LU_BRP,B_LU_NAME)]
  amendment[, c('P_NAME','month') := list('cattle_slurry',month = 3)]
  amendment <- merge(amendment,dtcm,by.x = 'P_NAME',by.y = 'man_name',all.x = TRUE)
  amendment[cat == 1, P_DOSE := 63300]
  amendment[cat == 0, P_DOSE := 46700]
  amendment[grepl('uien|peen|witlof|graszaad|bieten, suiker-',B_LU_NAME), P_DOSE := 0]

  # scenario is BAUIMPR
  if(scen=='BAUIMPR'){

    # update timing cover crop
    rotation[, M_GREEN_TIMING := NA_character_]
    rotation[grepl('mais|aardappel',B_LU_NAME), M_GREEN_TIMING := 'september']
    rotation[grepl('gras',B_LU_NAME), M_GREEN_TIMING := 'never']
    rotation[is.na(M_GREEN_TIMING), M_GREEN_TIMING := 'october']

    # update manure by partly using compost
    amendment2 <- amendment[,.(year,B_LU_BRP,B_LU_NAME,cat,P_NAME,month)]
    amendment2[,c('P_NAME','month') := list('green_compost',month = 8)]
    amendment <- rbind(amendment[,.(year,B_LU_BRP,B_LU_NAME,cat,P_NAME,month)],amendment2)
    amendment <- merge(amendment,dtcm, by.x = 'P_NAME', by.y = 'man_name', all.x = TRUE)

    # Determine dosage of slurry and compost based on crop category
    amendment[cat == 1 & grepl('cattle',P_NAME), P_DOSE := round(95 * 0.95*1000*0.1/ p_p2o5)]
    amendment[cat == 0 & grepl('cattle',P_NAME), P_DOSE := round(70 * 0.95*1000*0.1/ p_p2o5)]
    amendment[cat == 1 & grepl('compost',P_NAME), P_DOSE := round(95 * 0.05*1000*2*0.1/ p_p2o5)]
    amendment[cat == 0 & grepl('compost',P_NAME), P_DOSE := round(70 * 0.05*1000*2*0.1/ p_p2o5)]

    amendment[grepl('uien|peen|witlof|graszaad|bieten, suiker-',B_LU_NAME) & grepl('cattle',P_NAME), P_DOSE := 0]
    amendment[grepl('uien|peen|witlof|graszaad|bieten, suiker-',B_LU_NAME) & grepl('compost',P_NAME), P_DOSE := round(70 * 0.3*1000*2 *.1/ p_p2o5)]


  }

  # only adaptation crop cultivation: permanent grassland or cereals, catch crops and crop residue use
  if(scen == 'CLT'){

    # adapt crop input
    if(luse == 'BLD'){B_LU_BRP <- rep(233,length(B_LU_BRP))}
    if(luse == 'GLD'){B_LU_BRP <- rep(265,length(B_LU_BRP))}

    # Set crop rotation
    rotation <- copy(dt)
    rotation <- rotation[,.(year,B_LU_NAME,B_LU_EOM,B_LU_EOM_RESIDUE,B_LU_HC)]

    # set default green manure
    rotation[grepl('mais|aardappel',B_LU_NAME), M_GREEN_TIMING := 'august']
    rotation[is.na(M_GREEN_TIMING), M_GREEN_TIMING := 'never']
    rotation <- rotation[,.(year,B_LU_NAME,B_LU_EOM,B_LU_EOM_RESIDUE,B_LU_HC,M_GREEN_TIMING)]

    # set default crop residue
    rotation[, M_CROPRESIDUE := TRUE]

    # Set amendment
    amendment <- dt[,.(year,cat,B_LU_BRP,B_LU_NAME)]
    amendment[, c('P_NAME','month') := list('cattle_slurry',month = 3)]
    amendment <- merge(amendment,dtcm,by.x = 'P_NAME',by.y = 'man_name',all.x = TRUE)
    amendment[cat == 1, P_DOSE := 63300]
    amendment[cat == 0, P_DOSE := 46700]
    amendment[grepl('uien|peen|witlof|graszaad|bieten, suiker-',B_LU_NAME), P_DOSE := 0]

  }

  if(scen=='ALL'){

    # set maximum cereal to 40% of the rotation
    fr_gras <- rotation[,sum(gld)/.N]
    fr_cereal <- rotation[,sum(cereal)/.N]
    fr_bld <- rotation[,sum(bld)/.N]

    # adapt bouwland
    if(luse=='BLD' & fr_cereal < 0.4){

      nextra <- max(0,0.4 - fr_cereal)

      rot1 <- rotation[gld==0 & cereal == 0][sample(1:.N,ceiling(nextra * .N)),B_LU_BRP := 233]
      rot2 <- rotation[!(gld==0 & cereal == 0)]
      rotation <- rbind(rot1,rot2)
    }

    # increase aandeel gras / leeftijd gras
    if(luse=='GLD' & fr_bld > 0.3){

      nextra <- max(0,0.5 * fr_bld)

      rot1 <- rotation[bld==1][sample(1:.N,ceiling(nextra * .N)),B_LU_BRP := 265]
      rot2 <- rotation[!bld==1]
      rotation <- rbind(rot1,rot2)
    }

    # update properties
    rotation <- merge(rotation[,.(year,B_LU_BRP)],
                      BLN::bln_crops,by.x = 'B_LU_BRP',
                      by.y='crop_code',all.x=TRUE)
    rotation[, gld := fifelse(grepl('gras',B_LU_NAME),1,0)]
    rotation[, cereal := fifelse(grepl('gerst|tarwe|rogge|haver|granen',B_LU_NAME),1,0)]
    rotation[, nat := fifelse(grepl('bomen|struiken|heesters|contain|wijn|definitief|fauna|boomkwe|natuur|boomgr|scheerheg|hakhout|wandelp|landschaps|zandwal|boom|bufferstr|^rand',B_LU_NAME),1,0)]
    rotation[, bld := 1 - gld - cereal - nat]

    # update catch crop
    rotation[grepl('mais|aardappel',B_LU_NAME), M_GREEN_TIMING := 'september']
    rotation[grepl('gras',B_LU_NAME), M_GREEN_TIMING := 'never']
    rotation[is.na(M_GREEN_TIMING), M_GREEN_TIMING := 'october']

    # update crop residue input
    rotation[gld == 0 | nat == 0,M_CROPRESIDUE := TRUE]
    rotation[is.na(M_CROPRESIDUE), M_CROPRESIDUE := FALSE]

    # update manure by partly using compost
    amendment2 <- amendment[,.(year,B_LU_BRP,B_LU_NAME,cat,P_NAME,month)]
    amendment2[,c('P_NAME','month') := list('green_compost',month = 8)]
    amendment <- rbind(amendment[,.(year,B_LU_BRP,B_LU_NAME,cat,P_NAME,month)],amendment2)
    amendment <- merge(amendment,dtcm, by.x = 'P_NAME', by.y = 'man_name', all.x = TRUE)

    # Determine dosage of slurry and compost based on crop category
    amendment[cat == 1 & grepl('cattle',P_NAME), P_DOSE := round(95 * 0.95*1000*0.1/ p_p2o5)]
    amendment[cat == 0 & grepl('cattle',P_NAME), P_DOSE := round(70 * 0.95*1000*0.1/ p_p2o5)]
    amendment[cat == 1 & grepl('compost',P_NAME), P_DOSE := round(95 * 0.05*1000*2*0.1/ p_p2o5)]
    amendment[cat == 0 & grepl('compost',P_NAME), P_DOSE := round(70 * 0.05*1000*2*0.1/ p_p2o5)]

    amendment[grepl('uien|peen|witlof|graszaad|bieten, suiker-',B_LU_NAME) & grepl('cattle',P_NAME), P_DOSE := 0]
    amendment[grepl('uien|peen|witlof|graszaad|bieten, suiker-',B_LU_NAME) & grepl('compost',P_NAME), P_DOSE := round(70 * 0.3*1000*2 *.1/ p_p2o5)]


  }

  # subset final rotation
  rotation <- rotation[,.(year,B_LU_BRP,B_LU_EOM,B_LU_EOM_RESIDUE,B_LU_HC,M_GREEN_TIMING,M_CROPRESIDUE)]

  # add B_LU for Dutch situation
  rotation[, B_LU := paste0('nl_',B_LU_BRP)]

  # Remove unnecessary columns
  amendment[,c('cat','B_LU_BRP','B_LU_NAME') := NULL]
  rotation[,c('B_LU_BRP') := NULL]

  out <- list(rotation = rotation, amendment = amendment)
  return(out)
}


#' function to run RothC parallel for a series of fields
#' @param this.xs (numeric) selected id for a single field
#' @param dt.c (data.table) set will all fields
#' @param scen (character) scenarios to be simulated. Options include BAU, BAUIMPR, CLT and ALL.
#' @param simyears (integer) value for the amount of years to simulate, default is 50 years
#' @param p (progress bar) progress bar
#' @param final (boolean) option to select only the last year
#'
#' @export
rothc_parallel <- function(this.xs, dt.c, scen, simyears = 50,p = NULL,final = TRUE){

  # set visual binding
  xs = NULL

  # get simulation data
  sim.dt <- dt.c[xs == this.xs]

  mc <- sim.dt$mc[1]

  # do RothC simulation
  result <- tryCatch({

    # set seed
    set.seed(mc)

    # take average SOM and clay per field
    this.som <- mean(sim.dt$A_SOM_LOI)
    this.clay <- mean(sim.dt$A_CLAY_MI)

    # run RothC
    out <- bln_rothc_field(B_LU_BRP = sim.dt$B_LU_BRP,
                            A_SOM_LOI = this.som,
                            A_CLAY_MI =  this.clay,
                            simyears = simyears,
                            init = FALSE,
                            scen = scen,
                            spinup = 10)
    out[,xs := this.xs]

    # if final is true select only last prediction
    if(final){out <- out[year > max(year) - 10,lapply(.SD,mean)]}

    # show progress
    if (! is.null(p)) {if (this.xs %% 10 == 0) p(sprintf('id = %g', this.xs))}

    result <- copy(out)

    return(result)

  }, error = function (e) {


    if(final){
      result <-data.table(year = simyears,A_SOM_LOI_BAU = 0, A_SOM_LOI_ALL = 0,xs = this.xs)
      } else{
      result <- data.table(year = 1:simyears,A_SOM_LOI_BAU = 0, A_SOM_LOI_ALL = 0,xs = this.xs)
    }
    result$error <- as.character(e)

    if (! is.null(p)) {p(sprintf('id %g has error: %s', this.xs, as.character(e)))}

    return(result)
  })


  return(result)

}


#' Function to evaluate the carbon saturation via RothC simulation on multiple fields using multicore processing
#'
#' @param ID (character) A field id
#' @param B_LU_BRP (numeric) value of the BRP crop code
#' @param B_GWL_GLG (numeric) The lowest groundwater level averaged over the most dry periods in 8 years in cm below ground level
#' @param A_SOM_LOI (numeric) value for the soil organic matter content of the soil
#' @param A_CLAY_MI (numeric) value for the clay content of the soil
#' @param scen (character) scenarios to be simulated. Options include BAU, BAUIMPR,CLT and ALL.
#' @param simyears (integer) value for the amount of years to simulate, default is 50 years
#' @param quiet (boolean) showing progress bar for calculation RothC C-saturation for each field
#'
#' @import data.table
#' @import progressr
#' @import future.apply
#' @import future
#' @import parallelly
#'
#' @export
bln_rothc_multicore <- function(ID,B_LU_BRP,B_GWL_GLG,A_SOM_LOI,A_CLAY_MI, scen, simyears = 50, quiet = TRUE){

  # add visual bindings
  A_SOM_LOI_ALL = A_SOM_LOI_BAU = . = NULL

  # Check if relevant packes are installed
  if (system.file(package = 'future') == '') {stop('multicore processing requires future to be installed')}
  if (system.file(package = 'future.apply') == '') {stop('multicore processing requires the package future.apply to be installed')}
  if (system.file(package = 'parallelly') == '') {stop('multicore processing requires the package parallelly to be installed')}

  # Check inputs
  arg.length <- max(length(B_LU_BRP), length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(BLN::bln_crops$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0.1, upper = 100, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_numeric(B_GWL_GLG, lower = 0, any.missing = FALSE, len = arg.length)
  checkmate::assert_character(scen)
  checkmate::assert_subset(scen,choices = c('BAU','BAUIMPR','CLT','ALL'))
  checkmate::assert_numeric(simyears,len=1,lower=15)

  if(length(ID)>1){checkmate::assert_true(length(ID) == arg.length)}
  checkmate::assert_logical(quiet)

  #make internal table (set clay to 75 max given checkmate)
  dt.c <- data.table(ID = ID,
                     B_LU_BRP = B_LU_BRP,
                     B_GWL_GLG = B_GWL_GLG,
                     A_SOM_LOI = A_SOM_LOI,
                     A_CLAY_MI= pmin(75,A_CLAY_MI))

  # RothC
  simulation_time <- 50L

  # multithreading
  cm.versions <- c('CM4')

  # Run the simulations
  future::plan(future::multisession, workers = parallelly::availableCores()-1)

  # add seed
  dt.c$mc <- 111

  # add group
  dt.c[,xs := .GRP,by = ID]

  # run RothC function
  progressr::with_progress({
    xs <- sort(unique(dt.c$xs))
    if(quiet){p = NULL} else {p <- progressr::progressor(along = xs)}

    results <- future.apply::future_lapply(X = xs,
                                           FUN = BLN::rothc_parallel,
                                           dt.c = dt.c,
                                           p = p,
                                           scen = scen,
                                           simyears = simyears,
                                           future.seed = TRUE,
                                           future.packages = c('BLN'))
  })

  # close cluster
  future::plan(future::sequential)

  # combine output
  dt.res <- rbindlist(results, fill = TRUE)

  # columns to select
  mcols <- paste0('A_SOM_LOI_',scen)

  # merge with dt.c
  dt.c <- merge(dt.c,
                dt.res[,mget(c('xs',mcols))],
                by='xs',all.x=TRUE)

  # retreive unique value per ID
  dt.c <- dt.c[,lapply(.SD,mean),.SDcols = mcols,by=ID]

  # return
  return(dt.c)
}
