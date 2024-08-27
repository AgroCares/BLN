#' Function to evaluate the carbon saturation via RothC simulation
#'
#' @param ID (character) A field id
#' @param B_LU_BRP (numeric) value of the BRP crop code
#' @param B_GWL_GLG (numeric) The lowest groundwater level averaged over the most dry periods in 8 years in cm below ground level
#' @param A_SOM_LOI (numeric) value for the soil organic matter content of the soil
#' @param A_CLAY_MI (numeric) value for the clay content of the soil
#' @param quiet (boolean) showing progress bar for calculation RothC C-saturation for each field
#'
#' @import data.table
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#'
#' @export
bln_clim_rothc <- function(ID,B_LU_BRP,B_GWL_GLG,A_SOM_LOI,A_CLAY_MI,quiet = FALSE){

  # add visual bindings
  i_clim_rothc =A_SOM_LOI_ROTHC_BAU = A_SOM_LOI_ROTHC_ALL = NULL

  # Check inputs
  arg.length <- max(length(B_LU_BRP), length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(BLN::bln_crops$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0.1, upper = 100, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_numeric(B_GWL_GLG, lower = 0, any.missing = FALSE, len = arg.length)
  if(length(ID)>1){checkmate::assert_true(length(ID) == arg.length)}
  checkmate::assert_logical(quiet)

  # make internal table (set clay to 75 max given checkmate in carboncastr)
  dt <- data.table(ID = ID,
                   B_LU_BRP = B_LU_BRP,
                   B_GWL_GLG = B_GWL_GLG,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI= pmin(75,A_CLAY_MI))

  # add progress bar
  if(!quiet) {pb = txtProgressBar(min = 0, max = length(unique(dt$ID)), initial = 0)}

  # set initial settings
  stepi <- 0 ; out <- list()

  # run RothC to simulate current and potential SOC
  for(i in unique(ID)){

    stepi = stepi + 1
    this.brp <- dt[ID==i,B_LU_BRP]
    this.som <- dt[ID==i,A_SOM_LOI]
    this.clay <- dt[ID==i,A_CLAY_MI]
    this.glg <- dt[ID==i,B_GWL_GLG]

    # calculate decomposition rate via INITIATOR approach (de Vries et al., 2022)
    if(this.som[1]>20){

      # lowering surface due to oxidation (Beuving and van den Akker, 1996)
      fsmv <- stats::approxfun(x = seq(0,130,10),
                               y = c(0,0.008,0.017,0.026,0.038,0.051,0.074,0.098,
                                     0.122,0.145,0.169,0.192,0.216,0.239),
                               rule = 2)


      # estimate SOC decline based on groundwater level given an optimum where GLG is 45 cm below surface
      fr_smv <- pmin(1,(fsmv(this.glg[1])-fsmv(pmax(0,this.glg[1]-45)))/fsmv(this.glg[1]),na.rm=T)

      # bring distance to target in same output style as for rothc
      dt.prc <- data.table(A_SOM_LOI_BAU = this.som[1]*fr_smv,
                           A_SOM_LOI_ALL = this.som[1])


    } else {

      # run the RothC model for mineral soils
      dt.rothc <- bln_rothc_field(this.brp, this.som, this.clay)

      # take the mean of the last 10 years
      dt.prc <- dt.rothc[year > max(year) - 10,lapply(.SD,mean)]

    }


    # save in list
    out[[stepi]] <- data.table(ID=i,
                               A_SOM_LOI_ROTHC_BAU = dt.prc$A_SOM_LOI_BAU,
                               A_SOM_LOI_ROTHC_ALL = dt.prc$A_SOM_LOI_ALL)
    # show progressbar
    if(!quiet) {setTxtProgressBar(pb,stepi)}
  }

  # close progress bar
  if(!quiet) {close(pb)}

  # combine all sites in a data.table again
  dt.cs <- rbindlist(out)

  # calculate the distance to target
  dt.cs[,i_clim_rothc := pmin(1,A_SOM_LOI_ROTHC_BAU/A_SOM_LOI_ROTHC_ALL)]

  # merge with the internal dt
  dt <- merge(dt,dt.cs,by='ID',all.x=TRUE)

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
#' @param init (boolean) use internal analytical solution for initialisation from carboncastr
#' @param spinup (numeric) the spinup period that is used for initialisation model
#'
#' @import data.table
#' @import carboncastr
#'
#' @export
bln_rothc_field <- function(B_LU_BRP, A_SOM_LOI, A_CLAY_MI, simyears = 50, init = FALSE,spinup = 10){

  # initialize RothC with BAU for 150 years
  pool_fractions <- rothc_initialise(B_LU_BRP = B_LU_BRP,
                                     A_SOM_LOI = A_SOM_LOI[1],
                                     A_CLAY_MI = A_CLAY_MI[1])

  # run simulations for the desire scenarios
  sim <- list(); count <- 0

  # run the RothC model for two scenarios: BAU and ALL
  scen = c('BAU','ALL')

  # run the scenarios in a for loop
  for(i in scen){

    # add counter
    count <- count + 1

    # prepare input for the scenarios
    scen.inp <- rothc_scenario(B_LU_BRP = B_LU_BRP, scen = i)

    # retreive the rotation and amendment input data.tables
    rotation <- scen.inp$rotation
    amendment <- scen.inp$amendment

    # Run simulation
    result <- carboncastr::cc_rothc_sim(crops = rotation,
                                        A_SOM_LOI = A_SOM_LOI[1],
                                        A_CLAY_MI = A_CLAY_MI[1],
                                        A_DEPTH = 0.3,
                                        cf_yield = if(i=='BAU') {1} else {1.05},
                                        M_TILLAGE_SYSTEM = "CT",
                                        weather = NULL,
                                        rothc_amendment = amendment,
                                        rothc_parms = list(simyears = simyears + spinup,
                                                           c_fractions = pool_fractions,
                                                           initialize = init))
    # set startyear to zero
    result[,year := year - min(year)]

    # do a manual scaling for the spin-up period
    result[,A_SOM_LOI := A_SOM_LOI * A_SOM_LOI[1] / A_SOM_LOI[spinup]]

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
#' @import carboncastr
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
  parms <- list(simyears = 150,unit = 'psomperfraction')

  # Run initialization run for 30 years
  this.result <- carboncastr::cc_rothc_sim(crops = rotation,
                                           A_SOM_LOI = A_SOM_LOI,
                                           A_CLAY_MI = A_CLAY_MI,
                                           rothc_amendment = amendment,
                                           rothc_parms = parms)

  # take last two rotations
  this.result.fin <- this.result[year > max(year)-2*nrow(rotation),lapply(.SD,mean)]

  fractions <- this.result.fin[,.(fr_IOM = CIOM / (A_SOM_LOI),
                                  fr_DPM = CDPM / A_SOM_LOI,
                                  fr_RPM = CRPM / A_SOM_LOI,
                                  fr_BIO = CBIO / A_SOM_LOI)]
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
  P_OM = P_HC = P_p2o5 = P_DOSE = P_NAME = p_p2o5 = NULL

  # combine input data
  dt <- data.table(B_LU_BRP = B_LU_BRP,year = 1:length(B_LU_BRP))
  dt <- merge(dt,
              carboncastr::cc.crops[,.(B_LU_BRP,B_LU_NAME,B_LU_EOM,B_LU_EOM_RESIDUE,
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
  rotation <- merge(rotation,
                    carboncastr::cc.crops.blu[,.(B_LU,B_LU_BRP)],by='B_LU_BRP',all.x=TRUE)
  rotation <- rotation[,.(year,B_LU,B_LU_BRP,B_LU_NAME,B_LU_EOM,B_LU_EOM_RESIDUE,B_LU_HC)]

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
  amendment <- merge(amendment,
                     carboncastr::cc.manure[,list(man_name,P_OM,P_HC,p_p2o5)],
                     by.x = 'P_NAME',
                     by.y = 'man_name',
                     all.x = TRUE)
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
    amendment <- merge(amendment,
                       carboncastr::cc.manure[,list(man_name,P_OM,P_HC,p_p2o5)], by.x = 'P_NAME', by.y = 'man_name', all.x = TRUE)

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
    if(luse == 'GLD'){B_LU_BRP <- rep(233,length(B_LU_BRP))}
    if(luse == 'BLD'){B_LU_BRP <- rep(265,length(B_LU_BRP))}

    # Set crop rotation
    rotation <- copy(dt)
    rotation <- merge(rotation,
                      carboncastr::cc.crops.blu[,.(B_LU,B_LU_BRP)],by='B_LU_BRP',all.x=TRUE)
    rotation <- rotation[,.(year,B_LU,B_LU_NAME,B_LU_EOM,B_LU_EOM_RESIDUE,B_LU_HC)]

    # set default green manure
    rotation[grepl('mais|aardappel',B_LU_NAME), M_GREEN_TIMING := 'august']
    rotation[is.na(M_GREEN_TIMING), M_GREEN_TIMING := 'never']
    rotation <- rotation[,.(year,B_LU,B_LU_NAME,B_LU_EOM,B_LU_EOM_RESIDUE,B_LU_HC,M_GREEN_TIMING)]

    # set default crop residue
    rotation[, M_CROPRESIDUE := TRUE]

    # Set amendment
    amendment <- dt[,.(year,cat,B_LU_BRP,B_LU_NAME)]
    amendment[, c('P_NAME','month') := list('cattle_slurry',month = 3)]
    amendment <- merge(amendment,
                       carboncastr::cc.manure[,list(man_name,P_OM,P_HC,p_p2o5)],
                       by.x = 'P_NAME',
                       by.y = 'man_name',
                       all.x = TRUE)
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
    rotation <- merge(rotation[,.(B_LU_BRP,year)],
                      carboncastr::cc.crops.blu, by = "B_LU_BRP")
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
    amendment <- merge(amendment,
                       carboncastr::cc.manure[,list(man_name,P_OM,P_HC,p_p2o5)], by.x = 'P_NAME', by.y = 'man_name', all.x = TRUE)

    # Determine dosage of slurry and compost based on crop category
    amendment[cat == 1 & grepl('cattle',P_NAME), P_DOSE := round(95 * 0.95*1000*0.1/ p_p2o5)]
    amendment[cat == 0 & grepl('cattle',P_NAME), P_DOSE := round(70 * 0.95*1000*0.1/ p_p2o5)]
    amendment[cat == 1 & grepl('compost',P_NAME), P_DOSE := round(95 * 0.05*1000*2*0.1/ p_p2o5)]
    amendment[cat == 0 & grepl('compost',P_NAME), P_DOSE := round(70 * 0.05*1000*2*0.1/ p_p2o5)]

    amendment[grepl('uien|peen|witlof|graszaad|bieten, suiker-',B_LU_NAME) & grepl('cattle',P_NAME), P_DOSE := 0]
    amendment[grepl('uien|peen|witlof|graszaad|bieten, suiker-',B_LU_NAME) & grepl('compost',P_NAME), P_DOSE := round(70 * 0.3*1000*2 *.1/ p_p2o5)]


  }

  # subset final rotation
  rotation <- rotation[,.(year,B_LU,B_LU_EOM,B_LU_EOM_RESIDUE,B_LU_HC,M_GREEN_TIMING,M_CROPRESIDUE)]

  # Remove unnecessary columns
  amendment[,c('cat','B_LU_BRP','B_LU_NAME') := NULL]

  out <- list(rotation = rotation, amendment = amendment)
  return(out)
}


# function to run RothC parallel for a series of fields
rothc_parallel <- function(this.xs, dt.c, p = NULL, sdir = NULL){

  # # get simulation data
  # sim.dt <- dt.c[xs == this.xs]
  #
  # mc <- sim.dt$mc[1]
  #
  # # do RothC simulation
  # result <- tryCatch({
  #
  #   # set seed
  #   set.seed(mc)
  #
  #   # run RothC
  #   out <- bln_rothc_field(B_LU_BRP = sim.dt$B_LU_BRP,
  #                          A_SOM_LOI = sim.dt$A_SOM_LOI[1],
  #                          A_CLAY_MI = sim.dt$A_CLAY_MI[1],
  #                          simyears = 100,
  #                          init = FALSE,
  #                          scen = c('BAU','ALL'))
  #   out[,xs := this.xs]
  #
  #   # show progress
  #   if (! is.null(p)) {if (this.xs %% 10 == 0) p(sprintf('id = %g', this.xs))}
  #
  #   result <- copy(out)
  #
  #   return(result)
  #
  # }, error = function (e) {
  #
  #   result <- data.table(year = 1:50,BAU = 0, ALL = 0,xs = this.xs)
  #   result$error <- as.character(e)
  #
  #   if (! is.null(p)) {p(sprintf('id %g has error: %s', this.xs, as.character(e)))}
  #
  #   return(result)
  # })
  result = NULL

  return(result)


}
