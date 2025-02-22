#' Function to plot simple RothC simulation time series for a single field
#'
#' @param dt (data.table) data.table with results from BLN RothC simulation, calculated with bln_rothc_field
#'
#' @export
plot_bln_rothc_ts <- function(dt){

  # test plot BLN for a single field
  # dt <- bln_rothc_field(B_LU_BRP = c(3732,265,3732),
  #                       A_SOM_LOI = 4.5,
  #                       A_CLAY_MI = 15,
  #                       scen= c('BAU','ALL','CLT','BAUIMPR'))

  # checkmate on input file
  checkmate::assert_true('year' %in% colnames(dt))
  checkmate::assert_true(sum(grepl('ALL|CLT|BAU',colnames(dt))) > 0)

  # melt the data.table
  dt.melt <- melt(dt,id.vars='year',variable.name = 'scenario',value.name='A_SOM_LOI')

  # rename the scenarios for plotting
  dt.melt[grepl('ALL$',scenario), pscen := 'Klimaatvriendelijke landbouw']
  dt.melt[grepl('CLT$',scenario), pscen := 'Landgebruiksverandering']
  dt.melt[grepl('BAU$',scenario), pscen := 'Huidige landbouwpraktijk']
  dt.melt[grepl('BAUIMPR$',scenario), pscen := 'Betere bemesting']

  # maxima axis
  maxaxis <- round(max(dt.melt$A_SOM_LOI)+0.5)
  minaxis <- round(min(dt.melt$A_SOM_LOI)-0.5)

  # make plot with time series
  p1 <- ggplot(data=dt.melt,aes(x=year,y=A_SOM_LOI,group=pscen,color=pscen)) +
        geom_line(linewidth = 0.5) + theme_bw() + ylim(minaxis,maxaxis)+
        scale_color_manual(name = 'Scenario',
                           values = c('Klimaatvriendelijke landbouw'='black',
                                      'Landgebruiksverandering' = '#56B4E9',
                                      'Huidige landbouwpraktijk' ='#E69F00',
                                      'Betere bemesting' ='#009E73'))+
        theme(legend.position = 'inside',legend.position.inside = c(0.3,0.8)) +
        ylab('OS-gehalte (%)') + xlab('jaar')

  # return plot
  return(p1)
}

#' Plot a BLN score or indicator on a map
#'
#' @param dt.scores (data.table) data.table with results from BLN calculations. Includes a spatial geometry.
#' @param dt.region (data.table) data.table with a spatial geometry for a given region to be plot on top of the fields. Optional.
#' @param parm (character) one of the BLN indicator or scores to be plotted
#' @param pbreak (vector) a vector to categorize the input in classes. Default is NULL.
#' @param blabel (vector) a vector with label names used for the legend. Default is NULL.
#' @param ptitle (character) a title to be added to the figure. Default is NULL.
#' @param psubtitle (character) a subtitle to be added to the figure. Default is NULL.
#' @param rev (boolean) a boolean argument to reverse the color mapping. Default is FALSE.
#' @param lpos (vector) a two dimensional vector to position the legend inside the plot. Default c(0.2,0.8).
#'
#' @import sf
#' @import ggplot2
#'
#' @export
plot_bln_map <- function(dt.scores, dt.region = NULL, parm, pbreak = NULL, plabel = NULL,
                           ptitle = NULL, psubtitle = NULL, rev = FALSE,lpos= c(0.2,0.8)){

  # set breaks
  if(is.null(pbreak)){pbreak <- c(-1, 0.5, 0.7, 0.8, 10)}
  if(is.null(plabel)){plabel <- c('<5','5-7','7-8','>8')}

  # checkmate
  checkmate::assert_true(grepl('geom',colnames(dt.scores)))
  if(!is.null(dt.region)){checkmate::assert_true(grepl('geom',colnames(dt.region)))}

  # select the variable and convert into class
  setDT(dt.scores)
  dt.sel <- dt.scores[, .(var = get(parm), geom)]
  dt.sel <- dt.sel[!is.na(var), vclas := cut(var, breaks = pbreak, label = plabel)]

  # add figure title and title of the legend
  if(is.null(ptitle)){ptitle <- paste0('Bodemkwaliteit')}
  if(grepl('^A_|^D_|^B_', parm)){lname <- 'Klasse'} else { lname <- 'Scoreklasse'}

  # convert to spatial object
  dt.sel <- sf::st_as_sf(dt.sel)

  # plot the map
  pp1 <- ggplot() +
        geom_sf(data = dt.sel, aes(fill = cut(var, pbreak,labels = plabel),
                                   color = cut(var, pbreak,labels = plabel))) +
        geom_sf(data = dt.region, color='black', fill = NA) +
        scale_color_viridis_d(name = lname, end = 1, na.translate = F, direction = if(rev){-1} else {1}, limits = plabel) +
        scale_fill_viridis_d(name = lname, end = 1, na.translate = F, direction = if(rev){-1} else {1}, limits = plabel) +
        theme_bw() +
        labs(title = ptitle,subtitle = psubtitle) +
        coord_sf(crs = 28992) +
        theme(legend.position = 'inside',
              legend.position.inside = lpos,
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 10))

  # return figure
  return(pp1)
}


#' Plot boxplots for BLN indicators and scores for a group of fields
#'
#' @param dt (data.table) data.table with results from BLN calculations.
#' @param pesd (vector) a vector to select all indicators per ESD to be plotted. Options: crop, water, carbon, nutrient, or scores
#' @param plegend (boolean) a boolean argument to show legend. Default is TRUE
#' @param ptitle (character) a title to be added to the figure. Default is NULL.
#' @param psubtitle (character) a subtitle to be added to the figure. Default is NULL.
#' @param pgroup (character) a variable to facetwrap the boxplot
#' @param lpos (vector) a two dimensional vector to position the legend inside the plot. Default c(0.2,0.8).
#' @param ptype (character) a figure type. options: scores, knelpunten
#'
#' @export
plot_bln_boxplot <- function(dt, pesd = NULL, ptitle = NULL, psubtitle = NULL,
                             plegend = TRUE,pgroup = NULL, lpos= c(0.2,0.8),ptype='scores'){

  # reformat the BLN input data.table (with all indicators and scores as columns)

  # set all column names to lower case
  setnames(dt,tolower(colnames(dt)))

  # checkmate
  checkmate::assert_true('id' %in% colnames(dt))
  checkmate::assert_true(sum(grepl('^i_|^s_',colnames(dt)))>45)

  # make internal copy
  dtp <- copy(dt)

  # make one filter search option
  pesd <- paste0(pesd,collapse = '_')

  # select only one year
  if('year' %in% colnames(dt)){dtp <- dt[year == max(year)]}
  if('jaar' %in% colnames(dt)){dtp <- dt[jaar == max(jaar)]}

  # add soil type if present
  if('b_soiltype_agr' %in% colnames(dt)){
    dtp[gsub("dekzand|duinzand|dalgrond|^zand",b_soiltype_agr),soiltype :=  'zand']
    dtp[gsub("rivierklei|zeeklei|maasklei",b_soiltype_agr),soiltype :=  'klei']
    dtp[gsub("moerige_klei",b_soiltype_agr),soiltype :=  'veen']
  }

  # select columns for melting
  idcols <- grep("soiltype|b_lu_cultcat4|id|area", names(dtp), value = TRUE)
  mcols <- grep("^i_|^s_", names(dtp), value = TRUE)

  # melt the database
  mdt <- melt(dtp, id.vars = idcols, measure.vars = mcols)

  # make labels for figure
  indicator_labels <- c(

    # production
    'i_c_n' = "N", 'i_c_p' = 'P','i_c_k' = 'K', 'i_c_mg' = 'Mg','i_c_s' = 'S','i_c_ph' = 'pH',
    'i_c_cec' = 'CEC', 'i_c_cu' = 'Cu','i_c_zn' = 'Zn',
    'i_p_cr' = 'verkruimelbaarheid','i_p_se' = 'verslemping',
    'i_p_ws' = 'waterstres', 'i_p_ds' = 'droogtestres',
    'i_p_du' = 'verstuifingsrisico', 'i_p_ro' = 'bewortelbaarheid',
    'i_p_co' = 'verdichtingrisico', 'i_p_whc' = 'waterberging',
    'i_p_as' = 'aggregaatstabiliteit','i_p_wo' = 'bodem bewerkbaarheid',
    'i_b_di' = 'ziektewerendheid','i_b_sf' = 'activiteit bodemleven',

    # waterkwaliteit en waterkwantiteit grondwater
    "i_gw_gwr" = 'grondwater aanvulling',"i_gw_ngw" = "N buffering gw",
    "i_gw_nlea" = "N uitspoelingsrisico", "i_gw_nret" = "N retentie gw",
    "i_gw_pest" = "pesticide water","i_gw_wb" = "water retentie",

    # waterkwaliteit oppervlaktewater
    "i_sw_nret" = "N retentie sw","i_sw_nro" = "N afspoelingsrisico",
    "i_sw_nsw" = 'N buffering sw',"i_sw_psw" = 'P buffering sw',

    # nutrient recycling
    "i_nut_nue" = "NUE","i_nut_n" = "N kringloop",
    "i_nut_p" = "P kringloop","i_nut_k" = "K kringloop",

    # carbon sequestration
    "i_clim_csat" = "C-verzadigingsgraad","i_clim_osb" = "OS-balans",
    "i_clim_rothc" = "C vastlegging potentie","i_clim_somers" = "C vastlegging potentie veen",

    # subscores scores
    "s_bln_nut" = "BLN Kringlopen", "s_bln_clim" = "BLN Koolstof",
    "s_bln_sw_quality" = "BLN waterkwantiteit sw", "s_bln_gw_quality" = "BLN waterkwaliteit gw",
    "s_bln_gw_quantity" = "BLN waterkwantiteit gw",

    "s_bln_prod_b"   = 'OBI biologie',"s_bln_prod_c" ='OBI nutrienten',"s_bln_prod_p" = "OBI bodemstructuur",

    # overall score
    "s_bln_esd_nut" = "ESD Kringlopen","s_bln_esd_clim" = "ESD Koolstof",
    "s_bln_esd_water" = "ESD water","s_bln_esd_prod" = "ESD gewasproductie",
    "s_bln_total" = "BLN totaalscore"

  )

  # add ESD category
  mdt[grepl("^i_[cpb]", variable), ess_cat := "ESD productie"]
  mdt[grepl("^i_sw|^i_gw", variable), ess_cat := "ESD water"]
  mdt[grepl("^i_nut", variable), ess_cat := "ESD kringlopen"]
  mdt[grepl("^i_clim", variable), ess_cat := "ESD koolstof"]
  mdt[grepl("^s_[a-z]", variable), ess_cat := "ESD score"]
  mdt[, ess_cat := factor(ess_cat, levels = c("ESD productie", "ESD water","ESD kringlopen","ESD koolstof","ESD score"))]

  # define ess_cat colours
  ess_cat_colours <- c("ESD productie" = "#2CA02C","ESD water" = "#0072B2",
                       "ESD kringlopen" = "#FFD700","ESD koolstof" = "#bb8832",
                       "ESD score" = "#FFC0CB","NA" = "#AAAAAA")

  # setorder(mdt, figorder)
  mdt[, variable := factor(variable, levels = names(indicator_labels))]

  # make column where aggregated scores are in same group as individual indicators
  mdt[!ess_cat == 'ESD score', ess_cat_grouped := ess_cat]
  mdt[grepl('s_bln_prod|s_bln_esd_prod',variable), ess_cat_grouped := "Productie"]
  mdt[grepl('s_bln_wat|s_bln_esd_wat',variable), ess_cat_grouped := "Water"]
  mdt[grepl('s_bln_clim|s_bln_esd_clim',variable), ess_cat_grouped := "Koolstof"]
  mdt[grepl('s_bln_nut|s_bln_esd_nut',variable), ess_cat_grouped := "Kringlopen"]
  mdt[ess_cat == 'ESD score', ess_cat_grouped := ess_cat]

  # filter the database on only requested ecosystem service categories
  if(grepl('crop|gewas|landbouw',pesd)){dtp1 <- mdt[ess_cat_grouped=='ESD productie']} else {dtp1 <- NULL}
  if(grepl('carbon|koolstof|klimaat|clim',pesd)){dtp2 <- mdt[grepl('kool',ess_cat_grouped)]} else {dtp2 <- NULL}
  if(grepl('water',pesd)){dtp3 <- mdt[grepl('wat',ess_cat_grouped)]} else {dtp3 <- NULL}
  if(grepl('nue|kringloop|kringlopen|nutrient',pesd)){dtp4 <- mdt[grepl('kring',ess_cat_grouped)]} else {dtp4 <- NULL}
  if(grepl('score',pesd)){dtp5 <- mdt[grepl('score',ess_cat_grouped)]} else {dtp5 <- NULL}
  pdt <- rbind(dtp1,dtp2,dtp3,dpt4,dtp5)

  # remove all mising values
  pdt <- pdt[!is.na(value)]

  scaleFUN <- function(x) sprintf("%.1f", x)

  # plot BLNs cores for a selection of fields
  if(ptype=='scores'){

    p1 <- ggplot(data = pdt,mapping = aes(x = value*10, y = variable, fill = ess_cat)) +
      geom_boxplot(color = '#A5A5A5', outlier.size = 0.3, outlier.alpha = 0.5, outlier.shape = 1,show.legend = plegend) +
      scale_y_discrete(labels = indicator_labels) +
      scale_x_continuous(labels=scaleFUN,limits = c(0,10)) +
      # scale_fill_manual(values = c('#2CA02C', '#0072B2', '#FFD700', '#bb8832', "#FFC0CB","#AAAAAA")) +
      scale_fill_manual(values = ess_cat_colours) +
      geom_vline(xintercept = 7,colour='black',linetype = 3)+
      ylab('') +
      xlab('Score') +
      guides(fill = guide_legend(title = 'Ecosysteemdienst')) +
      theme_bw() + theme(panel.grid.major.y = element_blank()) +
      theme(panel.grid.major.x = element_blank()) +
      theme(panel.grid.minor.x = element_blank())+
      theme(text = element_text(colour = "black"))+       #
      theme(axis.text = element_text(colour = "black", size = 9)) +
      theme(axis.ticks = element_line())
    if(!is.null(pgroup)){p1 <- p1 + facet_wrap(.~pgroup)}
    if(!is.null(ptitle)){p1 <- p1 + ggtitle(ptitle)}
  }

  # plot number of BLN bottlenecks
  if(ptype=='bottlenecks'){

    # aantal knelpunten
    pdt.kp <- mdt[grepl('^i_',variable)]
    pdt.kp <- pdt.kp[!is.na(value)]

    # check groupby factor
    if(is.null(pgroup)){pdt.kp[,group := 'all']}

    # count number of bottlenecks with a fixed threshold of 0.7
    pdt.kp <- pdt.kp[,list(knelpunt = sum(value <= 0.7)*100/length(value)),by= c('id','ess_cat','group')]

    p1 <- ggplot(data = pdt.kp,
                 mapping = aes(x = ess_cat, y = knelpunt, fill = ess_cat)) +
          geom_boxplot(color = '#A5A5A5', outlier.size = 0.3, outlier.alpha = 0.5, outlier.shape = 1,show.legend = TRUE)+
          scale_x_discrete(labels = indicator_labels) +
          scale_y_continuous(limits = c(0,100)) +
          scale_fill_manual(name ='ESD categorie',values = ess_cat_colours) + theme_bw()+
          ylab('Aantal knelpunten per ESD (%)') +
          xlab('')+theme(legend.position = 'inside',legend.position.inside = lpos)

    if(!is.null(ptitle)){p1 <- p1 + ggtitle(ptitle)} else {p1 <- p1 +  ggtitle('Aantal knelpunten per ESD (%)') }

  }

  # return figure
  return(p1)
}



