# prepare example dataset for de Marke experimental farm with all properties for running BLN2 on field level

# clean memory
rm(list=ls())

# require packages
library(sf);library(data.table);library(pandex)

# set paths

nmi.dat <- Sys.getenv('NMI_DATA')
nmi.proj<- Sys.getenv('NMI-PROJ')
nmi.site<- Sys.getenv('NMI_SITE')

# shape file to extract data for
s1.sel <- st_read('dev/bln_demarke.gpkg')

# add unique id
s1.sel$fid <- s1.sel$id
s1.sel$id <- 1:nrow(s1.sel)

# Load BLN input data for spatial points object ----------------------------------------------------

# load in the data points for which the BLN data need to be selected. Only ID and coordinates
sf.sel <- s1.sel[,c('id','geom')]

# load in landbouwgebied, do spatial join with the object
tmp1 <- sf::st_read(paste0(nmi.dat, 'topo/landbouwgebieden/raw/landbouwgebieden_2016/landbouwgebieden_2016.shp'))
dt.aer <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = 'st_nearest_feature')
dt.aer <- as.data.table(dt.aer)
dt.aer <- dt.aer[,.(id,B_AER_CBS = statcode)]
print(paste0('dataset merged with landbouwgebied ',dt.aer[is.na(B_AER_CBS),length(unique(id))],' samples are missing'))

# load in bodemschat, do spatial join with the object
tmp1 <- sf::st_read(paste0(nmi.dat, 'bodem/bodemschat/products/BS6/BS6_2021.gpkg'))
dt.bs <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE)
dt.bs <- as.data.table(dt.bs)

# helper function to add a buffer aroudn point when extracting
extractwithbuffer <- function(dtte,spo,dt.sf,dbn = 'table_name',parm){

  # interne copy
  dtte.c <- copy(dtte)
  dtte.c[,checkparm := get(parm)]

  # set buffer size to extract
  buffersize = c(100, 500, 1000,5000)

  for(i in buffersize){

    # check missing ones
    check.mis <- dtte.c[is.na(checkparm),length(unique(id))] > 0

    # add check to add missing ones via buffer of 100m
    if(check.mis){

      # missing ids
      ids <- dtte.c[is.na(checkparm),unique(id)]

      # print warning
      print(paste0('dataset merged with ',dbn,' ',length(ids),' samples are missing. A buffer of ',i,'m will be applied and most nearby field properties selected'))

      # data.table with correct values
      dtte.cor <- dtte.c[!id %in% ids]

      # subset the spatial object for missing ones
      tmp2 <- spo[spo$id %in% ids,]
      tmp2 <- st_buffer(tmp2,i)

      # join with buffer one and convert back to points and data.table
      suppressWarnings(dtte.mis <-  st_join(tmp2,dt.sf,largest = TRUE, left = TRUE))
      suppressWarnings(dtte.mis <- st_cast(dtte.mis,'POINT'))
      dtte.mis <- as.data.table(dtte.mis)
      dtte.mis <- dtte.mis[!duplicated(id)]

      # correct names
      setnames(dtte.mis,
               old = c('A_OS_GV','A_KZK_MI','B_BT_AK','A_CN_OF'),
               new = c('A_SOM_LOI','A_CACO3_IF','B_SOILTYPE_AGR','A_CN_FR'), skip_absent = TRUE)
      if('sc.id' %in% colnames(dtte.mis)){dtte.mis[,sc.id := NULL]}

      dtte.mis[,checkparm := get(parm)]
      # combine with correct ones
      dtte.c <- rbind(dtte.cor,dtte.mis)
      setorder(dtte.c,id)
    }
  }

  print(paste0('dataset merged with ',dbn,' ',dtte.c[is.na(checkparm),length(unique(id))],' samples are missing.'))

  out <- copy(dtte.c)
  out[,checkparm := NULL]



  return(out)
}

# update BodemSchat
#dt.bs <- extractwithbuffer(dtte = dt.bs,spo = sf.sel,dt.sf = tmp1, dbn = 'BodemSchat',parm='A_SOM_LOI')
#rm(tmp1)

# load in bodembedrijf
tmp1 <- sf::st_read(paste0(nmi.dat, 'bodem/bodembedrijf/products/1/bodembedrijf_2019.gpkg'))
dt.bb <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = 'st_nearest_feature')
dt.bb <- as.data.table(dt.bb)
setnames(dt.bb,
         old = c('A_OS_GV','A_KZK_MI','B_BT_AK','A_CN_OF'),
         new = c('A_SOM_LOI','A_CACO3_IF','B_SOILTYPE_AGR','A_CN_FR'), skip_absent = TRUE)
#dt.bb <- extractwithbuffer(dtte = dt.bb,spo = sf.sel,dt.sf = tmp1, dbn = 'BodemBedrijf',parm='A_SOM_LOI')
rm(tmp1)

# load data from MOK
tmp1 <- sf::st_read(paste0(nmi.dat, 'maatregelen/Maatregelen-op-de-kaart/Fase2/Percelenkaart - Maatregel op de Kaart Fase 2.shp'))
tmp1 <- sf::st_transform(tmp1,28992)
dt.mok <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = 'st_nearest_feature')
dt.mok <- as.data.table(dt.mok)
#dt.mok <- extractwithbuffer(dtte = dt.mok,spo = sf.sel,dt.sf = tmp1, dbn = 'MOK',parm='bodem')
rm(tmp1)

# load HELP code for OBIC
tmp1 <- sf::st_read(paste0(nmi.dat, 'nmi/obi_helpcode/bodemtype_helpcode.gpkg'))
dt.help <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = 'st_nearest_feature')
dt.help <- as.data.table(dt.help)
print(paste0('dataset merged with HELP code ',dt.help[is.na(helpcode),length(unique(id))],' samples are missing'))
rm(tmp1)

# load soil compaction
tmp1 <- st_read(paste0(nmi.dat, 'bodem/alterra/ondergrondverdichting/B41_Risico_op_ondergrondverdichtingPolygon.shp'))
dt.sc <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = 'st_nearest_feature')
dt.sc <- as.data.table(dt.sc)
print(paste0('dataset merged with soil compaction ',dt.sc[is.na(VALUE),length(unique(id))],' samples are missing'))
rm(tmp1)

# load data bodemkaart
tmp1 <- st_read(paste0(nmi.dat, 'bodem/alterra/Bodemkaart50/products/bodemkaart50.gpkg'))
tmp1 <- sf::st_set_crs(tmp1,28992)
dt.bk <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = 'st_nearest_feature')
dt.bk <- as.data.table(dt.bk)
print(paste0('dataset merged with Bodemkaart50 ',dt.bk[is.na(bd50.hoofd),length(unique(id))],' samples are missing'))
rm(tmp1)

# load carbon saturation
tmp1 <- st_read(paste0(nmi.proj, 'Carbon_Saturation_Potential/results/agg_2019_84703f52aa91e09d.gpkg'))
tmp1 <- tmp1[,c('sc.id','a_som_loi_pred_mean_bau','a_som_loi_pred_mean_top','d_cs_bau','d_cs_top','geom')]
dt.cs <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = 'st_nearest_feature')
dt.cs <- as.data.table(dt.cs)
dt.cs[,sc.id := NULL]
#dt.cs <- extractwithbuffer(dtte = dt.cs,spo = sf.sel,dt.sf = tmp1, dbn = 'C saturation',parm='a_som_loi_pred_mean_bau')
rm(tmp1)

# Load the GWLdata from rasters LHM
require(terra)
r.gwl.ghg <- terra::rast(paste0(nmi.dat, 'watersysteem/Grondwaterniveau/raw/LHM GHG_2011-2018_L1.tif'))
r.gwl.glg <- terra::rast(paste0(nmi.dat, 'watersysteem/Grondwaterniveau/raw/LHM GLG_2011-2018_L1.tif'))
tmp1 <- c(r.gwl.ghg,r.gwl.glg)
vect.sel <- terra::vect(sf.sel)
dt.gwl <- terra::extract(tmp1,vect.sel,method='bilinear') # for shapes bilinear otherwis simple
dt.gwl <- as.data.table(dt.gwl)
dt.gwl <- dt.gwl[,lapply(.SD,mean),by='ID']
dt.gwl[,id := vect.sel$id]
setnames(dt.gwl,c('ID','B_GWL_GHG','B_GWL_GLG','id'))
print(paste0('dataset merged with GWL maps ',dt.gwl[is.na(B_GWL_GHG),length(unique(id))],' samples are missing'))
rm(tmp1)

# load Z crit (distance between groundwater table and root zone (30cm -mv) for delivering 2mm of water per day)
tmp1 <- st_read(paste0(nmi.dat, 'watersysteem/Grondwaterniveau/products/b_z_crit_two.gpkg'))
dt.zcrit <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = 'st_nearest_feature')
dt.zcrit <- as.data.table(dt.zcrit)
#dt.zcrit <- extractwithbuffer(dtte = dt.zcrit,spo = sf.sel,dt.sf = tmp1, dbn = 'zcrit',parm='B_Z_TWO')
rm(tmp1)

# load in ground water protection zone
tmp1 <- st_read(paste0(nmi.dat, "watersysteem/Grondwaterbeschermingsgebieden/raw/gwbg_nederland.gpkg"))
dt.gwpz <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = 'st_nearest_feature')
dt.gwpz <- as.data.table(dt.gwpz)
dt.gwpz[, B_GWP := fifelse(is.na(sid) & is.na(omschrijvi),FALSE,TRUE)]
print(paste0('dataset merged with GroundWater Protection Zone ',dt.gwpz[is.na(B_GWP),length(unique(id))],' samples are missing'))
rm(tmp1)

# load in LSW
tmp1 <- st_read(paste0(nmi.dat, "watersysteem/Opgave_oppervlaktewater/products/20240625_oppervlaktewateropgave.gpkg")) # 30213_oppervlaktewateropgave.gpkg"))
dt.lsw <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = st_intersects)
dt.lsw <- as.data.table(dt.lsw)
dt.lsw[,B_CT_PSW := fifelse(is.na(oow_phosphate),1, oow_phosphate)]
dt.lsw[,B_CT_NSW := fifelse(is.na(oow_nitrogen), 1, oow_nitrogen)]
print(paste0('dataset merged with LSW ',dt.lsw[is.na(B_CT_NSW),length(unique(id))],' samples are missing'))
rm(tmp1)

## load D_RO_R runoff risk
tmp1 <- st_read(paste0(nmi.dat, 'nmi/runoff_risk/runoff_risk19_v2.gpkg'))
dt.ro <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = st_nearest_feature)
dt.ro <- as.data.table(dt.ro)
#dt.ro <- extractwithbuffer(dtte = dt.ro,spo = sf.sel,dt.sf = tmp1, dbn = 'runof risks',parm='D_RO_R')
rm(tmp1)

## load D_SA_W wet surrounding
tmp1 <- st_read(paste0(nmi.dat, 'landgebruik/brp/natte omtrek/2021/brp_2021_natteomtrek_zonder_2m.gpkg'))
dt.saw <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = st_nearest_feature)
dt.saw <- as.data.table(dt.saw)
#dt.saw <- extractwithbuffer(dtte = dt.saw,spo = sf.sel,dt.sf = tmp1, dbn = 'wet surrounding',parm='fr_natte_omtrek')
rm(tmp1)

# a helper function to read the brp
readBRP <- function(years,sf.sel){

  syear <- sort(years,decreasing = TRUE)
  syear <- as.character(syear)

  for(i in syear){

    # read in the BRP
    tmp1 <- st_read(paste0(nmi.dat, 'landgebruik/brp/products/brpgewaspercelen_',i,'.gpkg'))
    tmp1 <- sf::st_set_crs(tmp1,28992)
    brp <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = st_nearest_feature)
    brp <- as.data.table(brp)

    # extract with buffer works with spatial points
    brp <- extractwithbuffer(dtte = brp,spo = sf.sel,dt.sf = tmp1, dbn = paste0('brp',i),parm='ref_id')

    # ensure that names are identical
    setnames(brp,old='gewascode',new='GWS_GEWASCODE',skip_absent = TRUE)

    # add and select correct columns
    brp[,c(i) := GWS_GEWASCODE]

    if(i == syear[1]){
      brp[,c(paste0('ref_id_',i)) := ref_id]
      cgeom <- if('geom' %in% colnames(brp)){'geom'} else {'geometry'}
      dt.brp <- brp[,mget(c('id',i,paste0('ref_id_',i),cgeom))]
    } else{
      dt.brp <- merge(dt.brp,brp[,mget(c('id',i))],by='id')
    }
  }

  # adapt the table in format
  cgeom <- if('geom' %in% colnames(dt.brp)){'geom'} else {'geometry'}
  dt.brp <- melt(dt.brp,
                 id.vars = c('id',paste0('ref_id_',syear[1]),cgeom),
                 variable.name = 'year',value.name = 'B_LU_BRP')
  dt.brp[,year := as.integer(as.character(year))]
  dt.brp[,B_LU_BRP := as.integer(B_LU_BRP)]
  setorder(dt.brp,id,-year)
  dt.brp[, B_LU_BRP := nafill(B_LU_BRP,type='locf'),by='id']
  dt.brp[, B_LU_BRP := nafill(B_LU_BRP,type='nocb'),by='id']

  return(dt.brp)
}

# read in all the BRP files
dt.brp <- readBRP(2012:2022,sf.sel = sf.sel)

# do this only for latest year
# tmp1 <- st_read(paste0(nmi.dat, 'landgebruik/brp/products/brpgewaspercelen_',2024,'_concept.gpkg'))
# brp <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = st_nearest_feature)
# brp <- as.data.table(brp)
# brp[is.na(gewascode),gewascode := 266]
# rm(tmp1)
# brp$B_LU_BRP <- brp$gewascode

# merge collected data and estimate derivates
dt.out <- copy(dt.brp)
ncols <- c('id', colnames(dt.bs)[grepl('^A_',colnames(dt.bs))])
dt.out <- merge(dt.out,dt.bs[,mget(ncols)],by='id',all.x = TRUE)
dt.out[, A_CA_CO_PO := A_CA_CO * 100 / A_CEC_CO]
dt.out[, A_MG_CO_PO := A_MG_CO * 100 / A_CEC_CO]
dt.out[, A_K_CO_PO := A_K_CO * 100 / A_CEC_CO]
ncols <- colnames(dt.bb)[grepl('^A_|^B_',colnames(dt.bb))]
ncols <- c('id',ncols[!ncols %in% colnames(dt.out)])
dt.out <- merge(dt.out,dt.bb[,mget(ncols)],by='id',all.x = TRUE)
dt.out <- merge(dt.out,
                dt.mok[,.(id,B_DRAIN = buisdrains,B_SLOPE_DEGREE = helling,own = ow_Nopg_lb,owp = ow_Popg_lb,gw = gw_status)],
                by= 'id',all.x=TRUE)
dt.out <- merge(dt.out,
                dt.aer,by='id',all.x=TRUE)
dt.out <- merge(dt.out,
                dt.help[,.(id,B_HELP_WENR = helpcode)],by='id',all.x=TRUE)
dt.out <- merge(dt.out,
                dt.sc[,.(id,B_SC_WENR = VALUE)], by= 'id',all.x=TRUE)
dt.out <- merge(dt.out,
                dt.bk[,.(id,B_GWL_CLASS = bd50.gwt.org)], by= 'id',all.x=TRUE)
dt.out <- merge(dt.out,
                dt.cs[,.(id,
                         a_som_loi_csat_bau = a_som_loi_pred_mean_bau,
                         a_som_loi_csat_top = a_som_loi_pred_mean_top,
                         d_cs_bau,d_cs_top)], by= 'id',all.x=TRUE)

dt.out <- merge(dt.out,
                dt.gwl[,.(id,B_GWL_GHG = B_GWL_GHG *100,B_GWL_GLG = B_GWL_GLG * 100)], by= 'id',all.x=TRUE)
dt.out <- merge(dt.out,
                dt.zcrit[,.(id,B_GWL_ZCRIT = B_Z_TWO)], by= 'id',all.x=TRUE)
dt.out <- merge(dt.out,
                dt.gwpz[,.(id,B_GWP)], by= 'id',all.x=TRUE)
dt.out <- merge(dt.out,
                dt.lsw[,.(id,B_CT_PSW, B_CT_NSW,B_LSW_ID = oow_id)], by= 'id',all.x=TRUE)
dt.out <- merge(dt.out,
                dt.ro[,.(id,D_RO_R)], by= 'id',all.x=TRUE)
dt.out <- merge(dt.out,
                dt.saw[,.(id,D_SA_W = fr_natte_omtrek)], by= 'id',all.x=TRUE)

# data conversions
dt.out[is.na(B_DRAIN)| B_DRAIN == 'nee', B_DRAIN := FALSE]
dt.out[B_DRAIN == 'ja', B_DRAIN := TRUE]
dt.out[,B_DRAIN := as.logical(B_DRAIN)]
dt.out[B_SLOPE_DEGREE > 30, B_SLOPE_DEGREE := 30]
dt.out[is.na(B_SLOPE_DEGREE), B_SLOPE_DEGREE := 0.1]
dt.out[,B_AREA_DROUGHT := TRUE]
dt.out[, B_FERT_NORM_FR := 1]
dt.out[,B_GWL_CLASS := OBIC::format_gwt(B_GWL_CLASS)]

# add crop categories
dt.out <- merge(dt.out,
                pandex::b_lu[!is.na(B_LU_BRP),.(B_LU_BRP, B_LU_WATERSTRESS_OBIC, B_LU_BBWP, B_LU_CULTCAT4, B_LU_SEASON)],
                by = 'B_LU_BRP',all.x=TRUE)

# ensure sum of most important occupation variables does not exceed 100
dt.out[A_CA_CO_PO + A_MG_CO_PO + A_K_CO_PO + A_NA_CO_PO > 100,
       c('A_CA_CO_PO', 'A_MG_CO_PO', 'A_K_CO_PO', 'A_NA_CO_PO') := .(
         A_CA_CO_PO * A_CA_CO_PO/(A_CA_CO_PO+A_MG_CO_PO+A_K_CO_PO+A_NA_CO_PO),
         A_MG_CO_PO * A_MG_CO_PO/(A_CA_CO_PO+A_MG_CO_PO+A_K_CO_PO+A_NA_CO_PO),
         A_K_CO_PO * A_K_CO_PO/(A_CA_CO_PO+A_MG_CO_PO+A_K_CO_PO+A_NA_CO_PO),
         A_NA_CO_PO * A_NA_CO_PO/(A_CA_CO_PO+A_MG_CO_PO+A_K_CO_PO+A_NA_CO_PO)
       )]

# estimate the orginal values
#dt.out <- dt.out[!is.na(ref_id)]
# dt.out$gewascode <- NULL
# dt.out$jaar <- NULL

# what columns are numeric
numeric_bb_cols <- colnames(dt.out[,.SD,.SDcols = is.numeric])
numeric_bb_cols <- numeric_bb_cols[!grepl('B_LU|B_LSW|id|year|^a_som|^d_cs',numeric_bb_cols)]

# set values below minimum to minumum and values above max to max
for(bb_param in numeric_bb_cols){
  dt.out[get(bb_param) > pandex::get_maxval(bb_param), c(bb_param) := pandex::get_maxval(bb_param)]
  dt.out[get(bb_param) < pandex::get_minval(bb_param), c(bb_param) := pandex::get_minval(bb_param)]
}

# add oow_nl for missing LSW
dt.out[is.na(B_LSW_ID),B_LSW_ID := 'lsw_nlmean']

bln_farm_hf <- copy(dt.out)

# save measures as bbwp table
usethis::use_data(bln_farm_hf, overwrite = TRUE)


# prepare LSW datafile for calculations BLN

# shape file to extract data for
s1.sel <- st_read('dev/bln_demarke.gpkg')

# add unique id
s1.sel$fid <- s1.sel$id
s1.sel$id <- 1:nrow(s1.sel)
sf.sel <- s1.sel[,c('id','geom')]

# load in LSW and do subset
tmp1 <- st_read(paste0(nmi.dat, "watersysteem/Opgave_oppervlaktewater/products/20240625_oppervlaktewateropgave.gpkg"))
dt.lsw <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = st_intersects)
tmp1 <- tmp1[tmp1$oow_id %in% unique(dt.lsw$oow_id),]

# load in BS
tmp2 <- sf::st_read(paste0(nmi.dat, 'bodem/bodemschat/products/BS6/BS6_2021.gpkg'))
tmp3 <- st_join(tmp2,tmp1,largest = TRUE)
tmp3 <- tmp3[tmp3$oow_id %in% unique(dt.lsw$oow_id) & !is.na(tmp3$oow_id),]
tmp3 <- as.data.table(tmp3)
tmp3 <- tmp3[,list(B_SOM_LOI = mean(A_SOM_LOI),
                   B_CLAY_MI = mean(A_CLAY_MI),
                   B_SAND_MI = mean(A_SAND_MI),
                   B_SILT_MI = mean(A_SILT_MI),
                   B_N_RT = mean(A_N_RT),
                   B_P_AL = mean(A_P_AL),
                   B_P_CC = mean(A_P_CC),
                   B_P_WA = mean(A_P_WA),
                   B_P_SG = mean(A_P_SG),
                   B_FE_OX = mean(A_FE_OX),
                   B_AL_OX = mean(A_AL_OX),
                   B_SOM_LOI_SD = sd(A_SOM_LOI),
                   B_CLAY_MI_SD = sd(A_CLAY_MI),
                   B_SAND_MI_SD = sd(A_SAND_MI),
                   B_SILT_MI_SD = sd(A_SILT_MI),
                   B_N_RT_SD = sd(A_N_RT),
                   B_P_AL_SD = sd(A_P_AL),
                   B_P_CC_SD = sd(A_P_CC),
                   B_P_WA_SD = sd(A_P_WA),
                   B_P_SG_SD = sd(A_P_SG),
                   B_FE_OX_SD = sd(A_FE_OX),
                   B_AL_OX_SD = sd(A_AL_OX)),by = 'oow_id']

## load D_RO_R runoff risk
tmp4 <- st_read(paste0(nmi.dat, 'nmi/runoff_risk/runoff_risk19_v2.gpkg'))
tmp4 <- st_join(tmp4,tmp1,largest = TRUE)
tmp4 <- tmp4[tmp4$oow_id %in% unique(dt.lsw$oow_id) & !is.na(tmp4$oow_id),]
tmp4 <- as.data.table(tmp4)
tmp4 <- tmp4[,list(B_RO_R = mean(D_RO_R,na.rm=T),
                   B_RO_R_SD = sd(D_RO_R,na.rm=T)),by='oow_id']

## load D_SA_W wet surrounding
tmp5 <- st_read(paste0(nmi.dat, 'landgebruik/brp/natte omtrek/2021/brp_2021_natteomtrek_zonder_2m.gpkg'))
tmp5 <- st_join(tmp5,tmp1,largest = TRUE)
tmp5 <- tmp5[tmp5$oow_id %in% unique(dt.lsw$oow_id) & !is.na(tmp5$oow_id),]
tmp5 <- as.data.table(tmp5)
tmp5 <- tmp5[,list(B_SA_W = mean(fr_natte_omtrek,na.rm=T),
                   B_SA_W_SD = sd(fr_natte_omtrek,na.rm=T)),by='oow_id']

# combine all LSW data
dt.lsw.extr <- merge(tmp3,tmp4,by='oow_id',all.x = TRUE)
dt.lsw.extr <- merge(dt.lsw.extr,tmp5,by='oow_id',all.x = TRUE)

# adapt name oow_id
setnames(dt.lsw.extr,'oow_id','B_LSW_ID')

# add averaged NL for missing ones
lsw.nl <- data.table(B_LSW_ID = 'lsw_nlmean', B_SOM_LOI = 8.65,B_CLAY_MI = 15.8,B_SAND_MI = 60.5,
                     B_SILT_MI = 23.71,B_N_RT = 3834,B_P_AL = 49,B_P_CC = 2.71,B_P_WA = 40,B_P_SG = 22,
                     B_FE_OX = 83,B_AL_OX = 40,B_RO_R = 0.5,B_SA_W = 0.47,B_SOM_LOI_SD = 6.67,B_CLAY_MI_SD = 13.45,
                     B_SAND_MI_SD = 23.5,B_SILT_MI_SD = 11.7,B_N_RT_SD = 2928,B_P_AL_SD = 13.5,B_P_CC_SD = 1.51,
                     B_P_WA_SD = 15.6,B_P_SG_SD = 14,B_FE_OX_SD = 59,B_AL_OX_SD = 19,B_RO_R_SD = 0.3,B_SA_W_SD = 0.33)
dt.lsw.extr <- rbind(dt.lsw.extr,lsw.nl)

# change ID into character
dt.lsw.extr[,B_LSW_ID := as.character(B_LSW_ID)]

# save LSW data
bln_lsw_farm_hf <- copy(dt.lsw.extr)

# save measures as bbwp table
usethis::use_data(bln_lsw_farm_hf, overwrite = TRUE)

