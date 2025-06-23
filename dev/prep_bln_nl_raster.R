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
s1.sel <- st_read(paste0(nmi.dat, 'landgebruik/brp/products/brpgewaspercelen_',2021,'.gpkg'))

# take a sample of 100.000 fields
a <- sample(1:nrow(s1.sel),100000)
s1.sel <- s1.sel[a,]

# add unique id
s1.sel$id <- 1:nrow(s1.sel)

# Load BLN input data for spatial points object ----------------------------------------------------


require(terra)
r <- terra::rast(paste0(nmi.dat, 'watersysteem/Grondwaterniveau/raw/LHM GHG_2011-2018_L1.tif'))
r <- aggregate(r, fact=4)

# load in the data points for which the BLN data need to be selected. Only ID and coordinates
sf.sel <- s1.sel[,c('id','geom')]

# convert to centroid and spatial point for speed reasons
sf.sel <- st_centroid(sf.sel)

# function to rasterize categorial or numeric variable
fc <- function(var,d,r) terra::rasterize(x = d,y = r,field=var,na.rm=T)
fn <- function(var,d,r) terra::rasterize(x = d,y = r,field=var,fun = mean,na.rm=T)
mfv <- function(x) names(sort(table(test),decreasing=T)[1])

# convert sampling points to vect
spv <- terra::vect(sf.sel)

# function to extract data
fex <- function(spv,s,r,varname,vn_new = NULL){

  # check variable type
  vars <- unlist(sapply(s[varname],is.numeric))
  vars <- vars[names(vars)[!grepl('geom',names(vars))]]

  # define length of the stack
  r2 <- c()

  # set counter
  count = 1

  # extract the value
  for(i in varname){

    # convert shape file to raster
    if(vars[i]==TRUE){r2 <- fn(var=i,d=s,r=r)} else {r2 <- fc(var=i,d=s,r=r)}

    # extract the value from the raster for sampling points (bilinear for shapes, when point also simple for numeric)
    #if(vars[i]==TRUE){e1 <- terra::extract(r2,spv,method='bilinear')} else {e1 <- terra::extract(r2,spv,method='simple')}
    e1 <- terra::extract(r2,spv,method='simple')

    # convert to data.table and take median per ID
    e1 <- as.data.table(e1)

    # adapt the names
    if(!is.null(vn_new)) {setnames(e1,i,vn_new[count])}

    # remove duplicated
    if(nrow(e1) != nrow(spv)){

      print(paste0('duplicates removed: ',e1[duplicated(ID),N],' samples'))
      e1 <- e1[!duplicated(ID)]
      #if(vars[i]==TRUE){e1 <- e1[,lapply(.SD,mean), by='ID']} else {e1 <- e1[!duplicated(ID)]}
    }

    # add to data.table
    if(count==1){dt <- copy(e1);dt[,id := spv$id]} else {dt <- merge(dt,e1,by='ID',all.x=TRUE)}

    # add counter
    count = count + 1

    # print
    print(paste0('parameter ',i,' has been extracted for ',nrow(e1),' sites'))
  }

  dt[,ID := NULL]
  setcolorder(dt,'id')

  print(paste0('dataset merged with input sf variables: ',paste(varname,collapse=', ')))
  rm(r2,e1)
  return(dt)
}

# load in landbouwgebied, do spatial join with the object
tmp1 <- sf::st_read(paste0(nmi.dat, 'topo/landbouwgebieden/raw/landbouwgebieden_2016/landbouwgebieden_2016.shp'))
r1 <- fex(spv = spv,s=tmp1,r=r,varname = 'statcode',vn_new = 'B_AER_CBS')

# load in bodemschat, do spatial join with the object
tmp1 <- sf::st_read(paste0(nmi.dat, 'bodem/bodemschat/products/BS6/BS6_2021.gpkg'))
cols <- colnames(tmp1)[grep('^A_',colnames(tmp1))]
r2 <- fex(spv = spv,s=tmp1,r=r,varname = 'ref_id',vn_new = NULL)
tmp1 <- as.data.table(tmp1)
tmp1[,geom := NULL]
r2 <- merge(r2,tmp1,by='ref_id')








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
dt.brp <- readBRP(2018:2022,sf.sel = sf.sel)

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
dt.out$B_LSW_ID <- as.character(dt.out$B_LSW_ID)
dt.out[is.na(B_LSW_ID),B_LSW_ID := 'lsw_nlmean']

bln_nl_test <- copy(dt.out)
saveRDS(bln_nl_test,'D:/OneDrive - SPRINGG/NMI-PROJ/BBWP/R_BBWPpaper/data/nl_bbwp_5000_fields.rds')


# prepare LSW datafile for calculations BLN

# shape file to extract data for
#s1.sel <- st_read('dev/bln_demarke.gpkg')

# add unique id
#s1.sel$fid <- s1.sel$id
#s1.sel$id <- 1:nrow(s1.sel)
#sf.sel <- s1.sel[,c('id','geom')]

require(sf);require(data.table)

# load in dataset
sf.sel <- readRDS('D:/OneDrive - SPRINGG/NMI-PROJ/BBWP/R_BBWPpaper/data/nl_bbwp_5000_fields.rds')
sf.sel <- st_as_sf(sf.sel[,.(id,B_LSW_ID,geom)])
sf.sel <- st_centroid(sf.sel)

# load in LSW and do subset
tmp1 <- st_read(paste0(nmi.dat, "watersysteem/Opgave_oppervlaktewater/products/20240625_oppervlaktewateropgave.gpkg"))
dt.lsw <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = st_intersects)
tmp1 <- tmp1[tmp1$oow_id %in% unique(dt.lsw$oow_id),]

# load in BS
tmp2 <- sf::st_read(paste0(nmi.dat, 'bodem/bodemschat/products/BS6/BS6_2021.gpkg'))
tmp2.sp <- tmp2[,c('ref_id','geom')]
tmp2.sp <- st_centroid(tmp2.sp)
tmp3 <- st_join(tmp2.sp,tmp1,largest = TRUE,join= st_intersects)
tmp3 <- tmp3[tmp3$oow_id %in% unique(dt.lsw$oow_id) & !is.na(tmp3$oow_id),]
tmp3 <- as.data.table(tmp3)
tmp4 <- merge(tmp3,as.data.table(tmp2),by='ref_id')
tmp4 <- tmp4[,list(B_SOM_LOI = mean(A_SOM_LOI),
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
tmp5 <- st_read(paste0(nmi.dat, 'nmi/runoff_risk/runoff_risk19_v2.gpkg'))
tmp5.sp <- tmp5[,c('ref_id','geom')]
tmp5.sp <- st_centroid(tmp5.sp)
tmp6 <- st_join(tmp5.sp,tmp1,largest = TRUE,join= st_intersects)
tmp6 <- tmp6[tmp6$oow_id %in% unique(dt.lsw$oow_id) & !is.na(tmp6$oow_id),]
tmp6 <- as.data.table(tmp6)
tmp6 <- merge(tmp6,as.data.table(tmp5),by='ref_id')
tmp6 <- tmp6[,list(B_RO_R = mean(D_RO_R,na.rm=T),
                   B_RO_R_SD = sd(D_RO_R,na.rm=T)),by='oow_id']

## load D_SA_W wet surrounding
tmp7 <- st_read(paste0(nmi.dat, 'landgebruik/brp/natte omtrek/2021/brp_2021_natteomtrek_zonder_2m.gpkg'))
tmp7.sp <- tmp7[,c('ref_id','geom')]
tmp7.sp <- st_centroid(tmp7.sp)
tmp8 <- st_join(tmp7.sp,tmp1,largest = TRUE,join= st_intersects)
tmp8 <- tmp8[tmp8$oow_id %in% unique(dt.lsw$oow_id) & !is.na(tmp8$oow_id),]
tmp8 <- as.data.table(tmp8)
tmp8 <- merge(tmp8,as.data.table(tmp7),by='ref_id')
tmp8 <- tmp8[,list(B_SA_W = mean(fr_natte_omtrek,na.rm=T),
                   B_SA_W_SD = sd(fr_natte_omtrek,na.rm=T)),by='oow_id']

# combine all LSW data
dt.lsw.extr <- merge(tmp4,tmp6,by='oow_id',all.x = TRUE)
dt.lsw.extr <- merge(dt.lsw.extr,tmp8,by='oow_id',all.x = TRUE)

# adapt name oow_id
setnames(dt.lsw.extr,'oow_id','B_LSW_ID')

# add averaged NL for missing ones
lsw.nl <- data.table(B_LSW_ID = 'lsw_nlmean', B_SOM_LOI = 8.65,B_CLAY_MI = 15.8,B_SAND_MI = 60.5,
                     B_SILT_MI = 23.71,B_N_RT = 3834,B_P_AL = 49,B_P_CC = 2.71,B_P_WA = 40,B_P_SG = 22,
                     B_FE_OX = 83,B_AL_OX = 40,B_RO_R = 0.5,B_SA_W = 0.47,B_SOM_LOI_SD = 6.67,B_CLAY_MI_SD = 13.45,
                     B_SAND_MI_SD = 23.5,B_SILT_MI_SD = 11.7,B_N_RT_SD = 2928,B_P_AL_SD = 13.5,B_P_CC_SD = 1.51,
                     B_P_WA_SD = 15.6,B_P_SG_SD = 14,B_FE_OX_SD = 59,B_AL_OX_SD = 19,B_RO_R_SD = 0.3,B_SA_W_SD = 0.33)
dt.lsw.extr <- rbind(dt.lsw.extr,lsw.nl)

# save measures as bbwp table
saveRDS(dt.lsw.extr,'D:/OneDrive - SPRINGG/NMI-PROJ/BBWP/R_BBWPpaper/data/nl_lsw_5000_fields.rds')






# Apply BLN input data for spatial points object ----------------------------------------------------

  # clear environment
  rm(list=ls())

  # require packages
  devtools::load_all()
  require(data.table)

  # load in prepared BLN data files
  dt.farm <- readRDS('D:/OneDrive - SPRINGG/NMI-PROJ/BBWP/R_BBWPpaper/data/nl_bbwp_5000_fields.rds')
  LSW <- readRDS('D:/OneDrive - SPRINGG/NMI-PROJ/BBWP/R_BBWPpaper/data/nl_lsw_5000_fields.rds')

  # run BLN for first 50 fields
  #dt.farm <- dt.farm[id < 51]
  LSW <- LSW[B_LSW_ID %in% dt.farm$B_LSW_ID]

  # fix errors in input
  dt.farm <- dt.farm[!is.na(B_SOILTYPE_AGR)]
  dt.farm <- na.omit(dt.farm)

  # run BLN
  d1 <- bln_field(ID = dt.farm$ref_id_2022,
                  B_LU_BRP = dt.farm$B_LU_BRP,
                  B_SC_WENR = dt.farm$B_SC_WENR,
                  B_GWL_CLASS = dt.farm$B_GWL_CLASS,
                  B_SOILTYPE_AGR = dt.farm$B_SOILTYPE_AGR,
                  B_HELP_WENR = dt.farm$B_HELP_WENR,
                  B_AER_CBS = dt.farm$B_AER_CBS,
                  B_GWL_GLG = dt.farm$B_GWL_GLG,
                  B_GWL_GHG = dt.farm$B_GWL_GHG,
                  B_GWL_ZCRIT = dt.farm$B_GWL_ZCRIT,
                  B_DRAIN = dt.farm$B_DRAIN,
                  B_FERT_NORM_FR = dt.farm$B_FERT_NORM_FR,
                  B_SLOPE_DEGREE = dt.farm$B_SLOPE_DEGREE,
                  B_GWP = dt.farm$B_GWP,
                  B_AREA_DROUGHT = dt.farm$B_AREA_DROUGHT,
                  B_CT_PSW = dt.farm$B_CT_PSW,
                  B_CT_NSW = dt.farm$B_CT_NSW,
                  B_CT_PSW_MAX =0.5,
                  B_CT_NSW_MAX = 5.0,
                  A_SOM_LOI = dt.farm$A_SOM_LOI,
                  A_SOM_LOI_MLMAX = dt.farm$a_som_loi_csat_top,
                  A_CLAY_MI = dt.farm$A_CLAY_MI,
                  A_SAND_MI = dt.farm$A_SAND_MI,
                  A_SILT_MI = dt.farm$A_SILT_MI,
                  A_DENSITY_SA = NA_real_,
                  A_FE_OX = dt.farm$A_FE_OX,
                  A_AL_OX = dt.farm$A_AL_OX,
                  A_PH_CC = dt.farm$A_PH_CC,
                  A_N_RT = dt.farm$A_N_RT,
                  A_CN_FR = dt.farm$A_CN_FR,
                  A_S_RT = dt.farm$A_S_RT,
                  A_N_PMN = dt.farm$A_N_PMN,
                  A_P_AL = dt.farm$A_P_AL,
                  A_P_CC = dt.farm$A_P_CC,
                  A_P_WA = dt.farm$A_P_WA,
                  A_P_SG = dt.farm$A_P_SG,
                  A_CEC_CO = dt.farm$A_CEC_CO,
                  A_CA_CO_PO = dt.farm$A_CA_CO_PO,
                  A_MG_CO_PO = dt.farm$A_MG_CO_PO,
                  A_K_CO_PO = dt.farm$A_K_CO_PO,
                  A_K_CC = dt.farm$A_K_CC,
                  A_MG_CC = dt.farm$A_MG_CC,
                  A_MN_CC = dt.farm$A_MN_CC,
                  A_ZN_CC = dt.farm$A_ZN_CC,
                  A_CU_CC = dt.farm$A_CU_CC,
                  A_EW_BCS = NA,A_SC_BCS = NA,A_GS_BCS = NA,A_P_BCS = NA,A_C_BCS = NA,
                  A_RT_BCS = NA,A_RD_BCS = NA,A_SS_BCS = NA,A_CC_BCS = NA,
                  D_SA_W = dt.farm$D_SA_W,
                  D_RO_R = dt.farm$D_RO_R,
                  M_COMPOST = NA_real_,M_GREEN = NA,M_NONBARE = NA,M_EARLYCROP = NA,
                  M_SLEEPHOSE = NA,M_DRAIN = NA,M_DITCH = NA,M_UNDERSEED = NA,
                  M_LIME = NA,M_NONINVTILL = NA,M_SSPM = NA,M_SOLIDMANURE = NA,
                  M_STRAWRESIDUE = NA,M_MECHWEEDS = NA,M_PESTICIDES_DST = NA,
                  B_LSW_ID = dt.farm$B_LSW_ID,LSW = LSW, output ='all',
                  runrothc = FALSE,
                  mc = TRUE)


  # check I_P_DU = NAN, I_GW_PEST
  # ID = dt.farm$ref_id_2022
  # B_LU_BRP = dt.farm$B_LU_BRP
  # B_SC_WENR = dt.farm$B_SC_WENR
  # B_GWL_CLASS = dt.farm$B_GWL_CLASS
  # B_SOILTYPE_AGR = dt.farm$B_SOILTYPE_AGR
  # B_HELP_WENR = dt.farm$B_HELP_WENR
  # B_AER_CBS = dt.farm$B_AER_CBS
  # B_GWL_GLG = dt.farm$B_GWL_GLG
  # B_GWL_GHG = dt.farm$B_GWL_GHG
  # B_GWL_ZCRIT = dt.farm$B_GWL_ZCRIT
  # B_DRAIN = dt.farm$B_DRAIN
  # B_FERT_NORM_FR = dt.farm$B_FERT_NORM_FR
  # B_SLOPE_DEGREE = dt.farm$B_SLOPE_DEGREE
  # B_GWP = dt.farm$B_GWP
  # B_AREA_DROUGHT = dt.farm$B_AREA_DROUGHT
  # B_CT_PSW = dt.farm$B_CT_PSW
  # B_CT_NSW = dt.farm$B_CT_NSW
  # B_CT_PSW_MAX =0.5
  # B_CT_NSW_MAX = 5.0
  # A_SOM_LOI = dt.farm$A_SOM_LOI
  # A_SOM_LOI_MLMAX = dt.farm$a_som_loi_csat_top
  # A_CLAY_MI = dt.farm$A_CLAY_MI
  # A_SAND_MI = dt.farm$A_SAND_MI
  # A_SILT_MI = dt.farm$A_SILT_MI
  # A_DENSITY_SA = NA_real_
  # A_FE_OX = dt.farm$A_FE_OX
  # A_AL_OX = dt.farm$A_AL_OX
  # A_PH_CC = dt.farm$A_PH_CC
  # A_N_RT = dt.farm$A_N_RT
  # A_CN_FR = dt.farm$A_CN_FR
  # A_S_RT = dt.farm$A_S_RT
  # A_N_PMN = dt.farm$A_N_PMN
  # A_P_AL = dt.farm$A_P_AL
  # A_P_CC = dt.farm$A_P_CC
  # A_P_WA = dt.farm$A_P_WA
  # A_P_SG = dt.farm$A_P_SG
  # A_CEC_CO = dt.farm$A_CEC_CO
  # A_CA_CO_PO = dt.farm$A_CA_CO_PO
  # A_MG_CO_PO = dt.farm$A_MG_CO_PO
  # A_K_CO_PO = dt.farm$A_K_CO_PO
  # A_K_CC = dt.farm$A_K_CC
  # A_MG_CC = dt.farm$A_MG_CC
  # A_MN_CC = dt.farm$A_MN_CC
  # A_ZN_CC = dt.farm$A_ZN_CC
  # A_CU_CC = dt.farm$A_CU_CC
  # A_EW_BCS = NA
  # A_SC_BCS = NA
  # A_GS_BCS = NA
  # A_P_BCS = NA
  # A_C_BCS = NA
  # A_RT_BCS = NA
  # A_RD_BCS = NA
  # A_SS_BCS = NA
  # A_CC_BCS = NA
  # D_SA_W = dt.farm$D_SA_W
  # D_RO_R = dt.farm$D_RO_R
  # M_COMPOST = NA_real_
  # M_GREEN = NA
  # M_NONBARE = NA
  # M_EARLYCROP = NA
  # M_SLEEPHOSE = NA
  # M_DRAIN = NA
  # M_DITCH = NA
  # M_UNDERSEED = NA
  # M_LIME = NA
  # M_NONINVTILL = NA
  # M_SSPM = NA
  # M_SOLIDMANURE = NA
  # M_STRAWRESIDUE = NA
  # M_MECHWEEDS = NA
  # M_PESTICIDES_DST = NA
  # B_LSW_ID = dt.farm$B_LSW_ID
  # output ='all'
  # runrothc = TRUE
  # mc = TRUE
  # i_clim_rothc = NA_real_
