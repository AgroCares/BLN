# prepare example dataset for de Marke experimental farm with all properties for running BLN2 on field level

# clean memory
rm(list=ls())

# require packages
library(sf);library(data.table);library(pandex)

# set paths
nmi.dat <- Sys.getenv('NMI_DATA')
nmi.proj<- Sys.getenv('NMI-PROJ')
nmi.site<- Sys.getenv('NMI_SITE')

# -- helper functions ----

  # helper function to add a buffer aroudn point when extracting
  extractwithbuffer <- function(dtte,spo,dt.sf,dbn = 'table_name',parm, sbuffer = NULL){

    # interne copy
    dtte.c <- copy(dtte)
    dtte.c[,checkparm := get(parm)]

    # set buffer size to extract
    if(is.null(sbuffer)){buffersize = c(100, 500, 1000,5000)}

    # for loop for extraction
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



# -- step 1. prepare spatial file for data extraction ----

  # shape file to extract data for, and remove the bufferstrips and other fields that have no agricultural usage
  s1.sel <- st_read(paste0(nmi.dat, 'landgebruik/brp/products/brpgewaspercelen_',2024,'_concept.gpkg'))
  s1.sel <- s1.sel[s1.sel$is_gewasperceel==TRUE,]

  # add unique id
  s1.sel$id <- 1:nrow(s1.sel)

  # select only spatial geometry for which the BLN data need to be selected. Only ID and coordinates
  sf.sel <- s1.sel[,c('id','geom')]

  # convert to centroid and spatial point for speed reasons
  sf.sel <- st_centroid(sf.sel)

  # safe the spatial files as RDS files
  saveRDS(sf.sel,'D:/DATA/18 bln/brp24_sfsel.rds')
  saveRDS(s1.sel,'D:/DATA/18 bln/brp24_s1sel.rds')

# -- Load BLN input data for spatial points object ----------------------------------------------------

  # load in bodemschat, do spatial join with the object, save object
  tmp1 <- sf::st_read(paste0(nmi.dat, 'bodem/bodemschat/products/BS6/BS6_2021.gpkg'))
  dt.bs <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = 'st_nearest_feature')
  dt.bs <- as.data.table(dt.bs)
  dt.bs <- extractwithbuffer(dtte = dt.bs,spo = sf.sel,dt.sf = tmp1, dbn = 'BS',parm='A_SOM_LOI')
  saveRDS(dt.bs,'D:/DATA/18 bln/brp24_bs.rds')
  rm(tmp1);rm(dt.bs);gc()

  # load in BodemBedrijf
  tmp1 <- sf::st_read(paste0(nmi.dat, 'bodem/bodembedrijf/products/1/bodembedrijf_2019.gpkg'))
  dt.bb <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = 'st_nearest_feature')
  dt.bb <- as.data.table(dt.bb)
  setnames(dt.bb,
           old = c('A_OS_GV','A_KZK_MI','B_BT_AK','A_CN_OF'),
           new = c('A_SOM_LOI','A_CACO3_IF','B_SOILTYPE_AGR','A_CN_FR'), skip_absent = TRUE)
  dt.bb <- extractwithbuffer(dtte = dt.bb,spo = sf.sel,dt.sf = tmp1, dbn = 'BodemBedrijf',parm='A_SOM_LOI')
  saveRDS(dt.bb,'D:/DATA/18 bln/brp24_bb.rds')
  rm(tmp1,dt.bb);gc()

  # load data from MOK
  tmp1 <- sf::st_read(paste0(nmi.dat, 'maatregelen/Maatregelen-op-de-kaart/Fase2/Percelenkaart - Maatregel op de Kaart Fase 2.shp'))
  tmp1 <- sf::st_transform(tmp1,28992)
  dt.mok <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = 'st_nearest_feature')
  dt.mok <- as.data.table(dt.mok)
  dt.mok <- extractwithbuffer(dtte = dt.mok,spo = sf.sel,dt.sf = tmp1, dbn = 'MOK',parm='bodem')
  saveRDS(dt.mok,'D:/DATA/18 bln/brp24_mok.rds')
  rm(tmp1,dt.mok);gc()

  # load HELP code for OBIC
  tmp1 <- sf::st_read(paste0(nmi.dat, 'nmi/obi_helpcode/bodemtype_helpcode.gpkg'))
  dt.help <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = 'st_intersects')
  dt.help <- as.data.table(dt.help)
  dt.help <- extractwithbuffer(dtte = dt.help,spo = sf.sel,dt.sf = tmp1, dbn = 'HELP',parm='helpcode')
  saveRDS(dt.help,'D:/DATA/18 bln/brp24_help.rds')
  rm(tmp1,dt.help);gc()

  # load soil compaction
  tmp1 <- st_read(paste0(nmi.dat, 'bodem/alterra/ondergrondverdichting/B41_Risico_op_ondergrondverdichtingPolygon.shp'))
  dt.sc <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = 'st_nearest_feature')
  dt.sc <- as.data.table(dt.sc)
  dt.sc <- extractwithbuffer(dtte = dt.sc,spo = sf.sel,dt.sf = tmp1, dbn = 'verdichting',parm='VALUE')
  saveRDS(dt.sc,'D:/DATA/18 bln/brp24_sc.rds')
  rm(tmp1,dt.sc);gc()

  # load data bodemkaart
  tmp1 <- st_read(paste0(nmi.dat, 'bodem/alterra/Bodemkaart50/products/bodemkaart50.gpkg'))
  tmp1 <- sf::st_set_crs(tmp1,28992)
  dt.bk <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = 'st_nearest_feature')
  dt.bk <- as.data.table(dt.bk)
  dt.bk <- extractwithbuffer(dtte = dt.bk,spo = sf.sel,dt.sf = tmp1, dbn = 'Bodemkaart',parm='bd50.hoofd')
  saveRDS(dt.bk,'D:/DATA/18 bln/brp24_bk.rds')
  rm(tmp1,dt.bk);gc()

  # load carbon saturation
  tmp1 <- st_read(paste0(nmi.proj, 'Carbon_Saturation_Potential/results/agg_2019_84703f52aa91e09d.gpkg'))
  tmp1 <- tmp1[,c('sc.id','a_som_loi_pred_mean_bau','a_som_loi_pred_mean_top','d_cs_bau','d_cs_top','geom')]
  dt.cs <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = 'st_nearest_feature')
  dt.cs <- as.data.table(dt.cs)
  dt.cs[,sc.id := NULL]
  dt.cs <- extractwithbuffer(dtte = dt.cs,spo = sf.sel,dt.sf = tmp1, dbn = 'C saturation',parm='a_som_loi_pred_mean_bau')
  saveRDS(dt.cs,'D:/DATA/18 bln/brp24_cs.rds')
  rm(tmp1,dt.cs);gc()

  # load Z crit (distance between groundwater table and root zone (30cm -mv) for delivering 2mm of water per day)
  tmp1 <- st_read(paste0(nmi.dat, 'watersysteem/Grondwaterniveau/products/b_z_crit_two.gpkg'))
  dt.zcrit <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = 'st_nearest_feature')
  dt.zcrit <- as.data.table(dt.zcrit)
  dt.zcrit <- extractwithbuffer(dtte = dt.zcrit,spo = sf.sel,dt.sf = tmp1, dbn = 'zcrit',parm='B_Z_TWO')
  saveRDS(dt.zcrit,'D:/DATA/18 bln/brp24_zcrit.rds')
  rm(tmp1,dt.zcrit);gc()

  # load in ground water protection zone
  tmp1 <- st_read(paste0(nmi.dat, "watersysteem/Grondwaterbeschermingsgebieden/raw/gwbg_nederland.gpkg"))
  dt.gwpz <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = 'st_nearest_feature')
  dt.gwpz <- as.data.table(dt.gwpz)
  dt.gwpz[, B_GWP := fifelse(is.na(sid) & is.na(omschrijvi),FALSE,TRUE)]
  saveRDS(dt.gwpz,'D:/DATA/18 bln/brp24_gwpz.rds')
  rm(tmp1,dt.gwpz);gc()

  # load in LSW
  tmp1 <- st_read(paste0(nmi.dat, "watersysteem/Opgave_oppervlaktewater/products/20240625_oppervlaktewateropgave.gpkg")) # 30213_oppervlaktewateropgave.gpkg"))
  dt.lsw <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = st_intersects)
  dt.lsw <- as.data.table(dt.lsw)
  dt.lsw[,B_CT_PSW := fifelse(is.na(oow_phosphate),1, oow_phosphate)]
  dt.lsw[,B_CT_NSW := fifelse(is.na(oow_nitrogen), 1, oow_nitrogen)]
  saveRDS(dt.lsw,'D:/DATA/18 bln/brp24_lsw.rds')
  rm(tmp1,dt.lsw);gc()

  ## load D_RO_R runoff risk
  tmp1 <- st_read(paste0(nmi.dat, 'nmi/runoff_risk/runoff_risk19_v2.gpkg'))
  dt.ro <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = st_nearest_feature)
  dt.ro <- as.data.table(dt.ro)
  dt.ro <- extractwithbuffer(dtte = dt.ro,spo = sf.sel,dt.sf = tmp1, dbn = 'runof risks',parm='D_RO_R')
  saveRDS(dt.ro,'D:/DATA/18 bln/brp24_ro.rds')
  rm(tmp1,dt.ro);gc()

  ## load D_SA_W wet surrounding
  tmp1 <- st_read(paste0(nmi.dat, 'landgebruik/brp/natte omtrek/2021/brp_2021_natteomtrek_zonder_2m.gpkg'))
  dt.saw <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = st_nearest_feature)
  dt.saw <- as.data.table(dt.saw)
  dt.saw <- extractwithbuffer(dtte = dt.saw,spo = sf.sel,dt.sf = tmp1, dbn = 'wet surrounding',parm='fr_natte_omtrek')
  saveRDS(dt.saw,'D:/DATA/18 bln/brp24_saw.rds')
  rm(tmp1,dt.saw);gc()

  # load in landbouwgebied, do spatial join with the object
  tmp1 <- sf::st_read(paste0(nmi.dat, 'topo/landbouwgebieden/raw/landbouwgebieden_2016/landbouwgebieden_2016.shp'))
  dt.aer <- st_join(sf.sel,tmp1,largest = TRUE, left = TRUE, join = st_nearest_feature)
  dt.aer <- as.data.table(dt.aer)
  dt.aer <- extractwithbuffer(dtte = dt.aer,spo = sf.sel,dt.sf = tmp1, dbn = 'statcode',parm='B_AER_CBS')
  saveRDS(dt.aer,'D:/DATA/18 bln/brp24_aer.rds')

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
  saveRDS(dt.gwl,'D:/DATA/18 bln/brp24_gwl.rds')
  rm(tmp1,dt.gwl,r.gwl.ghg,r.gwl.glg,vect.sel);gc()




require(terra)
r <- terra::rast(paste0(nmi.dat, 'watersysteem/Grondwaterniveau/raw/LHM GHG_2011-2018_L1.tif'))
r <- aggregate(r, fact=4)

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
dt.aer <- fex(spv = spv,s=tmp1,r=r,varname = 'statcode',vn_new = 'B_AER_CBS')


# save files










# remove files
rm(dt.aer,dt.bb,dt.bk,dt.bs,dt.cs,dt.gwl,dt.gwpz,dt.help,dt.lsw,dt.mok,dt.ro,dt.saw,dt.sc,dt.zcrit)

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
dt.brp <- readBRP(2016:2020,sf.sel = sf.sel)
saveRDS(dt.brp,'D:/DATA/18 bln/brp21_brp1620.rds')
rm(dt.brp);gc()

# read in older BRP files
dt.brp.old <- readBRP(2011:2015,sf.sel = sf.sel)
saveRDS(dt.brp.old,'D:/DATA/18 bln/brp21_brp1115.rds')

# load previous prepared databases
dt.brp <- readRDS('D:/DATA/18 bln/brp21_brp1620.rds')
dt.brp.old <- readRDS('D:/DATA/18 bln/brp21_brp1115.rds')
setnames(dt.brp.old,old = 'ref_id_2015',new = 'ref_id')
setnames(dt.brp,old = 'ref_id_2020',new = 'ref_id')
dt.brp <- rbind(dt.brp,dt.brp.old)
rm(dt.brp.old)
dt.aer <- readRDS('D:/DATA/18 bln/brp21_aer.rds')
dt.bb <- readRDS('D:/DATA/18 bln/brp21_bb.rds')
dt.bk <- readRDS('D:/DATA/18 bln/brp21_bk.rds')
dt.bs <- readRDS('D:/DATA/18 bln/brp21_bs.rds')
dt.cs <- readRDS('D:/DATA/18 bln/brp21_cs.rds')
dt.gwl <- readRDS('D:/DATA/18 bln/brp21_gwl.rds')
dt.gwpz <- readRDS('D:/DATA/18 bln/brp21_gwpz.rds')
dt.help <- readRDS('D:/DATA/18 bln/brp21_help.rds')
dt.lsw <- readRDS('D:/DATA/18 bln/brp21_lsw.rds')
dt.mok <- readRDS('D:/DATA/18 bln/brp21_mok.rds')
dt.ro <- readRDS('D:/DATA/18 bln/brp21_ro.rds')
dt.saw <- readRDS('D:/DATA/18 bln/brp21_saw.rds')
dt.sc <- readRDS('D:/DATA/18 bln/brp21_sc.rds')
dt.zcrit <- readRDS('D:/DATA/18 bln/brp21_zcrit.rds')

# merge collected data and estimate derivates
dt.out <- copy(dt.brp)
ncols <- c('id', colnames(dt.bs)[grepl('^A_',colnames(dt.bs))])
dt.sf <- as.data.table(s1.sel)
dt.sf <- dt.sf[,.(id,ref_id_2021 = ref_id)]
dt.bs[,c('geom.x','geom.y') := NULL]
dt.bs <- merge(dt.bs,dt.sf,by.x='ref_id',by.y='ref_id_2021',all.x=TRUE)
dt.out <- merge(dt.out,dt.bs[,mget(ncols)],by='id',all.x = TRUE)
rm(dt.bs)
dt.out[, A_CA_CO_PO := A_CA_CO * 100 / A_CEC_CO]
dt.out[, A_MG_CO_PO := A_MG_CO * 100 / A_CEC_CO]
dt.out[, A_K_CO_PO := A_K_CO * 100 / A_CEC_CO]
ncols <- colnames(dt.bb)[grepl('^A_|^B_',colnames(dt.bb))]
ncols <- c('id',ncols[!ncols %in% colnames(dt.out)])
dt.out <- merge(dt.out,dt.bb[,mget(ncols)],by='id',all.x = TRUE)
rm(dt.bb)
dt.out <- merge(dt.out,
                dt.mok[,.(id,B_DRAIN = buisdrains,B_SLOPE_DEGREE = helling,own = ow_Nopg_lb,owp = ow_Popg_lb,gw = gw_status)],
                by= 'id',all.x=TRUE)
rm(dt.mok);gc()
dt.out <- merge(dt.out,
                dt.aer,by='id',all.x=TRUE)
rm(dt.aer);gc()
dt.out <- merge(dt.out,
                dt.help[,.(id,B_HELP_WENR = helpcode)],by='id',all.x=TRUE)
rm(dt.help);gc()
dt.out <- merge(dt.out,
                dt.sc[,.(id,B_SC_WENR = VALUE)], by= 'id',all.x=TRUE)
rm(dt.sc);gc()
dt.out <- merge(dt.out,
                dt.bk[,.(id,B_GWL_CLASS = bd50.gwt.org)], by= 'id',all.x=TRUE)
rm(dt.bk);gc()
dt.out <- merge(dt.out,
                dt.cs[,.(id,
                         a_som_loi_csat_bau = a_som_loi_pred_mean_bau,
                         a_som_loi_csat_top = a_som_loi_pred_mean_top,
                         d_cs_bau,d_cs_top)], by= 'id',all.x=TRUE)
rm(dt.cs);gc()
dt.out <- merge(dt.out,
                dt.gwl[,.(id,B_GWL_GHG = B_GWL_GHG *100,B_GWL_GLG = B_GWL_GLG * 100)], by= 'id',all.x=TRUE)
rm(dt.gwl);gc()
dt.out <- merge(dt.out,
                dt.zcrit[,.(id,B_GWL_ZCRIT = B_Z_TWO)], by= 'id',all.x=TRUE)
rm(dt.zcrit);gc()
dt.out <- merge(dt.out,
                dt.gwpz[,.(id,B_GWP)], by= 'id',all.x=TRUE)
rm(dt.gwpz);gc()
dt.out <- merge(dt.out,
                dt.lsw[,.(id,B_CT_PSW, B_CT_NSW,B_LSW_ID = oow_id)], by= 'id',all.x=TRUE)
rm(dt.lsw);gc()
dt.out <- merge(dt.out,
                dt.ro[,.(id,D_RO_R)], by= 'id',all.x=TRUE)
rm(dt.ro);gc()
dt.out <- merge(dt.out,
                dt.saw[,.(id,D_SA_W = fr_natte_omtrek)], by= 'id',all.x=TRUE)
rm(dt.saw);gc()
rm(dt.sf)

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
dt.out <- dt.out[!is.na(ref_id)]

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

saveRDS(dt.out,'D:/DATA/18 bln/brp21_blninput.rds')

# prepare LSW datafile for calculations BLN

# shape file to extract data for
#s1.sel <- st_read('dev/bln_demarke.gpkg')

# add unique id
#s1.sel$fid <- s1.sel$id
#s1.sel$id <- 1:nrow(s1.sel)
#sf.sel <- s1.sel[,c('id','geom')]

# make LSW file for whole of NL
require(sf);require(data.table)

# load in LSW and do subset
tmp1 <- st_read(paste0(nmi.dat, "watersysteem/Opgave_oppervlaktewater/products/20240625_oppervlaktewateropgave.gpkg"))

# load in all soil properties needed for LSW preparation
tmp2 <- dt[year==1,.(B_LSW_ID,D_RO_R,D_SA_W,A_SOM_LOI,A_CLAY_MI,A_SAND_MI,A_SILT_MI,
                     A_N_RT,A_P_AL,A_P_WA,A_P_CC,A_P_SG,A_FE_OX,A_AL_OX)]
tmp2 <- tmp2[B_LSW_ID != 'lsw_nlmean']
tmp2 <- tmp2[,list(B_SOM_LOI = mean(A_SOM_LOI),
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
                   B_AL_OX_SD = sd(A_AL_OX),
                   B_RO_R = mean(D_RO_R),
                   B_RO_R_SD = sd(D_RO_R),
                   B_SA_W = mean(D_SA_W),
                   B_SA_W_SD = sd(D_SA_W)),by = 'B_LSW_ID']

# add averaged NL for missing ones or LSW ids that have only a signle field
cols <- c('B_SOM_LOI','B_CLAY_MI','B_SAND_MI', 'B_SILT_MI','B_N_RT','B_P_AL','B_P_CC','B_P_WA' ,'B_P_SG',
          'B_FE_OX' ,'B_AL_OX' ,'B_RO_R' ,'B_SA_W','B_SOM_LOI_SD' ,'B_CLAY_MI_SD',
          'B_SAND_MI_SD' ,'B_SILT_MI_SD' ,'B_N_RT_SD' ,'B_P_AL_SD' ,'B_P_CC_SD' ,
          'B_P_WA_SD' ,'B_P_SG_SD' ,'B_FE_OX_SD' ,'B_AL_OX_SD' ,'B_RO_R_SD' ,'B_SA_W_SD'
          )
tmp2[is.na(B_SOM_LOI_SD), c(cols) := list(8.65,15.8, 60.5,23.71, 3834, 49, 2.71, 40, 22, 83, 40, 0.5, 0.47, 6.67, 13.45,
                          23.5, 11.7, 2928, 13.5, 1.51,15.6, 14, 59, 19, 0.3, 0.33)]

# add averaged NL for missing ones
lsw.nl <- data.table(B_LSW_ID = 'lsw_nlmean', B_SOM_LOI = 8.65,B_CLAY_MI = 15.8,B_SAND_MI = 60.5,
                     B_SILT_MI = 23.71,B_N_RT = 3834,B_P_AL = 49,B_P_CC = 2.71,B_P_WA = 40,B_P_SG = 22,
                     B_FE_OX = 83,B_AL_OX = 40,B_RO_R = 0.5,B_SA_W = 0.47,B_SOM_LOI_SD = 6.67,B_CLAY_MI_SD = 13.45,
                     B_SAND_MI_SD = 23.5,B_SILT_MI_SD = 11.7,B_N_RT_SD = 2928,B_P_AL_SD = 13.5,B_P_CC_SD = 1.51,
                     B_P_WA_SD = 15.6,B_P_SG_SD = 14,B_FE_OX_SD = 59,B_AL_OX_SD = 19,B_RO_R_SD = 0.3,B_SA_W_SD = 0.33)
dt.lsw.extr <- rbind(tmp2,lsw.nl)

# save measures as bbwp table
saveRDS(dt.lsw.extr,'D:/DATA/18 bln/nl_lsw.rds')


# Apply BLN input for all BRP fields for carbon only ---------

  # clean memory
  rm(list=ls())

  # require packages
  library(sf);library(data.table);library(pandex)
  devtools::load_all()

  # set paths
  nmi.dat <- Sys.getenv('NMI_DATA')
  nmi.proj<- Sys.getenv('NMI-PROJ')
  nmi.site<- Sys.getenv('NMI_SITE')

  # load all input
  dt <- readRDS('D:/DATA/18 bln/brp21_blninput.rds')

  # load all LSW
  LSW <- readRDS('D:/DATA/18 bln/nl_lsw.rds')

  # update all inputs
  dt[is.na(B_AER_CBS),B_AER_CBS := 'LG09']
  dt[,B_GWL_GLG := pmin(B_GWL_GLG,300,na.rm=T)]
  dt[is.na(B_GWL_GHG), B_GWL_GHG := 136.47]
  dt[B_GWL_GHG >= B_GWL_GLG, B_GWL_GLG := B_GWL_GHG + 25]
  dt[B_SOILTYPE_AGR == 'loss', B_SOILTYPE_AGR := 'loess']
  dt[,A_DENSITY_SA := NA_real_]
  dt[,ID := id]

  # add default management
  cols <- c('M_GREEN', 'M_NONBARE', 'M_EARLYCROP','M_COMPOST','M_SLEEPHOSE','M_DRAIN','M_DITCH','M_UNDERSEED',
            'M_LIME', 'M_NONINVTILL', 'M_SSPM', 'M_SOLIDMANURE','M_STRAWRESIDUE','M_MECHWEEDS','M_PESTICIDES_DST')
  dt[,c(cols) := NA]
  dt[,M_COMPOST := NA_real_]
  cols <- c( 'A_EW_BCS' ,'A_SC_BCS' , 'A_GS_BCS' ,'A_P_BCS' ,  'A_C_BCS' ,'A_RT_BCS' ,'A_RD_BCS',  'A_SS_BCS' ,  'A_CC_BCS')
  dt[,c(cols) := NA]

  dt[,B_CT_PSW_MAX :=0.5]
  dt[,B_CT_NSW_MAX := 5.0]
  dt[, A_SOM_LOI_MLMAX := a_som_loi_csat_top]

  # load RothC calculations
  dt2 <- readRDS('D:/DATA/18 bln/brp21_i_clim_rothc.rds')
  dt2 <- unique(dt2[,.(id,ref_id,i_clim_rothc)])
  dt <- merge(dt,dt2,by=c('id','ref_id'),all.x=TRUE)

  saveRDS(dt,'D:/DATA/18 bln/brp21_bln_before_scoring.rds')
  saveRDS(out.score.bln,'D:/DATA/18 bln/brp21_bln_scores.rds')
  saveRDS(out.ind,'D:/DATA/18 bln/brp21_bln_indicators.rds')

  dt <- readRDS('D:/DATA/18 bln/brp21_bln_indicators.rds')
  dt.score <- readRDS('D:/DATA/18 bln/brp21_bln_scores.rds')

  s1 <- readRDS('D:/DATA/18 bln/brp21_s1sel.rds')

  dt <- readRDS('D:/DATA/18 bln/brp21_bln_before_scoring.rds')

  d1 <- dt[B_AER_CBS=='IJsselmeerpolders']
  d2 <- out.ind[out.ind$ID %in% d1$id]
  s2 <- s1[s1$id %in% d1$id,]

  st_write(s2,'D:/ESA/03 msc projects/23 dekkers/ijselmeerpolders.gpkg')
  saveRDS(d1,'D:/ESA/03 msc projects/23 dekkers/ijselmeerpolders.rds')
  saveRDS(d2,'D:/ESA/03 msc projects/23 dekkers/ijselmeerpolders_indicator_scores.rds')

  ID = dt$ID
  B_LU_BRP = dt$B_LU_BRP
  B_SOILTYPE_AGR <- dt$B_SOILTYPE_AGR
  A_CLAY_MI = dt$A_CLAY_MI
  A_SAND_MI = dt$A_SAND_MI
  A_SILT_MI = dt$A_SILT_MI
  A_SOM_LOI = dt$A_SOM_LOI
  M_GREEN = dt$M_GREEN
  M_MECHWEEDS = dt$M_MECHWEEDS
  M_PESTICIDES_DST = dt$M_PESTICIDES_DST

  B_SC_WENR = dt$B_SC_WENR
  B_GWL_CLASS = dt$B_GWL_CLASS
  B_AREA_DROUGHT = dt$B_AREA_DROUGHT

  B_DRAIN = dt$B_DRAIN

  penalty = TRUE

  rm(ID,B_LU_BRP,B_SC_WENR,B_GWL_CLASS,B_AREA_DROUGHT,A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI,B_DRAIN,M_GREEN,M_MECHWEEDS,M_PESTICIDES_DST,B_SOILTYPE_AGR)



  #check I_P_DU = NAN, I_GW_PEST
  dt.farm <- copy(dt)
  ID = dt.farm$ref_id
  B_LU_BRP = dt.farm$B_LU_BRP
  B_SC_WENR = dt.farm$B_SC_WENR
  B_GWL_CLASS = dt.farm$B_GWL_CLASS
  B_SOILTYPE_AGR = dt.farm$B_SOILTYPE_AGR
  B_HELP_WENR = dt.farm$B_HELP_WENR
  B_AER_CBS = dt.farm$B_AER_CBS
  B_GWL_GLG = dt.farm$B_GWL_GLG
  B_GWL_GHG = dt.farm$B_GWL_GHG
  B_GWL_ZCRIT = dt.farm$B_GWL_ZCRIT
  B_DRAIN = dt.farm$B_DRAIN
  B_FERT_NORM_FR = dt.farm$B_FERT_NORM_FR
  B_SLOPE_DEGREE = dt.farm$B_SLOPE_DEGREE
  B_GWP = dt.farm$B_GWP
  B_AREA_DROUGHT = dt.farm$B_AREA_DROUGHT
  B_CT_PSW = dt.farm$B_CT_PSW
  B_CT_NSW = dt.farm$B_CT_NSW
  B_CT_PSW_MAX =0.5
  B_CT_NSW_MAX = 5.0
  A_SOM_LOI = dt.farm$A_SOM_LOI
  A_SOM_LOI_MLMAX = dt.farm$a_som_loi_csat_top
  A_CLAY_MI = dt.farm$A_CLAY_MI
  A_SAND_MI = dt.farm$A_SAND_MI
  A_SILT_MI = dt.farm$A_SILT_MI
  A_DENSITY_SA = NA_real_
  A_FE_OX = dt.farm$A_FE_OX
  A_AL_OX = dt.farm$A_AL_OX
  A_PH_CC = dt.farm$A_PH_CC
  A_N_RT = dt.farm$A_N_RT
  A_CN_FR = dt.farm$A_CN_FR
  A_S_RT = dt.farm$A_S_RT
  A_N_PMN = dt.farm$A_N_PMN
  A_P_AL = dt.farm$A_P_AL
  A_P_CC = dt.farm$A_P_CC
  A_P_WA = dt.farm$A_P_WA
  A_P_SG = dt.farm$A_P_SG
  A_CEC_CO = dt.farm$A_CEC_CO
  A_CA_CO_PO = dt.farm$A_CA_CO_PO
  A_MG_CO_PO = dt.farm$A_MG_CO_PO
  A_K_CO_PO = dt.farm$A_K_CO_PO
  A_K_CC = dt.farm$A_K_CC
  A_MG_CC = dt.farm$A_MG_CC
  A_MN_CC = dt.farm$A_MN_CC
  A_ZN_CC = dt.farm$A_ZN_CC
  A_CU_CC = dt.farm$A_CU_CC
  A_EW_BCS = NA; A_SC_BCS = NA; A_GS_BCS = NA;A_P_BCS = NA;  A_C_BCS = NA;A_RT_BCS = NA;A_RD_BCS = NA;  A_SS_BCS = NA;  A_CC_BCS = NA
  D_SA_W = dt.farm$D_SA_W
  D_RO_R = dt.farm$D_RO_R
  M_COMPOST = NA_real_;  M_GREEN = NA;  M_NONBARE = NA;  M_EARLYCROP = NA;  M_SLEEPHOSE = NA;  M_DRAIN = NA;  M_DITCH = NA;  M_UNDERSEED = NA
  M_LIME = NA;  M_NONINVTILL = NA;  M_SSPM = NA;  M_SOLIDMANURE = NA;  M_STRAWRESIDUE = NA;  M_MECHWEEDS = NA;  M_PESTICIDES_DST = NA
  B_LSW_ID = dt.farm$B_LSW_ID
  output ='all'
  runrothc = FALSE
  mc = TRUE
  i_clim_rothc = NA_real_



# run carbon for BLN separate ----------

  # extract relevant properties
  ID = dt$id
  B_LU_BRP <- dt$B_LU_BRP
  B_GWL_GLG <- pmin(dt$B_GWL_GLG,300,na.rm=T)
  A_SOM_LOI <- dt$A_SOM_LOI
  A_CLAY_MI <- dt$A_CLAY_MI
  quiet = FALSE
  mc = TRUE
  rm(dt)
  out <- list()
  count <- 0

  # run rothC
  rid <- round(seq(1,max(dt$id),length.out = 20))

  a = Sys.time()

  for(i in 20:20){#length(rid)){

    count <- count + 1
    asample <- c(rid[i-1]:rid[i])
    this.id <- ID[ID %in% asample]
    this.brp <- B_LU_BRP[ID %in% asample]
    this.gwl <- B_GWL_GLG[ID %in% asample]
    this.som <- A_SOM_LOI[ID %in% asample]
    this.clay <- A_CLAY_MI[ID %in% asample]


    out[[count]] <-bln_clim_rothc(ID = this.id,
                              B_LU_BRP = this.brp,
                              B_GWL_GLG = this.gwl,
                              A_SOM_LOI = this.som,
                              A_CLAY_MI = this.clay,
                              quiet = quiet, mc=mc)

    print(i)
  }
  saveRDS(out,'D:/DATA/18 bln/rothc_out.rds')
  rm(A_CLAY_MI,A_SOM_LOI,B_GWL_GLG,B_LU_BRP,ID,this.brp,this.clay,this.gwl,this.id,this.som)
  out2 <- rbind(out)
  out2 <- data.table(i_clim_rothc = as.numeric(out2))


  dt <- readRDS('D:/DATA/18 bln/brp21_blninput.rds')
  ID = dt$id
  count <- 0; this.id <- list()
  for(i in 2:length(rid)){
    count <- count + 1
    asample <- c(rid[i-1]:rid[i])

    tmp1 <- data.table(id = ID[ID %in% asample])
    #tmp1[,xs := .GRP,by = id]
    #setorder(tmp1,xs)
    this.id[[count]] <- tmp1$id

  }

  this.id <- data.table(id=unlist(this.id))
  this.id[,year := 2010 + 1:.N,by=id]

  dt2 <- copy(this.id)
  dt2[,i_clim_rothc := out2$i_clim_rothc]
  dt2 <- dt2[year < 2021]
  dt2 <- unique(dt2[,.(id,i_clim_rothc)])

  dt3 <- copy(dt)
  dt3 <- merge(dt3,dt2,by='id',all.x=TRUE)

  dt4 <- dt3[,.(id,ref_id,geom,year,i_clim_rothc)]
  saveRDS(dt4,'D:/DATA/18 bln/brp21_i_clim_rothc.rds')

  # test for case 1039 => initial fractions carbon becomes negative.
  dt.test <- dt3[id==1039]
  dt.test[,value := bln_clim_rothc(ID = id,B_LU_BRP = B_LU_BRP,B_GWL_GLG = B_GWL_GLG,
                                   A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI,
                                   quiet = FALSE, mc=FALSE)]
  dt.c <- data.table(ID = dt.test$id,
                     B_LU_BRP = dt.test$B_LU_BRP,
                     B_GWL_GLG = dt.test$B_GWL_GLG,
                     A_SOM_LOI = dt.test$A_SOM_LOI,
                     A_CLAY_MI= pmin(75,dt.test$A_CLAY_MI))



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
  dt.farm[B_SOILTYPE_AGR=='loss', B_SOILTYPE_AGR := 'loess']
  dt.farm[B_LSW_ID == 175, B_LSW_ID := 'lsw_nlmean']
  dt.farm[B_GWL_GHG >= B_GWL_GLG, B_GWL_GLG := B_GWL_GHG-10]

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

  saveRDS(out,'D:/OneDrive - SPRINGG/NMI-PROJ/BBWP/R_BBWPpaper/data/nl_bbwp_5000_fields_results.rds')

  d2 <- merge(dt.farm[year==2021,],out,by.x='ref_id_2022',by.y='ID',all.x=TRUE)
  s2 <- sf::st_as_sf(d2)
  sf::st_write(s2,'D:/OneDrive - SPRINGG/NMI-PROJ/BBWP/R_BBWPpaper/data/nl_bbwp_5000_fields_results.gpkg')


