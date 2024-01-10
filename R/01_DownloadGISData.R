# This script will be setup to download data from from government of Canada


# Integrated Hydrology ----------------------------------------------------

IH_list<-list(
  SE="https://ws.gisetl.lrc.gov.on.ca/fmedatadownload/Packages/OIH-Data-Package-SE.zip",
  SW="https://ws.gisetl.lrc.gov.on.ca/fmedatadownload/Packages/OIH-Data-Package-SW.zip",
  NE="https://ws.gisetl.lrc.gov.on.ca/fmedatadownload/Packages/OIH-Data-Package-NE.zip",
  NC="https://ws.gisetl.lrc.gov.on.ca/fmedatadownload/Packages/OIH-Data-Package-NC.zip"
)



# AEC ---------------------------------------------------------------------

AEC_list<-list(
  LakeOntario="https://www.gisapplication.lrc.gov.on.ca/fmedatadownload/Packages/AEC_Core_Package02_LakeOntario.zip",
  LakeErie="https://www.gisapplication.lrc.gov.on.ca/fmedatadownload/Packages/AEC_Core_Package01_LakeErie.zip",
  LakeHuronSouth="https://www.gisapplication.lrc.gov.on.ca/fmedatadownload/Packages/AEC_Core_Package04_LakeHuronSouth.zip",
  OttawaStLawrenceRivers="https://www.gisapplication.lrc.gov.on.ca/fmedatadownload/Packages/AEC_Core_Package03_OttawaStLawrenceRivers.zip",
  LakeHuronNorth="https://www.gisapplication.lrc.gov.on.ca/fmedatadownload/Packages/AEC_Core_Package05_LakeHuronNorth.zip"
)


# Land Cover --------------------------------------------------------------
# Only Ontario_Landcover_compilation used for now

LC_list<-list(
  Ontario_Landcover_compilation="https://ws.gisetl.lrc.gov.on.ca/fmedatadownload/Packages/OntarioLandCoverComp-v2.zip"#,
  # SOLRIS="https://ws.gisetl.lrc.gov.on.ca/fmedatadownload/Packages/SOLRIS_Version_3_0.zip",
  # Far_North_LC="https://ws.gisetl.lrc.gov.on.ca/fmedatadownload/Packages/FarNorthLandCover.zip",
  # Provincial_LC="https://ws.gisetl.lrc.gov.on.ca/fmedatadownload/Packages/Provincial-Landcover-2000.zip",
  # Canada_landcover_2010="https://ftp.maps.canada.ca/pub/nrcan_rncan/Land-cover_Couverture-du-sol/canada-landcover_canada-couverture-du-sol/CanadaLandcover2010.zip",
  # Canada_landcover_2015="https://ftp.maps.canada.ca/pub/nrcan_rncan/Land-cover_Couverture-du-sol/canada-landcover_canada-couverture-du-sol/CanadaLandcover2015.zip",
  # Canada_landcover_2015="https://datacube-prod-data-public.s3.ca-central-1.amazonaws.com/store/land/landcover/landcover-2020-classification.tif"
)

# ESRI_LC_list<-list(
#   ESRI_LC_17T_2017="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2017/17T_20170101-20180101.tif",
#   ESRI_LC_17T_2018="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2018/17T_20180101-20190101.tif",
#   ESRI_LC_17T_2019="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2019/17T_20190101-20200101.tif",
#   ESRI_LC_17T_2020="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2020/17T_20200101-20210101.tif",
#   ESRI_LC_17T_2021="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2021/17T_20210101-20220101.tif",
#   ESRI_LC_18T_2017="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2017/18T_20170101-20180101.tif",
#   ESRI_LC_18T_2018="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2018/18T_20180101-20190101.tif",
#   ESRI_LC_18T_2019="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2019/18T_20190101-20200101.tif",
#   ESRI_LC_18T_2020="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2020/18T_20200101-20210101.tif",
#   ESRI_LC_18T_2021="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2021/18T_20210101-20220101.tif",
#   ESRI_LC_17U_2017="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2017/17U_20170101-20180101.tif",
#   ESRI_LC_17U_2018="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2018/17U_20180101-20190101.tif",
#   ESRI_LC_17U_2019="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2019/17U_20190101-20200101.tif",
#   ESRI_LC_17U_2020="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2020/17U_20200101-20210101.tif",
#   ESRI_LC_17U_2021="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2021/17U_20210101-20220101.tif",
#   ESRI_LC_16U_2017="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2017/16U_20170101-20180101.tif",
#   ESRI_LC_16U_2018="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2018/16U_20180101-20190101.tif",
#   ESRI_LC_16U_2019="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2019/16U_20190101-20200101.tif",
#   ESRI_LC_16U_2020="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2020/16U_20200101-20210101.tif",
#   ESRI_LC_16U_2021="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2021/16U_20210101-20220101.tif",
#   ESRI_LC_15U_2017="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2017/15U_20170101-20180101.tif",
#   ESRI_LC_15U_2018="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2018/15U_20180101-20190101.tif",
#   ESRI_LC_15U_2019="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2019/15U_20190101-20200101.tif",
#   ESRI_LC_15U_2020="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2020/15U_20200101-20210101.tif",
#   ESRI_LC_15U_2021="https://lulctimeseries.blob.core.windows.net/lulctimeseriespublic/lc2021/15U_20210101-20220101.tif"
# 
# )

# Combine Lists -----------------------------------------------------------

master_dl<-list(
  IH=IH_list,
  AEC=AEC_list,
  LC=LC_list#,
  # ESRI=ESRI_LC_list
)


# Download files ----------------------------------------------------------

if (T) {
  library(curl)
  
  if (!dir.exists(file.path("data","raw","GIS"))) dir.create(file.path("data","raw","GIS"))
  
  dl<-lapply(master_dl,function(l1) lapply(l1,function(l2) {
    if (!file.exists(file.path("data","raw","GIS",basename(l2)))){
      h = new_handle(dirlistonly=TRUE)
      con = curl(l2, "r", h)
      curl_download(l2,file.path("data","raw","GIS",basename(l2)))
    }
  }))
}



if (F) {
  library(sf)
  library(tidyverse)
  library(rgee)
  library(future.apply)
  
  # will need to process hydrology first 
  hydrology<-readRDS(file.path("data","Processed","Hydrology","Processed_Hydrology.rds"))
  boundaries<-map_dfr(hydrology,~map_dfr(.,~.$boundary %>% st_buffer(60))) %>% 
    rename(tile=WorkUnitName)
  rm(hydrology)
  
  plan(multisession(workers = 1))
  
  
  #ee_clean_container()
  ee_Initialize(user = 'Patrick Schaefer', drive = TRUE)
  
  
  # region <- ee$Geometry$Polygon(list(c(-74.34,56.86),c(-74.34,41.66),c(-95.16,41.66),c(-95.16,56.86))) # all ontario
  # 
  # region_split<-st_make_grid(ee_as_sf(region)) %>% 
  #   st_as_sf() %>% 
  #   mutate(tile=row_number()) %>% 
  #   filter(!tile %in% c(31,32,11,1,32,22,12,2,33,23,13,3,34,24,14,4,
  #                       21,25,15,5,16,
  #                       91,97,98,99,100,
  #                       88,89,90,
  #                       78:80,
  #                       69:70,
  #                       59:60,
  #                       49:50,
  #                       40,
  #                       10,9#,
  #                       #6,7,8,17,18,19,20,26
  #                       
  #   )) %>% 
  #   split(.$tile)
  
  region_split<-split(boundaries,boundaries$tile)
  
  zip_file<-file.path("data","raw","GIS",paste0("GoogleEarthEngine.zip"))
  fl<-unzip(zip_file,list=T)$Name
  # 
  t1<-stringr::str_split(fl,"_",simplify = T)[,c(1,2,4:7,8,9)]
  colnames(t1)<-c("layer","sat","region","loc1","loc2","loc3","other","year")
  
  t2<-t1 %>%
    as_tibble() %>%
    mutate(year=case_when(
      year=="" ~ other,
      T ~ year
    )) %>%
    mutate(other=case_when(
      other==year ~ "",
      T ~ other
    )) %>%
    mutate(loc=paste0(loc1,loc2,loc3,other)) %>%
    select(-loc1:-other) %>%
    mutate(year=str_sub(year,1,4))
  
  table(t2$year,t2$region,t2$layer)
  
  out_loop<-future_lapply(region_split,function(reg){
    ee_Initialize(user = 'Patrick Schaefer', drive = TRUE)
    
    
    # bnd<-st_bbox(reg)
    # region<-ee$Geometry$Polygon(list(c(bnd[c(3,4)]),c(bnd[c(3,2)]),c(bnd[c(1,2)]),c(bnd[c(1,4)])))
    region<-sf_as_ee(st_geometry(reg))
    
    for (yr in 2014:2021){
      # Existing EVI LS8
      file_out<-file.path("data","raw","GIS",paste0("EVI_LS8_mean8Day_",reg$tile,"_",yr,".tif"))
      if (!paste0("EVI_LS8_mean8Day_",reg$tile,"_",yr,".tif") %in% fl) {
        EVI_8Day<-ee$ImageCollection('LANDSAT/LC08/C01/T1_8DAY_EVI')$ 
          filterDate(paste0(yr,'-01-01'), paste0(yr,'-12-31'))$ 
          filter(ee$Filter$calendarRange(94,254,"day_of_year"))$ #julian weeks 15 to 37
          filterBounds(region)
        
        years <- ee$List$sequence(yr, yr)
        
        EVI_8Day_Yearly <- ee$ImageCollection$fromImages(
          years$map( ee_utils_pyfunc(function(year) {
            yearly <- EVI_8Day$
              filter(ee$Filter$calendarRange(year, year, "Year"))$
              filter(ee$Filter$calendarRange(94,254,"day_of_year"))$ #julian weeks 15 to 37
              select("EVI")$mean()
            return(yearly$
                     set("year", year)
            )
          })))
        
        #ee_print(EVI_8Day_Yearly)
        
        out<-try(ee_imagecollection_to_local(EVI_8Day_Yearly,
                                             region=region,
                                             dsn=file_out,
                                             scale=30,
                                             maxPixels=10^10,
                                             container = paste0("EVI_LS8_mean8Day_",reg$tile,"_",yr)
        ))
        
        try(zip(files=file_out,
                zipfile=zip_file,
                flags ="-r9Xjqu"
        ))
        
        file.remove(file_out)
        
        ee_clean_container(paste0("EVI_LS8_mean8Day_",reg$tile,"_",yr))
      }
      
      # Existing NDWI LS8
      file_out<-file.path("data","raw","GIS",paste0("NDWI_LS8_mean8Day_",reg$tile,"_",yr,".tif"))
      if (!paste0("NDWI_LS8_mean8Day_",reg$tile,"_",yr,".tif") %in% fl) {
        NDWI_8Day<-ee$ImageCollection('LANDSAT/LC08/C01/T1_8DAY_NDWI')$ 
          filterDate(paste0(yr,'-01-01'), paste0(yr,'-12-31'))$ 
          filter(ee$Filter$calendarRange(94,254,"day_of_year"))$ #julian weeks 15 to 37
          filterBounds(region)
        
        years <- ee$List$sequence(yr, yr)
        
        NDWI_8Day_Yearly <- ee$ImageCollection$fromImages(
          years$map( ee_utils_pyfunc(function(year) {
            yearly <- NDWI_8Day$
              filter(ee$Filter$calendarRange(year, year, "Year"))$
              filter(ee$Filter$calendarRange(94,254,"day_of_year"))$ #julian weeks 15 to 37
              select("NDWI")$mean()
            return(yearly$
                     set("year", year)
            )
          })))
        
        #ee_print(NDWI_8Day_Yearly)
        
        out<-try(ee_imagecollection_to_local(NDWI_8Day_Yearly,
                                             region=region,
                                             dsn=file_out,
                                             scale=30,
                                             maxPixels=10^10,
                                             container = paste0("NDWI_LS8_mean8Day_",reg$tile,"_",yr)
        ))
        
        try(zip(files=file_out,
                zipfile=zip_file,
                flags ="-r9Xjqu"
        ))
        
        file.remove(file_out)
        
        ee_clean_container(paste0("NDWI_LS8_mean8Day_",reg$tile,"_",yr))
      }
      
      
    }
    
    if (T){
      for (yr in 2000:2013) {
        # Existing EVI LS7
        file_out<-file.path("data","raw","GIS",paste0("EVI_LS7_mean8Day_",reg$tile,"_",yr,".tif"))
        if (!paste0("EVI_LS7_mean8Day_",reg$tile,"_",yr,".tif") %in% fl) {
          EVI_8Day<-ee$ImageCollection("LANDSAT/LE07/C01/T1_8DAY_EVI")$ 
            filterDate(paste0(yr,'-01-01'), paste0(yr,'-12-31'))$ 
            filter(ee$Filter$calendarRange(94,254,"day_of_year"))$ #julian weeks 15 to 37
            filterBounds(region)
          
          years <- ee$List$sequence(yr, yr)
          
          EVI_8Day_Yearly <- ee$ImageCollection$fromImages(
            years$map( ee_utils_pyfunc(function(year) {
              yearly <- EVI_8Day$
                filter(ee$Filter$calendarRange(year, year, "Year"))$
                filter(ee$Filter$calendarRange(94,254,"day_of_year"))$ #julian weeks 15 to 37
                select("EVI")$mean()
              return(yearly$
                       set("year", year)
              )
            })))
          
          #ee_print(EVI_8Day_Yearly)
          
          out<-try(ee_imagecollection_to_local(EVI_8Day_Yearly,
                                               region=region,
                                               dsn=file_out,
                                               scale=30,
                                               maxPixels=10^10,
                                               container = paste0("EVI_LS7_mean8Day_",reg$tile,"_",yr)))
          
          try(zip(files=file_out,
                  zipfile=zip_file,
                  flags ="-r9Xjqu"
          ))
          
          file.remove(file_out)
          
          ee_clean_container(paste0("EVI_LS7_mean8Day_",reg$tile,"_",yr))
        }
        
        # Existing NDWI LS7
        file_out<-file.path("data","raw","GIS",paste0("NDWI_LS7_mean8Day_",reg$tile,"_",yr,".tif"))
        if (!paste0("NDWI_LS7_mean8Day_",reg$tile,"_",yr,".tif") %in% fl) {
          NDWI_8Day<-ee$ImageCollection("LANDSAT/LE07/C01/T1_8DAY_NDWI")$ 
            filterDate(paste0(yr,'-01-01'), paste0(yr,'-12-31'))$ 
            filter(ee$Filter$calendarRange(94,254,"day_of_year"))$ #julian weeks 15 to 37
            filterBounds(region)
          
          years <- ee$List$sequence(yr, yr)
          
          NDWI_8Day_Yearly <- ee$ImageCollection$fromImages(
            years$map( ee_utils_pyfunc(function(year) {
              yearly <- NDWI_8Day$
                filter(ee$Filter$calendarRange(year, year, "Year"))$
                filter(ee$Filter$calendarRange(94,254,"day_of_year"))$ #julian weeks 15 to 37
                select("NDWI")$mean()
              return(yearly$
                       set("year", year)
              )
            })))
          
          #ee_print(NDWI_8Day_Yearly)
          
          out<-try(ee_imagecollection_to_local(NDWI_8Day_Yearly,
                                               region=region,
                                               dsn=file_out,
                                               scale=30,
                                               maxPixels=10^10,
                                               container = paste0("NDWI_LS7_mean8Day_",reg$tile,"_",yr)))
          
          try(zip(files=file_out,
                  zipfile=zip_file,
                  flags ="-r9Xjqu"
          ))
          
          file.remove(file_out)
          
          ee_clean_container(paste0("NDWI_LS7_mean8Day_",reg$tile,"_",yr))    }
        
        
      }
    }
    
  })
  
  
  
  
  
  
  
  
  
  if (F){# old
    library(rgee)
    
    ee_Initialize(user = 'Patrick Schaefer', drive = TRUE)
    
    library(sf)
    library(tidyverse)
    
    addNDVI <- function(image) {
      ndvi <- image$normalizedDifference(c('B5', 'B4'))$rename('NDVI');
      return(image$addBands(ndvi))
    }
    
    #region <- ee$Geometry$Polygon(list(c(-105, 39.05), c(-104.5, 39.05), c(-104.5, 38.75), c(-105, 38.75), c(-105, 39.05))) 
    region <- ee$Geometry$Polygon(list(c(-74.34,56.86),c(-74.34,41.66),c(-95.16,41.66),c(-95.16,56.86))) # all ontario
    
    region_split<-st_make_grid(ee_as_sf(region)) %>% 
      st_as_sf() %>% 
      mutate(tile=row_number()) %>% 
      filter(!tile %in% c(31,32,11,1,32,22,12,2,33,23,13,3,34,24,14,4,
                          21,25,15,5,16,
                          91,97,98,99,100,
                          88,89,90,
                          78:80,
                          69:70,
                          59:60,
                          49:50,
                          40,
                          10,9#,
                          #6,7,8,17,18,19,20,26
                          
      )) %>% 
      split(.$tile)
    
    #mapview::mapview(region_split)
    
    out_loop<-lapply(region_split,function(reg){
      
      if (all(file.exists(file.path("data","raw","GIS",paste0("NDVI_",reg$tile,"_",2014:2021,".tif"))))){
        return(file.path("data","raw","GIS",paste0("NDVI_",reg$tile,"_",2014:2021,".tif")))
      }
      
      bnd<-st_bbox(reg)
      region<-ee$Geometry$Polygon(list(c(bnd[c(3,4)]),c(bnd[c(3,2)]),c(bnd[c(1,2)]),c(bnd[c(1,4)])))
      
      landsat <- ee$ImageCollection('LANDSAT/LC08/C01/T1_SR')$
        filterDate(paste0("2014",'-01-01'), paste0("2021",'-12-31'))$
        filterBounds(region)$
        map(addNDVI)$
        select('NDVI')
      
      years <- ee$List$sequence(2014, 2021)
      
      landsatYearly <- ee$ImageCollection$fromImages(
        years$map( ee_utils_pyfunc(function(year) {
          yearly <- landsat$
            filter(ee$Filter$calendarRange(year, year, "Year"))$
            select("NDVI")$max()
          return(yearly$
                   set("year", year)$
                   set("system:time_start", ee$Date$fromYMD(year, 1, 1)))
        })))
      
      #ee_print(landsatYearly)
      
      out<-ee_imagecollection_to_local(landsatYearly,
                                       region=region,
                                       dsn=file.path("data","raw","GIS",paste0("NDVI_",reg$tile,"_",2014:2021)),
                                       scale=30,
                                       maxPixels=10^10)
      
      ee_clean_container()
      
      return(file.path("data","raw","GIS",paste0("NDVI_",reg$tile,"_",2014:2021,".tif")))
      
    })
    
    
  }
  
  
  
}
