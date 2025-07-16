library(tidyverse)
library(sf)

save_dir <- tempdir() #change this for permanent storage
temp_dir <- tempfile()
dir.create(temp_dir)

# Download GIS Data -------------------------------------------------------

AEC_list<-list( # could add remaining regions if you want
  LakeOntario="https://ws.gisetl.lrc.gov.on.ca/fmedatadownload/Packages/AEC_Core_Package02_LakeOntario.zip",
  LakeErie="https://ws.gisetl.lrc.gov.on.ca/fmedatadownload/Packages/AEC_Core_Package01_LakeErie.zip",
  LakeHuronSouth="https://ws.gisetl.lrc.gov.on.ca/fmedatadownload/Packages/AEC_Core_Package04_LakeHuronSouth.zip",
  OttawaStLawrenceRivers="https://ws.gisetl.lrc.gov.on.ca/fmedatadownload/Packages/AEC_Core_Package03_OttawaStLawrenceRivers.zip",
  LakeHuronNorth="https://ws.gisetl.lrc.gov.on.ca/fmedatadownload/Packages/AEC_Core_Package05_LakeHuronNorth.zip",
  LakeSuperior="https://ws.gisetl.lrc.gov.on.ca/fmedatadownload/Packages/AEC_Core_Package06_LakeSuperior.zip"
)

master_dl<-list(
  AEC=AEC_list
)


dl<-lapply(master_dl,function(l1) lapply(l1,function(l2) {
  if (!file.exists(file.path(save_dir,basename(l2)))){
    h = curl::new_handle(dirlistonly=TRUE)
    con = curl::curl(l2, "r", h)
    curl::curl_download(l2,file.path(save_dir,basename(l2)))
  }
}))


# Download FWIS Data ------------------------------------------------------

source("FWIS_Functions.R")

# You can store your username and password locally by
# adding them to file.edit("~/.Renviron")
username<-Sys.getenv("FWIS_username")
password<-Sys.getenv("FWIS_password")

fwis_login<-wideLogin("http://www.comap.ca/fwis/wideR.php",username,password)

# Fish Download -----------------------------------------------------------

fish_tbls<-c("tblFishSummaryOfTotalCatches", # This table is fish collection records
             "tblChannelStructureSummary" # This table is some channel structure summaries
) # You may have to ask Les if there is a glossary for what each of the fields are

fish_all <- lapply(setNames(fish_tbls,fish_tbls), function(x){
  wideDataSelect( 1, table=x, colsSelected=as.list("*") )
})

saveRDS(fish_all,file.path(save_dir,"FWIS_data.rds"))


# Unique FWIS Coordinates -------------------------------------------------

fwis_coord <- bind_rows(
  fish_all$tblFishSummaryOfTotalCatches %>% select(SampleEventID,Latitude,Longitude),
  fish_all$tblChannelStructureSummary %>% select(SampleEventID,Latitude,Longitude)
) %>% 
  distinct() %>% 
  filter(!is.na(Latitude),
         !is.na(Longitude)) %>% 
  sf::st_as_sf(coords=c("Longitude","Latitude"),crs="WGS84")

# Review Locations
mapview::mapview(fwis_coord,zcol=NULL)

# Potential filters
# filter( 
#   OSAPSE==1, #the sample event was associated with at least one OSAP project and the sample event itself used site boundaries that were defined as per OSAP
#   !is.na(TotalWeightPer100m2) # This makes sure only valid single taxa are included (i.e., Cyprid bulk samples will be excluded)
# )

# Extract stream GIS Data --------------------------------------------------------

aec_region <- list.files(file.path(save_dir),pattern = "AEC_Core",full.names = T)
names(aec_region)<-aec_region

sub_regions <- lapply(names(aec_region),function(aes_nm){
  unzip(aes_nm,exdir=temp_dir)
  
  zip_cont<-list.files(file.path(temp_dir,gsub(".zip","",basename(aes_nm))),recursive = T,full.names = T)
  sub_regions<-zip_cont[grepl("/w",zip_cont) & grepl("AEC_Class.lyrx",zip_cont)]
  sub_regions<-gsub("_Class\\.lyrx","_Core\\.gdb",sub_regions)
  names(sub_regions)<-sub_regions
  
  sub_regions<-lapply(sub_regions, st_layers)
  
  sub_region_out<-map2(names(sub_regions),sub_regions,function(src,lyr) {
    list(boundary=read_sf(src,lyr$name[grepl("Boundary",lyr$name)]),
         stream=read_sf(src,lyr$name[grepl("Reach",lyr$name)]))
  })
  
  names(sub_region_out)<-sapply(names(sub_regions),function(x) gsub(".gdb","",basename(x)))
  
  return(sub_region_out)
})
names(sub_regions)<-gsub(".zip","",basename(names(aec_region)))

#Review Streamlines
mapview::mapview(sub_regions$AEC_Core_Package01_LakeErie$w01_AEC_Core$stream)

# Join Streamlines and FWIS -----------------------------------------------

all_streams <- map_dfr(sub_regions, ~map_dfr(.x,~.x$stream))
fwis_coord <- sf::st_transform(fwis_coord, sf::st_crs(all_streams))

# This will take a few while to run
joined_data <- sf::st_join(fwis_coord,
                           all_streams %>% 
                             filter(!Network_Line_Type %in% c("Shoreline Virtual Connector","Virtual Connector")),
                           join = nngeo::st_nn, k=1, maxdist=500, parallel = 8) # set maximum snap distance to 500m

# joined_data will now have everything you need to join FWIS and AEC data


