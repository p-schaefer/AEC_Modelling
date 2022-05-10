# This script will be setup to download data from from government of Canada


# Integrated Hydrology ----------------------------------------------------

IH_list<-list(
  SE="https://ws.gisetl.lrc.gov.on.ca/fmedatadownload/Packages/OIH-Data-Package-SE.zip",
  SW="https://ws.gisetl.lrc.gov.on.ca/fmedatadownload/Packages/OIH-Data-Package-SW.zip",
  NE="https://ws.gisetl.lrc.gov.on.ca/fmedatadownload/Packages/OIH-Data-Package-NE.zip"
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

LC_list<-list(
  SOLRIS="https://ws.gisetl.lrc.gov.on.ca/fmedatadownload/Packages/SOLRIS_Version_3_0.zip",
  Canada_landcover="https://ftp.maps.canada.ca/pub/nrcan_rncan/Land-cover_Couverture-du-sol/canada-landcover_canada-couverture-du-sol/CanadaLandcover2015.zip"
)



# Combine Lists -----------------------------------------------------------

master_dl<-list(
  IH=IH_list,
  AEC=AEC_list,
  LC=LC_list
)


# Download files ----------------------------------------------------------

dl<-lapply(master_dl,function(l1) lapply(l1,function(l2) download.file(l2,file.path("data","raw","GIS"))))
