source("R/00_Functions/FWIS_Functions.R")
library(tidyverse)

fwis_login<-wideLogin("http://www.comap.ca/fwis/wideR.php","pscha","P@tr!ck33")

# Fish Download -----------------------------------------------------------

fish_tbls<-"tblFishSummaryOfTotalCatches"

fish_all<-lapply(setNames(fish_tbls,fish_tbls),function(x){
  columns <- wideDataSelect(1,
                            "tblDataAPI",
                            list("tbldColName", "tbldActionsRdRegUser", "tbldPK", "tbldNulls", "tbldType", "tbldGSConstraint","tbldFilePath","tbldFileFolder"),
                            where=list(tbldUserTblName=c("=",x)),
                            orderBy=list("tbldOrdering"))
  
  wideDataSelect( 1, table=x, colsSelected=as.list(columns$tbldColName) )
  
})

write_csv(fish_all$tblFishSummaryOfTotalCatches,file.path("data","raw","Bio","tblFishSummaryOfTotalCatches.csv"))


# Benthic Invertebrate Download -------------------------------------------

tables <- unique(wideDataSelect(1,"tblDataAPI", list("tbldUserTblName"), orderBy = list("tbldUserTblName")))

bic_tbls<-tables$tbldUserTblName[grepl("benthic",tolower(tables$tbldUserTblName)) & grepl("tbl",tolower(tables$tbldUserTblName))]

bic_all<-lapply(setNames(bic_tbls,bic_tbls),function(x){
  columns <- wideDataSelect(1,
                            "tblDataAPI",
                            list("tbldColName", "tbldActionsRdRegUser", "tbldPK", "tbldNulls", "tbldType", "tbldGSConstraint","tbldFilePath","tbldFileFolder"),
                            where=list(tbldUserTblName=c("=",x)),
                            orderBy=list("tbldOrdering"))
  
  wideDataSelect( 1, table=x, colsSelected=as.list(columns$tbldColName) )
  
})

saveRDS(bic_all,file.path("data","raw","Bio","BIC_ALL_ED.rds"))

#I think I might actually need OBBN tables
obbn_tbls<-tables$tbldUserTblName[grepl("obbn",tolower(tables$tbldUserTblName)) & grepl("tbl",tolower(tables$tbldUserTblName))]

#obbn_tbls <- obbn_tbls[c(1,2)]
obbn_all<-lapply(setNames(obbn_tbls,obbn_tbls),function(x){
  columns <- wideDataSelect(1,
                            "tblDataAPI",
                            list("tbldColName", "tbldActionsRdRegUser", "tbldPK", "tbldNulls", "tbldType", "tbldGSConstraint","tbldFilePath","tbldFileFolder"),
                            where=list(tbldUserTblName=c("=",x)),
                            orderBy=list("tbldOrdering"))
  
  columns$tbldColName <- gsub("Â", "", columns$tbldColName)
  
  wideDataSelect( 1, table=x, colsSelected=as.list(columns$tbldColName) )
  
})

saveRDS(obbn_all,file.path("data","raw","Bio","OBBN_ALL.rds"))


