source("R/00_Functions/FWIS_Functions.R")

fwis_login<-wideLogin("http://www.comap.ca/fwis/wideR.php","pscha","P@tr!ck33")

tables <- unique(wideDataSelect(1,"tblDataAPI", list("tbldUserTblName"), orderBy = list("tbldUserTblName")))

tables$tbldUserTblName[grepl("benthic",tolower(tables$tbldUserTblName))]

columns <- wideDataSelect(1,
                          "tblDataAPI",
                          list("tbldColName", "tbldActionsRdRegUser", "tbldPK", "tbldNulls", "tbldType", "tbldGSConstraint","tbldFilePath","tbldFileFolder"),
                          where=list(tbldUserTblName=c("=","tblBenthicTallyArea")),
                          orderBy=list("tbldOrdering"))

data_sel<-wideDataSelect( 1, table="tblBenthicTallyArea", colsSelected=as.list(columns$tbldColName) )

data_sel<-wideDataSelect( 1, table="tblBenthicTallyArea", colsSelected=list("OrgSampleEventID") )
