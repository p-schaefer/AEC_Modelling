library(tidyverse)
library(openxlsx)

lu_master<-readRDS(file.path("data","lookups.rds"))

td<-tempdir()


sub_region_out<-read_csv(file.path("data","final","Biopoint_LC_Attr.csv"))

ep_data<-readxl::read_excel(file.path("data","Processed","Bio","Fish_Endpoints.xlsx"),"Results - Wide") %>% 
  mutate(SampleDate=as.Date(SampleDate)) %>% 
  left_join(sub_region_out)

write_csv(ep_data,file.path("data","final","Model_building_endpoint_data.csv"))

tx_data<-readxl::read_excel(file.path("data","Processed","Bio","Fish_Endpoints.xlsx"),"Taxa Table") %>% 
  mutate(SampleDate=as.Date(SampleDate)) %>% 
  left_join(sub_region_out)

write_csv(tx_data,file.path("data","final","Model_building_taxa_data.csv"))

wb = createWorkbook()

addWorksheet(wb, "Endpoint Dataset")
addWorksheet(wb, "Taxa Dataset")

writeData(wb,ep_data, sheet="Endpoint Dataset", rowNames =FALSE)
writeData(wb,tx_data, sheet="Taxa Dataset", rowNames =FALSE)

saveWorkbook(wb, file.path("data","final","Model_building_data.xlsx"),overwrite = T)

