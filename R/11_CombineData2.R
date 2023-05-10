library(tidyverse)
library(openxlsx)

lu_master<-readRDS(file.path("data","lookups.rds"))

td<-tempdir()

aec_out<-readRDS(file.path("data","final","int_results.rds"))
matched_points<-map_dfr(aec_out,~.x$bio_pnt_out) %>% 
  as_tibble() %>% 
  select(StreamName:SampleEventID,link_id,everything(),-geometry)

sub_region_out<-read_csv(file.path("data","final","Biopoint_LC_Attr.csv"))

ep_data<-readxl::read_excel(file.path("data","Processed","Bio","Fish_Endpoints.xlsx"),"Results - Wide") %>% 
  mutate(SampleDate=as.Date(SampleDate)) %>% 
  left_join(sub_region_out) %>% 
  left_join(matched_points)

write_csv(ep_data,file.path("data","final","Model_building_endpoint_data.csv"))

tx_data<-readxl::read_excel(file.path("data","Processed","Bio","Fish_Endpoints.xlsx"),"Taxa Table") %>% 
  mutate(SampleDate=as.Date(SampleDate)) %>% 
  left_join(sub_region_out) %>% 
  left_join(matched_points)

write_csv(tx_data,file.path("data","final","Model_building_taxa_data.csv"))

wb = createWorkbook()

addWorksheet(wb, "Endpoint Dataset")
addWorksheet(wb, "Taxa Dataset")

writeData(wb,ep_data, sheet="Endpoint Dataset", rowNames =FALSE)
writeData(wb,tx_data, sheet="Taxa Dataset", rowNames =FALSE)

saveWorkbook(wb, file.path("data","final","Model_building_data.xlsx"),overwrite = T)

