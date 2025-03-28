library(tidyverse)
library(tidymodels)

#HW_save_loc<-"/mnt/storage/HW"
HW_save_loc<-file.path("data","HW")
if (!dir.exists(HW_save_loc)) dir.create(HW_save_loc)

lu_master<-readRDS(file.path("data","lookups.rds"))

td<-tempdir()

n_cores<-availableCores(logical = F)-1

plan(multisession(workers=4))


parse_fn<-function(x){
  out<-tibble(
    fl=x
  ) %>% 
    mutate(
      categorical=if_else(grepl("cat",fl),"Categorical","Continuous"),
      weighted=!grepl("unwghtd",fl),
      percent=if_else(grepl("Perc",fl),"Percent of Community","Raw Community"),
      endpoint=if_else(grepl("Abundance",fl),"Abundance","Biomass"),
      equalization=str_split(fl,"Eqlz",simplify = T)[,2]
    ) %>% 
    mutate(equalization=gsub(".rds","",equalization)) %>% 
    mutate(full_endpoint=paste(categorical,percent,endpoint))
  
  split(out %>% select(-fl),out$fl)
}

fl<-list.files(file.path("data","fits"),full.names = T)

perf<-readRDS(fl[grepl("Tune",fl)]) %>% 
  group_by(file) %>% 
  mutate(rank=case_when(
    .metric=="roc_auc" ~ order(mean,decreasing = T),
    T ~ order(mean)
  )) %>% 
  filter(rank==1) %>% 
  mutate(ep=parse_fn(file)) %>% 
  unnest(ep)

ggplot(perf,aes(x=equalization,y=mean,colour=weighted))+
  geom_point(position = position_dodge(0.5))+
  geom_linerange(aes(ymin=mean-std_err,ymax=mean+std_err),position = position_dodge(0.5))+
  facet_grid(categorical~endpoint+percent,scales="free")

fl<-tibble(
  fl=fl[!grepl("Tune",fl)]
  ) %>% 
  mutate(ep=parse_fn(fl)) %>% 
  unnest(ep)


# For Continuous Variables ------------------------------------------------
p1<-ggplot(a2,aes(x=resp_Comm_Abundance,y=.pred,colour=tx_Taxa))+
  geom_abline(slope=1,intercept = 0)+
  geom_smooth(method="lm",se=F)+
  geom_point()

plotly::ggplotly(p1)

a2 %>% 
  group_by(tx_Taxa) %>% 
  summarise(rmse=rmse_vec(resp_Comm_Abundance,.pred),
            rsq=rsq_vec(resp_Comm_Abundance,.pred)
            )

# For categorical variables -----------------------------------------------

pp1<-pROC::multiclass.roc(a1$cat_resp_Comm_Abundance,as.matrix(setNames(a1[,2:7],levels(a1$cat_resp_Comm_Abundance))))
pROC::ggroc(pp1)




# Mapping -----------------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(whitebox)
library(future.apply)
library(ihydro)


# AEC region processing ---------------------------------------------------

a22<-a2 %>% 
  group_by(gen_ProvSegmentID,tx_Taxa) %>% 
  summarize(pred=mean(.pred),
            obs=mean(resp_Comm_Abundance),
            .groups="drop")


aec_region<-lapply(lu_master$stream_packages,function(x) file.path("data","raw","GIS",x))
names(aec_region)<-sapply(names(aec_region),function(x) file.path("data","raw","GIS",x))
# Prepare AEC subregions data
sub_regions<-lapply(names(aec_region),function(aes_nm){
  unzip(aes_nm,exdir=td)
  
  zip_cont<-list.files(file.path(td,gsub(".zip","",basename(aes_nm))),recursive = T,full.names = T)
  sub_regions<-zip_cont[grepl("/w",zip_cont) & grepl("AEC_Class.lyrx",zip_cont)]
  sub_regions<-gsub("_Class\\.lyrx","_Core\\.gdb",sub_regions)
  names(sub_regions)<-sub_regions
  
  sub_regions<-lapply(sub_regions, st_layers)
  
  sub_region_out<-map2(names(sub_regions),sub_regions,function(src,lyr) {read_sf(src,lyr$name[grepl("Reach",lyr$name)])}) #%>% 
    #bind_rows()
  
  # sub_region_out <- sub_region_out %>% 
  #   full_join(a22 %>% filter(gen_ProvSegmentID %in% sub_region_out$ProvSegmentID),by=c("ProvSegmentID"="gen_ProvSegmentID"))

  names(sub_region_out)<-sapply(names(sub_regions),function(x) gsub(".gdb","",basename(x)))
  
  return(sub_region_out)
})
names(sub_regions)<-gsub(".zip","",basename(names(aec_region)))

sub_regions<-unlist(sub_regions,recursive = F)
#saveRDS(sub_regions,"AEC_Streams.rds",compress = F)

sub_regions$AEC_Core_Package02_LakeOntario %>% 
  filter(tx_Taxa=="White Sucker" | is.na(tx_Taxa)) %>% 
  mapview::mapview(.,zcol="pred")



