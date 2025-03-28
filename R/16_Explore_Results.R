library(tidyverse)
fp<-file.path("app","Model_Explore2","data",paste0("Model_data_v5_dart_old.gpkg"))

con <- DBI::dbConnect(RSQLite::SQLite(), fp)

regions<-tbl(con,"Region_names") %>% collect() %>% pull(1)
taxa<-tbl(con,"Taxa_names") %>% collect() %>% pull(1)
CalcEP<-tbl(con,"CalcEP_names") %>% collect() %>% pull(1)
pred_names<-tbl(con,"Predictor_names") %>% collect() %>% pull(1)
ep<-list(Biomass = "resp_Comm_Biomass",
         Density = "resp_Comm_Abundance")


q10<-function(x) quantile(x,0.10,na.rm = T)
mn<-function(x) mean(x,na.rm=T)
q90<-function(x) quantile(x,0.90,na.rm = T)

ep_rn<-function(x) {
  case_when(
    x=="resp_Comm_Biomass" ~ "Biomass (g/m²)",
    x=="resp_Comm_Abundance" ~ "Density (ind/m²)",
    x=="Atlantic Salmon (ouananiche)" ~ "Atlantic Salmon",
    x=="Brook (speckled) Trout" ~ "Brook Trout",
    x=="Brook Stickleback" ~ "Brook Stickleback",
    x=="Brown Trout" ~ "Brown Trout",
    x=="Central Mudminnow" ~ "Central Mudminnow",
    x=="Common Shiner" ~ "Common Shiner",
    x=="Creek Chub" ~ "Creek Chub",
    x=="Fantail Darter" ~ "Fantail Darter",
    x=="Johnny/tesselated Darter" ~ "Johnny/Tesselated Darter",
    x=="Pumpkinseed" ~ "Pumpkinseed",
    x=="Rainbow Darter" ~ "Rainbow Darter",
    x=="Rainbow Trout (steelhead)" ~ "Rainbow Trout",
    x=="Rock Bass" ~ "Rock Bass",
    x=="White Sucker" ~ "White Sucker",
    x=="Sculpin Cottus" ~ "Sculpins",
    T ~ x
  )
}

pred_rn<-function(x) {
  case_when(
    x=="tx_Taxa" ~ "Species Interactions",
    x=="tx_Family" ~ "Family",
    x=="tx_Tolerance" ~ "Species Tolerance",
    x=="tx_Trophic_Class" ~ "Species Trophic Class",
    x=="tx_Thermal_Regime" ~ "Species Thermal Requirement",
    x=="tx_General_Habitat" ~ "Species Habitat Preference",
    x=="tx_Environment" ~ "Species Environmental Preference",
    x=="tx_Repro_1" ~ "Species Nest Guarding",
    x=="tx_Repro_2" ~ "Species Spawning Habitat Preference",
    x=="tx_Repro_3" ~ "Species Spawning Methods",
    x=="hb_Temperature" ~ "AEC Modeled Stream Temperature",
    x=="hb_GDDair_UpstreamCatchmentMean" ~ "AEC Air Temperature Growing Degree Days in Catchment",
    x=="hb_Turbidity" ~ "AEC Turbidity",
    x=="hb_Slope" ~ "AEC Channel Slope",
    x=="hb_BFI_RCA" ~ "AEC Baseflow Index in Reach Contributing Area",
    x=="hb_BFI_UCA" ~ "AEC Baseflow Index in Upstream Catchment Area",
    x=="hb_UCA" ~ "AEC Catchment Area",
    x=="hb_Temperature_Class" ~ "AEC Stream Temperature Class",
    x=="hb_Lake_Inf" ~ "AEC Lake Influence",
    x=="hb_Wadeability" ~ "AEC Wadeability",
    x=="nr_BroadleafForest_HAiFLS_prop" ~ "OLCC Deciduous Treed - HAiFLS",
    x=="nr_BroadleafForest_HAiFLO_prop" ~ "OLCC Deciduous Treed - HAiFLO",
    x=="nr_ConiferousForest_HAiFLS_prop" ~ "OLCC Coniferous Treed - HAiFLS",
    x=="nr_ConiferousForest_HAiFLO_prop" ~ "OLCC Coniferous Treed - HAiFLO",
    x=="nr_MixedAgriculture_HAiFLS_prop" ~ "OLCC Agriculture and Undifferentiated Rural - HAiFLS",
    x=="nr_MixedAgriculture_HAiFLO_prop" ~ "OLCC Agriculture and Undifferentiated Rural - HAiFLO",
    x=="nr_MixedForest_HAiFLS_prop" ~ "OLCC Mixed, Sparse, Upland, Plantations Treed - HAiFLS",
    x=="nr_MixedForest_HAiFLO_prop" ~ "OLCC Mixed, Sparse, Upland, Plantations Treed - HAiFLO",
    x=="nr_Water_HAiFLS_prop" ~ "OLCC Clear Open, Turbid Water - HAiFLS",
    x=="nr_Water_HAiFLO_prop" ~ "OLCC Clear Open, Turbid Water - HAiFLO",
    x=="nr_Wetland_HAiFLS_prop" ~ "OLCC Marsh, Swamp, Fen, Bog - HAiFLS",
    x=="nr_Wetland_HAiFLO_prop" ~ "OLCC Marsh, Swamp, Fen, Bog - HAiFLO",
    x=="nr_UrbanDeveloped_HAiFLS_prop" ~ "OLCC Urban Developed - HAiFLS",
    x=="nr_UrbanDeveloped_HAiFLO_prop" ~ "OLCC Urban Developed - HAiFLO",
    x=="nr_ExposedLandBarren_HAiFLS_prop" ~ "OLCC Depletion/Disturbance - HAiFLS",
    x=="nr_ExposedLandBarren_HAiFLO_prop" ~ "OLCC Depletion/Disturbance - HAiFLO",
    x=="LDI_HAiFLS_mean" ~ "Land Disturbance Index - HAiFLS",
    x=="LDI_HAiFLO_mean" ~ "Land Disturbance Index - HAiFLO",
    T ~ x
  )
}

taxa<-tbl(con,"Taxa_names") %>% collect() %>% pull(1)
CalcEP<-tbl(con,"CalcEP_names") %>% collect() %>% pull(1)
pred_names<-tbl(con,"Predictor_names") %>% collect() %>% pull(1)

pred_tbl <- tibble(raw_nm=colnames(tbl(con,"SHAP_scores"))) %>% 
  mutate(Predictor=pred_rn(raw_nm),
         `Predictor Group`=case_when(
           grepl("^AEC",Predictor) ~ "Natural Feature",
           grepl("^OLCC|Land Disturbance",Predictor) ~ "Landcover Feature",
           grepl("tx_",raw_nm) ~ "Taxonomic Feature",
           T ~ NA_character_
         )) %>% 
  filter(!is.na(`Predictor Group`)) %>% 
  filter(!grepl("sel_",raw_nm)) %>% 
  select(-raw_nm) %>% 
  write_csv(file.path("data","report tables","Table x Predictors_all.csv"))

pred_tbl <- tibble(raw_nm=colnames(tbl(con,"SHAP_scores"))) %>% 
  mutate(Predictor=pred_rn(raw_nm),
         `Predictor Group`=case_when(
           grepl("^AEC",Predictor) ~ "Natural Feature",
           grepl("^OLCC|Land Disturbance",Predictor) ~ "Landcover Feature",
           grepl("tx_",raw_nm) ~ "Taxonomic Feature",
           T ~ NA_character_
         )) %>% 
  filter(!is.na(`Predictor Group`)) %>% 
  filter(!grepl("sel_",raw_nm)) %>% 
  select(-raw_nm) %>% 
  mutate(Predictor=gsub(" - HAiFLO| - HAiFLS","",Predictor)) %>% 
  distinct() %>% 
  write_csv(file.path("data","report tables","Table x Predictors_sub.csv"))

tibble(`Community Endpoint`=CalcEP) %>% 
  write_csv(file.path("data","report tables","Table x Derived Endpoints.csv"))

# OOS Predictive Performance --------------------------------------------------
sel_modelOOSpredictions<-tbl(con,"OOS_Predictions") %>% 
  #filter(tx_Taxa == local(input$sel_taxa)) %>% 
  #filter(gen_ProvReachID %in% local(sel_ProvReachID)) %>%
  #filter(endpoint == local(input$sel_ep)) %>% 
  collect() %>% 
  mutate(endpoint=ep_rn(endpoint),
         tx_Taxa=ep_rn(tx_Taxa))

rng_fn<-function(x){
  rng<-range(c(x$observed,x$quant_0.75),na.rm=T)
  rng<-range(pretty(rng))
  rng
}

r2_fun<-function(x,taxa){
  if (taxa %in% c(
    "Atlantic Salmon",
    "Brook Trout",
    "Brook Stickleback",
    "Brown Trout",
    "Central Mudminnow",
    "Common Shiner",
    "Creek Chub",
    "Fantail Darter",
    "Johnny/Tesselated Darter",
    "Tesselated Darter",
    "Pumpkinseed",
    "Rainbow Darter",
    "Rainbow Trout",
    "Rock Bass",
    "White Sucker",
    "Sculpins"
  )) {
    return(
      x %>% 
        group_by(endpoint) %>% 
        summarise(R2=cor(observed,quant_0.75,method = "spearman",use = "pairwise.complete.obs")^2,
                  AUC=pROC::auc(observed>0,1-gate)[[1]],
                  .groups = "drop") %>% 
        mutate(val=paste0(endpoint," R²/AUC = ",scales::number(R2,accuracy=0.01),"/",scales::number(AUC,accuracy=0.01))) %>% 
        pull(val) %>% 
        paste0(collapse = "\n")
    )
  } else  {
    return(
      x %>% 
        group_by(endpoint) %>% 
        summarise(R2=cor(observed,quant_0.75,method = "spearman",use = "pairwise.complete.obs")^2,
                  .groups = "drop") %>% 
        mutate(val=paste0(endpoint," R² = ",scales::number(R2,accuracy=0.01))) %>% 
        pull(val) %>% 
        paste0(collapse = "\n")
    )
  }
}

plt_1to1 <- sel_modelOOSpredictions %>% 
  mutate(across(`quant_0.05`:observed,
                ~case_when(
                  tx_Taxa == "SATI" & (observed == 0 | quant_0.75 ==0) ~ NA_real_,
                  T ~ .
                ))) %>% 
  group_by(tx_Taxa) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(ep_gp=case_when(
    tx_Taxa %in% CalcEP ~ "Derived",
    T ~ "Taxa"
  )) %>% 
  mutate(plt=map2(data,tx_Taxa,
                  ~ggplot(.x ,
                          aes(x=observed,y=quant_0.75))+
                    geom_point(size=0.1,alpha=0.1)+
                    geom_abline(slope=1,intercept=0,linewidth=0.25)+
                    geom_smooth(aes(x=observed,y=quant_0.75),se=F,method="lm",colour="blue")+
                    #geom_quantile(aes(x=observed,y=quant_0.75),quantiles = c(0.05,0.5,0.95),method="rqss",colour="blue",lambda=10)+
                    #geom_smooth(aes(x=observed,y=quant_0.75),se=F,method="gam",colour="blue", formula = y ~ splines::bs(x, 3))+
                    #geom_smooth(aes(x=observed,y=quant_0.75),se=F,method="gam",colour="blue", formula = y ~ splines::bs(x, 3))+
                    #geom_smooth(aes(x=observed,y=quant_0.84),se=F,method="gam",colour="blue", formula = y ~ splines::bs(x, 3))+
                    #geom_smooth(aes(x=observed,y=quant_0.66),se=F,method="gam",colour="blue", formula = y ~ splines::bs(x, 3))+
                    scale_x_continuous(breaks=scales::pretty_breaks())+
                    scale_y_continuous(breaks=scales::pretty_breaks())+
                    coord_cartesian(xlim=rng_fn(.x),ylim=rng_fn(.x))+
                    theme_bw()+
                    labs(x="Observed",
                         y="Predicted",
                         title=ep_rn(paste0(.y)),
                         caption = r2_fun(.x,.y)) +
                    facet_wrap(~endpoint)
  )) %>% 
  group_by(ep_gp ) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(plt=map(data,~cowplot::plot_grid(plotlist = .x$plt,align="hv",axis="tblr")))

ggsave(file.path("Figs","Fig 1 1to1 Taxa.pdf"),plt_1to1$plt[[1]],height=8.5,width=11)
ggsave(file.path("Figs","Fig 1 1to1 Derived.pdf"),plt_1to1$plt[[2]],height=8.5,width=11)

plt_1to1$data[[1]]$plt[[8]]
plt_1to1$data[[2]]$plt[[4]]

# plt<-ggplot(sel_modelOOSpredictions,
#             aes(x=observed,y=quant_0.5))+
#   geom_point(size=0.5)+
#   geom_abline(slope=1,intercept=0)+
#   geom_smooth(aes(x=observed,y=quant_0.5),se=F,method="gam",colour="black", formula = y ~ splines::bs(x, 2))+
#   geom_smooth(aes(x=observed,y=quant_0.75),se=F,method="gam",colour="blue", formula = y ~ splines::bs(x, 2))+
#   geom_smooth(aes(x=observed,y=quant_0.25),se=F,method="gam",colour="blue", formula = y ~ splines::bs(x, 2))+
#   scale_x_continuous(breaks=scales::pretty_breaks())+
#   scale_y_continuous(breaks=scales::pretty_breaks())+
#   coord_cartesian(xlim=rng,ylim=rng)+
#   theme_bw()+
#   labs(x="Observed (ln(x+1)-scaled)",
#        y="Predicted (ln(x+1)-scaled)",
#        title="Out of Sample Predictions vs. Observed")+
#   facet_grid(endpoint~tx_Taxa,scales="free")

# do for calculated endpoints as well


# Predictive Performance 2 ------------------------------------------------

sel_modelISpredictions <- tbl(con,"Model_Predictions") %>% 
  filter(!is.na(resp_Comm_Biomass_observed)) %>% 
  select(tx_Taxa,gen_ProvReachID,contains("_quant_"),resp_Comm_Biomass_observed,resp_Comm_Abundance_observed) %>% 
  pivot_longer(c(contains("_quant_"),resp_Comm_Biomass_observed,resp_Comm_Abundance_observed)) %>% 
  collect() %>% 
  mutate(endpoint=case_when(
    grepl("Biomass",name) ~ "resp_Comm_Biomass",
    grepl("Abundance",name) ~ "resp_Comm_Abundance"
  )) %>% 
  mutate(name=gsub("resp_Comm_Biomass_|resp_Comm_Abundance_","",name)) %>% 
  pivot_wider() %>% 
  mutate(endpoint=ep_rn(endpoint),
         tx_Taxa=ep_rn(tx_Taxa))

plt_1to1 <- sel_modelISpredictions %>% 
  mutate(across(`quant_0.05`:observed,
                ~case_when(
                  tx_Taxa == "SATI" & (observed == 0 | quant_0.75 ==0) ~ NA_real_,
                  T ~ .
                ))) %>% 
  group_by(tx_Taxa) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(ep_gp=case_when(
    tx_Taxa %in% CalcEP ~ "Derived",
    T ~ "Taxa"
  )) %>% 
  mutate(plt=map2(data,tx_Taxa,
                  ~ggplot(.x,
                          aes(x=observed,y=quant_0.75))+
                    geom_point(size=0.1,alpha=0.1)+
                    geom_abline(slope=1,intercept=0)+
                    geom_smooth(aes(x=observed,y=quant_0.75),se=F,method="gam",colour="black", formula = y ~ splines::bs(x, 3))+
                    geom_smooth(aes(x=observed,y=quant_0.84),se=F,method="gam",colour="blue", formula = y ~ splines::bs(x, 3))+
                    geom_smooth(aes(x=observed,y=quant_0.66),se=F,method="gam",colour="blue", formula = y ~ splines::bs(x, 3))+
                    scale_x_continuous(breaks=scales::pretty_breaks())+
                    scale_y_continuous(breaks=scales::pretty_breaks())+
                    coord_cartesian(xlim=rng_fn(.x),ylim=rng_fn(.x))+
                    theme_bw()+
                    labs(x="Observed",
                         y="Predicted",
                         title=ep_rn(paste0(.y))) +
                    facet_wrap(~endpoint)
  )) %>% 
  group_by(ep_gp ) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(plt=map(data,~cowplot::plot_grid(plotlist = .x$plt,align="hv",axis="tblr")))

ggsave(file.path("Figs","Fig 1 1to1 in sample Taxa.pdf"),plt_1to1$plt[[1]],height=8.5,width=11)
ggsave(file.path("Figs","Fig 1 1to1 in sample Derived.pdf"),plt_1to1$plt[[2]],height=8.5,width=11)
# Predictor Importance ----------------------------------------------------

sel_modelShap0<-tbl(con,"SHAP_scores") %>% 
  #filter(endpoint == local(input$sel_ep)) %>%
  #filter(sel_tx_Taxa == local(input$sel_taxa)) %>%
  #filter(sel_gen_ProvReachID %in% local(sel_ProvReachID)) %>%
  select(endpoint,sel_tx_Taxa,shape_param,starts_with("tx_"),any_of(local(pred_names))) %>% 
  group_by(endpoint,sel_tx_Taxa,shape_param) %>% 
  mutate(across(everything(),~abs(.x))) %>% 
  collect() 

sel_modelShap <- sel_modelShap0 %>% 
  mutate(endpoint=ep_rn(endpoint),
         sel_tx_Taxa=ep_rn(sel_tx_Taxa)) %>% 
  summarise(across(everything(),list(Importance=mn,q10=q10,q90=q90),.names = "{.fn}_{.col}")) %>% 
  pivot_longer(c(everything(),-shape_param,-endpoint,-sel_tx_Taxa,),names_to = "Predictors", values_to = "Importance") %>% 
  mutate(Summary=stringr::str_split(Predictors,"_",n=2,simplify=T)[,1]) %>% 
  mutate(Predictors=str_replace(Predictors,Summary,""))%>% 
  mutate(Predictors=str_replace(Predictors,"^_","")) %>% 
  pivot_wider(names_from=Summary,values_from = Importance)

pred_imp<-sel_modelShap %>% 
  filter(grepl("Current",shape_param)) %>% 
  mutate(shape_param=gsub("Current ","",shape_param))

plt_predImp <- pred_imp %>% 
  mutate(ttl=paste(gsub("\\\n","",sel_tx_Taxa))) %>% 
  group_by(sel_tx_Taxa,ttl) %>% 
  nest() %>% 
  mutate(plt=map2(data,ttl,
                  ~ggplot(.x %>% mutate(Predictors=pred_rn(Predictors)),
                          aes(x=Importance,
                              y=Predictors,
                              xmin=q10,
                              xmax=q90,
                              colour=endpoint))+
                    geom_point(position=position_dodge(width=0.5))+
                    geom_linerange(position=position_dodge(width=0.5))+
                    scale_colour_manual(values = c(RColorBrewer::brewer.pal(3,"Dark2")[1:2]))+
                    facet_wrap(~shape_param,scales="free_x")+
                    labs(title=.y,colour = "")+
                    xlab("Importance\n(mean absolute SHAP value | 10th-90th percentile range)")+
                    theme_bw()+
                    theme(legend.position = "bottom")
  ))

a<-map2(plt_predImp$ttl, plt_predImp$plt, ~ggsave(file.path("Figs",paste0("Fig 2. PredImp ",gsub("\\/","",.x),".pdf")),.y,height=8.5,width=11))

plt2_predImpAll <- pred_imp %>% 
  mutate(ttl=endpoint) %>% 
  group_by(ttl) %>% 
  nest() %>% 
  mutate(plt=map2(data,ttl,
                  ~ggplot(.x %>% mutate(Predictors=pred_rn(Predictors)),
                          aes(x=Importance,
                              y=Predictors,
                              xmin=q10,
                              xmax=q90,
                              colour=sel_tx_Taxa,
                              group = sel_tx_Taxa))+
                    geom_linerange(position=position_dodge(width=0.75),linewidth=0.25)+
                    geom_point(position=position_dodge(width=0.75))+
                    scale_colour_manual(values = c(RColorBrewer::brewer.pal(12,"Paired"),"gray50"))+
                    facet_wrap(~shape_param,scales="free_x")+
                    labs(title=.y,colour = "")+
                    xlab("Importance\n(mean absolute SHAP value | 10th-90th Percentile range)")+
                    theme_bw()+
                    theme(legend.position = "right")
  ))

a<-map2(plt2_predImpAll$ttl, plt2_predImpAll$plt, ~ggsave(file.path("Figs",paste0("Fig 2. PredImp ",gsub("\\/","",.x)," All Taxa.pdf")),.y,height=8.5,width=11))

# Response Surfaces -------------------------------------------------------

ax_brk<-function(x){
  ax_brk<-scales::pretty_breaks(5)(x)
  unique(sort(c(-ax_brk,0,ax_brk)))
}

ax_lm<-function(x){
  #browser()
  ax_brk<-scales::pretty_breaks(5)(x)
  range(sort(c(-ax_brk,0,ax_brk)))
}

plt_RespSurf <- tibble(
  #ep=ep,
  taxa=list(taxa),
  pred_names=list(c(pred_names)) #tbl(con,"SHAP_scores") %>% select(starts_with("tx_")) %>% colnames(),
) %>% 
  #unnest(ep) %>% 
  unnest(taxa) %>% 
  unnest(pred_names) 

plt_RespSurf <- plt_RespSurf %>% 
  mutate(plt=pmap(list(sel_taxa=taxa,shap_pred_sel=pred_names), #sel_ep=ep,
                  function(sel_ep,sel_taxa,shap_pred_sel) {
                    browser()
                    
                    sel_modelShap <- tbl(con,"SHAP_scores") %>%
                      #filter(endpoint == local(sel_ep)) %>%
                      filter(sel_tx_Taxa == local(sel_taxa)) %>%
                      select(sel_gen_ProvReachID,endpoint,shape_param,
                             all_of(local(shap_pred_sel)),
                             all_of(local(paste0("sel_",shap_pred_sel)))) %>%
                      collect() %>%
                      setNames(c("ProvReachID","endpoint","shape_param","y","x")) %>% 
                      filter(!grepl("Reference",shape_param)) %>% 
                      mutate(shape_param=gsub("Current ","",shape_param)) %>% 
                      filter(!is.na(y)) %>% 
                      filter(!is.na(x)) %>% 
                      filter(x!="NA") 
                    
                    knots <- 3
                    if (is.character(sel_modelShap$x[[1]])){
                      sel_modelShap <- sel_modelShap %>% 
                        mutate(x_lab=x,
                               x=as.numeric(factor(x)))
                      
                      knots <- length(unique(sel_modelShap$x))
                    } 
                    
                    
                    
                    mod_frame<-sel_modelShap %>% 
                      select(shape_param,endpoint,y,x) %>% 
                      distinct() %>% 
                      group_by(shape_param,endpoint) %>% 
                      nest() %>% 
                      mutate(mod=map(data,function(zzz){
                        mod <- try(mgcv::gam(y~s(x, bs = "cs",k=knots), data=zzz ),silent=T)
                        if (inherits(mod,"try-error")){
                          mod <- try(loess(y~x, data=zzz ),silent=T)
                          
                          xrange <- range(sel_modelShap$x,na.rm=T)
                          xseq <- seq(from=xrange[1], to=xrange[2], length=100)
                          pred <- predict(mod, newdata = data.frame(x = xseq), se=F)
                          y = pred
                          ci <- NA_real_
                          ymin = NA_real_
                          ymax = NA_real_
                          return(data.frame(x = xseq, y, ymin, ymax, se = NA_real_))
                        }
                        if (inherits(mod,"try-error")){
                          return(data.frame(x = NA_real_, y=NA_real_, ymin=NA_real_, ymax=NA_real_, se = NA_real_)[F,])
                        }
                        xrange <- range(sel_modelShap$x,na.rm=T)
                        xseq <- seq(from=xrange[1], to=xrange[2], length=100)
                        pred <- predict(mod, newdata = data.frame(x = xseq), se=T)
                        y = pred$fit
                        ci <- pred$se.fit * qt(0.95 / 2 + .5, 10)
                        ymin = y - ci
                        ymax = y + ci
                        data.frame(x = xseq, y, ymin, ymax, se = pred$se.fit)
                      })) %>% 
                      select(-data) %>% 
                      unnest(mod)
                    
                    plt<-ggplot(sel_modelShap,aes(x=x,y=y,colour=endpoint))+
                      geom_point(size=0.1,alpha=0.01)+
                      geom_hline(yintercept = 0,linetype="dashed",linewidth=0.25)+
                      geom_smooth(aes_auto(mod_frame), data=mod_frame, stat="identity")+
                      #geom_smooth(aes(x=x,y=y,colour=endpoint),inherit.aes = F,se=T,method="gam")+ #,formula = y ~ s(x,bs="ps")
                      labs(
                        x=pred_rn(shap_pred_sel),
                        y="SHAP Score",
                        title=paste(ep_rn(sel_taxa)) #,ep_rn(sel_ep)
                      )+
                      theme_bw()+
                      scale_colour_manual(values = c(RColorBrewer::brewer.pal(3,"Dark2")[1:2]))+
                      scale_y_continuous(#labels=scales::comma,
                        #breaks=ax_brk,
                        limits=ax_lm)+ #,expand=c(0,0) breaks=ax_brk,
                      facet_grid(ep_rn(shape_param)~ep_rn(endpoint),scales="free")+
                      theme(legend.position = "none")
                    
                    if (any(colnames(sel_modelShap)=="x_lab")){
                      plt <- plt +
                        scale_x_continuous(labels = sel_modelShap %>% select(x,x_lab) %>% distinct() %>% arrange(x) %>% pull(x_lab),
                                           breaks = sel_modelShap %>% select(x,x_lab) %>% distinct() %>% arrange(x) %>% pull(x))
                    }
                    
                    return(plt)
                  }
  ))

plt_RespSurf2 <- plt_RespSurf %>% 
  mutate(plt_tbl=map(plt,~.x$layers[[3]]$data)) %>% 
  select(-plt) %>% 
  unnest(plt_tbl)

saveRDS(plt_RespSurf2,file.path("Figs","Fig 3. PredSirf.rds"))
plt_RespSurf2<-readRDS(file.path("Figs","Fig 3. PredSirf.rds"))

gp_plt<-plt_RespSurf2 %>%
  group_by(shape_param,endpoint) %>% 
  nest() %>% 
  mutate(plt_nm=paste(ep_rn(endpoint),shape_param)) %>% 
  mutate(plot=map(data,
                  ~ggplot(.x,aes(x=x,y=y,colour=ep_rn(taxa)))+
                    geom_hline(yintercept = 0,linetype="dashed",linewidth=0.25)+
                    geom_smooth(stat="identity")+
                    labs(colour="Taxa",y="SHAP")+
                    facet_wrap(~pred_rn(pred_names),scales = "free")+
                    scale_colour_manual(values = c(RColorBrewer::brewer.pal(12,"Paired"),"grey"))+
                    scale_y_continuous(limits=ax_lm)+
                    theme_bw()+
                    theme(legend.position = "bottom")))

a<-map2(gp_plt$plt_nm,gp_plt$plot,
        ~ggsave(file.path("Figs",paste0("Fig 3. PredSurf ",gsub("\\/","",.x),".pdf")),.y,height=11.5,width=17))



# Individual Response Surfaces --------------------------------------------

sel_taxa<-c("Brook (speckled) Trout","White Sucker","Rock Bass","Sculpin Cottus")
shap_pred_sel<-c("LDI_HAiFLS_mean","hb_Turbidity","hb_GDDair_UpstreamCatchmentMean") #pred_names
#shap_pred_sel<-"tx_Taxa"
sel_ep<-"resp_Comm_Abundance"
sel_shape_param<-"Current Presence/Absence"

sel_modelShap1 <- tbl(con,"SHAP_scores") %>%
  filter(shape_param == local(sel_shape_param)) %>%
  filter(endpoint == local(sel_ep)) %>%
  filter(sel_tx_Taxa %in% local(c(sel_taxa))) %>%
  select(sel_gen_ProvReachID,sel_gen_Region,sel_tx_Taxa,endpoint,shape_param,
         all_of(local(shap_pred_sel)),
         #all_of(local(paste0("sel_",shap_pred_sel)))
  ) %>%
  pivot_longer(all_of(local(shap_pred_sel))) %>% 
  collect() %>%
  setNames(c("ProvReachID","gen_Region","Taxa","endpoint","shape_param","y_nm","y")) %>% 
  filter(!grepl("Reference",shape_param)) %>% 
  mutate(shape_param=gsub("Current ","",shape_param)) 

sel_modelShap2 <- tbl(con,"SHAP_scores") %>%
  filter(shape_param == local(sel_shape_param)) %>%
  filter(endpoint == local(sel_ep)) %>%
  filter(sel_tx_Taxa %in% local(c(sel_taxa))) %>%
  select(sel_gen_ProvReachID,sel_gen_Region,sel_tx_Taxa,endpoint,shape_param,
         #all_of(local(shap_pred_sel)),
         all_of(local(paste0("sel_",shap_pred_sel)))
  ) %>%
  pivot_longer(all_of(local(paste0("sel_",shap_pred_sel)))) %>% 
  collect() %>%
  setNames(c("ProvReachID","gen_Region","Taxa","endpoint","shape_param","y_nm","x")) %>% 
  filter(!grepl("Reference",shape_param)) %>% 
  mutate(shape_param=gsub("Current ","",shape_param)) %>% 
  mutate(y_nm=gsub("sel_","",y_nm))


sel_modelShap<-left_join(sel_modelShap1,sel_modelShap2) %>% 
  filter(!is.na(y)) %>% 
  filter(!is.na(x)) %>% 
  filter(x!="NA") %>% 
  filter(gen_Region %in% c("w03_Lake_Ontario_West","w01_Lake_Erie_West","w22_Lake_Superior_Lake_Nipigon","w14_Georgian_Bay_South_Simcoe")) %>% 
  mutate(gen_Region=gsub("w\\d\\d_","",gen_Region))%>% 
  mutate(gen_Region=gsub("_Lake_Nipigon","",gen_Region)) %>% 
  mutate(gen_Region=gsub("_South_Simcoe","",gen_Region)) %>% 
  mutate(gen_Region=gsub("_"," ",gen_Region)) 

plt<-ggplot(sel_modelShap,aes(x=x,y=y,colour=ep_rn(Taxa)))+
  geom_point(size=0.1,alpha=0.01)+
  geom_hline(yintercept = 0,linetype="dashed",linewidth=0.25)+
  geom_smooth(aes(x=x,y=y,colour=ep_rn(Taxa)),inherit.aes = F,se=T,method="gam", formula = y ~ splines::bs(x, 3))+ #,formula = y ~ s(x,bs="ps")
  labs(
    x="",
    #x=pred_rn(shap_pred_sel),
    y="SHAP Score",
    colour="Taxa",
    title="SHAP Contributions to Presence/Absence"
    #title=paste(ep_rn(sel_taxa)) #,ep_rn(sel_ep)
  )+
  theme_bw()+
  scale_colour_manual(values = c(RColorBrewer::brewer.pal(length(sel_taxa),"Dark2")))+
  scale_y_continuous(#labels=scales::comma,
    #breaks=ax_brk,
    limits=ax_lm)+ #,expand=c(0,0) breaks=ax_brk,
  facet_grid(gen_Region~pred_rn(y_nm),scales="free") +
  #facet_grid(ep_rn(shape_param)~ep_rn(endpoint),scales="free")+
  theme(legend.position = "bottom")

# Maps --------------------------------------------------------------------


sel_strms <- sf::read_sf(fp,"AEC_Streams") %>% 
  sf::st_transform(4326)

sel_modelpredictions<-tbl(con,"Model_Predictions") %>% 
  filter(tx_Taxa == "Brook (speckled) Trout") %>%
  select(gen_ProvReachID,contains("Biomass"),contains(pred_names)) %>% 
  collect()

out<- sel_strms %>% 
  left_join(sel_modelpredictions,
            by=c("ProvReachID"="gen_ProvReachID")) %>% 
  #mutate(across(contains(c("quant_","observed","predicted")),~expm1(.x))) %>% 
  rename_with(~gsub(paste0("resp_Comm_Biomass","_"),"",.x)) %>% 
  select(ProvReachID,
         observed,
         p50=quant_0.75,
         p50_ref=quant_0.75_ref,
         p50_refdiff=quant_0.75_refdiff,
         contains(pred_names),
         geom) %>% 
  mutate(
    `Observed`=observed,
    `Predicted - Reference`=p50_ref,
    `Predicted - Current`=p50,
    `(Current - Reference)`=p50_refdiff
  ) %>% 
  filter(!is.na(p50)) %>% 
  sf::st_as_sf() %>% 
  sf::st_cast("LINESTRING")

col_pred<-leaflet::colorBin("viridis", 
                            bins = c(0,quantile(out$p50[out$p50>0],probs = seq(0, 1, length.out = 8),na.rm=T)), 
                            na.color = "grey",
                            reverse=F)

map_bio_pred <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE,
                                                                   zoomSnap = 0.25,
                                                                   zoomDelta = 1)) %>%
  leaflet::addTiles() %>%
  #leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group ="ESRI - Imagery") %>%
  #leaflet::addProviderTiles(leaflet::providers$OpenStreetMap.Mapnik, group ="OpenStreetMap") %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group ="CartoDB") %>% 
  leaflet::addLayersControl(
    # baseGroups = c("CartoDB",
    #                "OpenStreetMap",
    #                "ESRI - Imagery"),
    position = "topleft",
    options = leaflet::layersControlOptions(collapsed = F)
  ) %>%
  leafgl::addGlPolylines(
    data=out,
    weight=0.1,
    opacity=0.9,
    src =F,
    col=~col_pred(out$p50)
  )

map_bio_obs <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE,
                                                                  zoomSnap = 0.25,
                                                                  zoomDelta = 1)) %>%
  leaflet::addTiles() %>%
  #leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group ="ESRI - Imagery") %>%
  #leaflet::addProviderTiles(leaflet::providers$OpenStreetMap.Mapnik, group ="OpenStreetMap") %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group ="CartoDB") %>% 
  leaflet::addLayersControl(
    # baseGroups = c("CartoDB",
    #                "OpenStreetMap",
    #                "ESRI - Imagery"),
    position = "topleft",
    options = leaflet::layersControlOptions(collapsed = F)
  ) %>% 
  leafgl::addGlPolylines(
    data=out,
    weight=0.1,
    opacity=0.9,
    src =F,
    col=~col_pred(out$observed)
  )

if (F) { # Predictor variable summaries
  pred_names<-tbl(con,"Predictor_names") %>% collect() %>% pull(1)
  
  sel_modelpredictors<-tbl(con,"Predictor_Data") %>% 
    collect()
  
  sel_modelpredictions<-tbl(con,"Model_Predictions") %>% 
    filter(tx_Taxa == "Brook (speckled) Trout") %>% 
    collect()
  
  obs_reaches <- sel_modelpredictions$gen_ProvReachID[!is.na(sel_modelpredictions$resp_Comm_Biomass_observed)]
  all_reaches <- sel_modelpredictions$gen_ProvReachID
  
  df <- bind_rows(
    sel_modelpredictors %>% 
      filter(gen_ProvReachID %in% obs_reaches) %>% 
      select(all_of(pred_names),-where(is.character)) %>% 
      pivot_longer(everything()) %>% 
      group_by(name) %>% 
      summarise(
        mean=mean(value,na.rm=T),
        sd=sd(value,na.rm=T),
        p2.5=quantile(value,0.025,na.rm=T),
        p97.5=quantile(value,0.975,na.rm=T)
      ) %>% 
      ungroup() %>% 
      mutate(name=pred_rn(name)) %>% 
      mutate(subset="Sampled Segments"),
    sel_modelpredictors %>% 
      filter(!gen_ProvReachID %in% obs_reaches) %>% 
      select(all_of(pred_names),-where(is.character)) %>% 
      pivot_longer(everything()) %>% 
      group_by(name) %>% 
      summarise(
        mean=mean(value,na.rm=T),
        sd=sd(value,na.rm=T),
        p50=quantile(value,0.5,na.rm=T),
        p2.5=quantile(value,0.025,na.rm=T),
        p97.5=quantile(value,0.975,na.rm=T)
      ) %>% 
      ungroup() %>% 
      mutate(name=pred_rn(name)) %>% 
      mutate(subset="Unsampled Segments")
  )
  
  df %>% 
    mutate(num=paste0(scales::number(p2.5,accuracy=0.01)," (",scales::number(p2.5,accuracy=0.01)," ",scales::number(p97.5,accuracy=0.01),")")) %>% 
    select(name,num,subset) %>% 
    pivot_wider(names_from = subset,values_from = num) %>% 
    write_csv(file.path("Figs","Predictor Summaries.csv"))

}

if (F) {
  pred_names
  
  sel_modelShap <- tbl(con,"SHAP_scores") %>% 
    select(tx_Taxa,endpoint,shape_param,sel_tx_Taxa) %>% 
    collect() %>% 
    filter(!grepl("Reference",shape_param)) %>% 
    mutate(sel_tx_Taxa=ep_rn(sel_tx_Taxa)) %>% 
    mutate(endpoint=ep_rn(endpoint)) %>% 
    mutate(shape_param=gsub("Current ","",shape_param)) %>% 
    mutate(tx_Taxa=abs(tx_Taxa)) %>% 
    group_by(sel_tx_Taxa,endpoint,shape_param) %>% 
    summarise(
      mean=mean(tx_Taxa,na.rm=T),
      sd=sd(tx_Taxa,na.rm=T),
      p50=quantile(tx_Taxa,0.5,na.rm=T),
      p2.5=quantile(tx_Taxa,0.025,na.rm=T),
      p97.5=quantile(tx_Taxa,0.975,na.rm=T)
    ) 
  
  sel_modelShap %>% 
    mutate(num=paste0(scales::number(p2.5,accuracy=0.01)," (",scales::number(p2.5,accuracy=0.01)," ",scales::number(p97.5,accuracy=0.01),")")) %>% 
    select(Taxa=sel_tx_Taxa,Endpoint=endpoint,num,shape_param) %>% 
    pivot_wider(names_from = shape_param,values_from = num) %>% 
    write_csv(file.path("Figs","Taxa Shap Summaries.csv"))
}

# Database Disconnect -------------------------------------------------------

DBI::dbDisconnect(con)

