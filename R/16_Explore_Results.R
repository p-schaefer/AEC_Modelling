library(tidyverse)
fp<-file.path("app","Model_Explore2","data",paste0("Model_data_v2_old.gpkg"))

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
    T ~ x
  )
}

# Predictive Performance --------------------------------------------------
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
                  ~ggplot(.x,
                          aes(x=observed,y=quant_0.5))+
                    geom_point(size=0.5)+
                    geom_abline(slope=1,intercept=0)+
                    geom_smooth(aes(x=observed,y=quant_0.5),se=F,method="gam",colour="black", formula = y ~ splines::bs(x, 3))+
                    geom_smooth(aes(x=observed,y=quant_0.75),se=F,method="gam",colour="blue", formula = y ~ splines::bs(x, 3))+
                    geom_smooth(aes(x=observed,y=quant_0.25),se=F,method="gam",colour="blue", formula = y ~ splines::bs(x, 3))+
                    scale_x_continuous(breaks=scales::pretty_breaks())+
                    scale_y_continuous(breaks=scales::pretty_breaks())+
                    coord_cartesian(xlim=rng_fn(.x),ylim=rng_fn(.x))+
                    theme_bw()+
                    labs(x="Observed",
                         y="Predicted",
                         title=paste0(.y)) +
                    facet_wrap(~endpoint)
  )) %>% 
  group_by(ep_gp ) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(plt=map(data,~cowplot::plot_grid(plotlist = .x$plt,align="hv",axis="tblr")))



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

# Predictor Importance ----------------------------------------------------

sel_modelShap0<-tbl(con,"SHAP_scores")%>% 
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
                  ~ggplot(.x,
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
                    xlab("Importance\n(mean absolute SHAP value | 10th-90th Percentile range)")+
                    theme_bw()+
                    theme(legend.position = "none")
  ))

plt2_predImpAll <- pred_imp %>% 
  mutate(ttl="All",
         sel_tx_Taxa="ALL") %>% 
  group_by(sel_tx_Taxa,ttl) %>% 
  nest() %>% 
  mutate(plt=map2(data,ttl,
                  ~ggplot(.x,
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
                    xlab("Importance\n(mean absolute SHAP value | 10th-90th Percentile range)")+
                    theme_bw()+
                    theme(legend.position = "none")
  ))

# Response Surfaces -------------------------------------------------------

ax_brk<-function(x){
  #browser()
  a1<-scales::transform_pseudo_log()
  a1$breaks(x)
  #a1$minor_breaks(x,c(),3)
  # tns<-scales::pseudo_log_trans()$transform(x)
  # brks<-scales::pretty_breaks(5)(tns)
  # 
  # scales::pseudo_log_trans()$inverse(abs(scales::pretty_breaks(5)(abs(tns))))
  
  #ax_brk<-scales::log_breaks(5)(c(0.3,max(x)))
  #sort(c(-ax_brk,0,ax_brk))
}

ax_lm<-function(x){
  #browser()
  ax_brk<-scales::log_breaks(3)(abs(x))
  range(sort(c(-ax_brk,0,ax_brk)))
}

plt_RespSurf <- tibble(
  #ep=ep,
  taxa=list(taxa),
  pred_names=list(c(pred_names)) #tbl(con,"SHAP_scores") %>% select(starts_with("tx_")) %>% colnames(),
) %>% 
  #unnest(ep) %>% 
  unnest(taxa) %>% 
  unnest(pred_names) %>% 
  mutate(plt=pmap(list(sel_taxa=taxa,shap_pred_sel=pred_names), #sel_ep=ep,
                  function(sel_ep,sel_taxa,shap_pred_sel) {
                    #browser()
                    sel_modelShap <- tbl(con,"SHAP_scores") %>%
                      #filter(endpoint == local(sel_ep)) %>%
                      filter(sel_tx_Taxa == local(sel_taxa)) %>%
                      select(sel_gen_ProvReachID,endpoint,shape_param,
                             all_of(local(shap_pred_sel)),
                             all_of(local(paste0("sel_",shap_pred_sel)))) %>%
                      collect() %>%
                      setNames(c("ProvReachID","endpoint","shape_param","y","x")) %>% 
                      filter(!grepl("Reference",shape_param))
                    
                    
                    ggplot(sel_modelShap,aes(x=x,y=y,colour=endpoint))+
                      #geom_point(size=0.1,alpha=0.05)+
                      geom_hline(yintercept = 0,linetype="dashed",linewidth=0.25)+
                      geom_smooth(aes(x=x,y=y,colour=endpoint),inherit.aes = F,se=T)+
                      labs(
                        x=shap_pred_sel,
                        y="SHAP Score",
                        title=paste(ep_rn(sel_taxa)) #,ep_rn(sel_ep)
                      )+
                      theme_bw()+
                      scale_colour_manual(values = c(RColorBrewer::brewer.pal(3,"Dark2")[1:2]))+
                      scale_y_continuous(transform = "pseudo_log",breaks=ax_brk,labels=scales::comma,limits=ax_lm)+ #,expand=c(0,0)
                      facet_grid(ep_rn(shape_param)~ep_rn(endpoint),scales="free")+
                      theme(legend.position = "bottom")
                  }
  ))

# Database Disconnect -------------------------------------------------------

DBI::dbDisconnect(con)

