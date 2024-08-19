library(tidyverse)
fp<-file.path("app","Model_Explore2","data",paste0("Model_data_v2.gpkg"))

con <- DBI::dbConnect(RSQLite::SQLite(), fp)

regions<-tbl(con,"Region_names") %>% collect() %>% pull(1)
taxa<-tbl(con,"Taxa_names") %>% collect() %>% pull(1)
CalcEP<-tbl(con,"CalcEP_names") %>% collect() %>% pull(1)
pred_names<-tbl(con,"Predictor_names") %>% collect() %>% pull(1)
ep<-list(Biomass = "resp_Comm_Biomass",
         Density = "resp_Comm_Abundance")


q25<-function(x) quantile(x,0.10,na.rm = T)
mn<-function(x) mean(x,na.rm=T)
q75<-function(x) quantile(x,0.90,na.rm = T)

ep_rn<-function(x) {
  case_when(
    x=="resp_Comm_Biomass" ~ "Biomass (g/m²)",
    x=="resp_Comm_Abundance" ~ "Density (ind/m²)",
    x=="Atlantic Salmon (ouananiche)" ~ "Atlantic \nSalmon",
    x=="Brook (speckled) Trout" ~ "Brook \nTrout",
    x=="Brook Stickleback" ~ "Brook  \nStickleback",
    x=="Brown Trout" ~ "Brown \nTrout",
    x=="Central Mudminnow" ~ "Central \nMudminnow",
    x=="Common Shiner" ~ "Common \nShiner",
    x=="Creek Chub" ~ "Creek \nChub",
    x=="Fantail Darter" ~ "Fantail \nDarter",
    x=="Johnny/tesselated Darter" ~ "Johnny/tesselated \nDarter",
    x=="Pumpkinseed" ~ "Pumpkinseed",
    x=="Rainbow Darter" ~ "Rainbow \nDarter",
    x=="Rainbow Trout (steelhead)" ~ "Rainbow \nTrout",
    x=="Rock Bass" ~ "Rock \nBass",
    x=="White Sucker" ~ "White \nSucker",
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

rng<-range(c(sel_modelOOSpredictions$observed,sel_modelOOSpredictions$quant_0.75),na.rm=T)

rng<-range(pretty(rng))

plt<-ggplot(sel_modelOOSpredictions,
            aes(x=observed,y=quant_0.5))+
  geom_point(size=0.5)+
  geom_abline(slope=1,intercept=0)+
  geom_smooth(aes(x=observed,y=quant_0.5),se=F,method="gam",colour="black", formula = y ~ splines::bs(x, 2))+
  geom_smooth(aes(x=observed,y=quant_0.75),se=F,method="gam",colour="blue", formula = y ~ splines::bs(x, 2))+
  geom_smooth(aes(x=observed,y=quant_0.25),se=F,method="gam",colour="blue", formula = y ~ splines::bs(x, 2))+
  scale_x_continuous(breaks=scales::pretty_breaks())+
  scale_y_continuous(breaks=scales::pretty_breaks())+
  coord_cartesian(xlim=rng,ylim=rng)+
  theme_bw()+
  labs(x="Observed (ln(x+1)-scaled)",
       y="Predicted (ln(x+1)-scaled)",
       title="Out of Sample Predictions vs. Observed")+
  facet_grid(endpoint~tx_Taxa)

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
  summarise(across(everything(),list(Importance=mn,q25=q25,q75=q75),.names = "{.fn}_{.col}")) %>% 
  pivot_longer(c(everything(),-shape_param,-endpoint,-sel_tx_Taxa,),names_to = "Predictors", values_to = "Importance") %>% 
  mutate(Summary=stringr::str_split(Predictors,"_",n=2,simplify=T)[,1]) %>% 
  mutate(Predictors=str_replace(Predictors,Summary,""))%>% 
  mutate(Predictors=str_replace(Predictors,"^_","")) %>% 
  pivot_wider(names_from=Summary,values_from = Importance)

pred_imp<-sel_modelShap %>% 
  filter(grepl("Current",shape_param)) %>% 
  mutate(shape_param=gsub("Current ","",shape_param))

plt <- pred_imp %>% 
  mutate(ttl=paste(gsub("\\\n","",sel_tx_Taxa))) %>% 
  group_by(sel_tx_Taxa,ttl) %>% 
  nest() %>% 
  mutate(plt=map2(data,ttl,
                 ~ggplot(.x,
                         aes(x=Importance,
                             y=Predictors,
                             xmin=q25,
                             xmax=q75,
                             colour=endpoint))+
                   geom_point(position=position_dodge(width=0.5))+
                   geom_linerange(position=position_dodge(width=0.5))+
                   scale_colour_manual(values = c(RColorBrewer::brewer.pal(3,"Dark2")[1:2]))+
                   facet_wrap(~shape_param,scales="free_x")+
                   labs(title=.y,colour = "")+
                   xlab("Importance\n(mean absolute SHAP value | 10th-90th range)")+
                   theme_bw()))


# Response Surfaces -------------------------------------------------------

sel_modelShap<-tbl(con,"SHAP_scores")%>% 
  filter(endpoint == local(input$sel_ep)) %>%
  filter(sel_tx_Taxa == local(input$sel_taxa)) %>%
  filter(sel_gen_ProvReachID %in% local(sel_ProvReachID)) %>% 
  select(sel_gen_ProvReachID,shape_param,
         all_of(local(input$shap_pred_sel)),
         all_of(local(paste0("sel_",input$shap_pred_sel))),
         all_of(local(paste0("sel_",input$shap_col_sel)))) %>% 
  collect() %>% 
  setNames(c("ProvReachID","shape_param","y","x","colour")) 

ax_brk<-function(x){
  ax_brk<-scales::log_breaks(5)(abs(x))
  sort(c(-ax_brk,0,ax_brk))
}

ax_lm<-function(x){
  #browser()
  ax_brk<-scales::log_breaks(3)(abs(x))
  range(sort(c(-ax_brk,0,ax_brk)))
}

plt<-ggplot(sel_modelShap,aes(x=x,y=y,colour=colour,text=ProvReachID))+
  geom_point()+
  geom_hline(yintercept = 0,linetype="dashed")+
  geom_smooth(aes(x=x,y=y),inherit.aes = F,se=F,colour="black")+
  labs(
    x=input$shap_pred_sel,
    y="SHAP Score",
    colour=input$shap_col_sel
  )+
  theme_bw()+
  theme(text=element_text(size=21))+
  scale_y_continuous(transform = "pseudo_log",breaks=ax_brk,labels=scales::comma)+ #,limits=ax_lm,expand=c(0,0)
  facet_wrap(~shape_param,scales="free",ncol=2)

# Database Disconnect -------------------------------------------------------

DBI::dbDisconnect(con)

