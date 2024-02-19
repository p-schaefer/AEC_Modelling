library(shinydashboard)
library(tidyverse)
library(sf)

fp<-file.path("data",paste0("Model_datas.gpkg"))
con <- DBI::dbConnect(RSQLite::SQLite(), fp)

regions<-tbl(con,"Region_names") %>% collect() %>% pull(1)
taxa<-tbl(con,"Taxa_names") %>% collect() %>% pull(1)
pred_names<-tbl(con,"Predictor_names") %>% collect() %>% pull(1)
ep<-list(Biomass = "resp_Comm_Biomass",
         Density = "resp_Comm_Abundance")

DBI::dbDisconnect(con)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  # Tab Contents ------------------------------------------------------------
  
  # map_bio_tab tab content --
  
  output$map_bio <- leaflet::renderLeaflet({
    req(input$sel_region)
    req(input$sel_taxa)
    req(input$sel_ep)
    validate(need(length(input$sel_region)<5,"Select up to 4 regions for mapping"))
    con <- DBI::dbConnect(RSQLite::SQLite(), fp)
    
    sel_strms<-sf::read_sf(fp,
                           query=paste0("SELECT * FROM AEC_Streams WHERE AEC_Region_sub IN ('",paste(input$sel_region,collapse="', '"),"')"))
    
    sel_modelpredictions<-tbl(con,"Model_Predictions") %>% 
      filter(tx_Taxa == local(input$sel_taxa)) %>%
      filter(gen_Region %in% local(input$sel_region)) %>%
      select(gen_ProvReachID,contains(input$sel_ep),contains(pred_names)) %>% 
      collect()
    
    DBI::dbDisconnect(con)
    
    Predicted<-sel_strms %>% 
      left_join(sel_modelpredictions,
                by=c("ProvReachID"="gen_ProvReachID")) %>% 
      #mutate(across(contains(c("quant_","observed","predicted")),~expm1(.x))) %>% 
      rename_with(~gsub(paste0(input$sel_ep,"_"),"",.x)) %>% 
      select(observed,
             P50=quant_0.5,
             p75=quant_0.75,
             p75_ref=quant_0.75_ref,
             p75_refdiff=quant_0.75_refdiff,
             contains(pred_names),
             geom) %>% 
      sf::st_as_sf()
    
    Observed<-Predicted %>%
      select(`log-scale`=observed)
    RefPredicted<-Predicted %>% 
      select(`log-scale`=p75_ref,any_of(paste0(pred_names,"_ref"))) %>%
      rename_with(.cols=any_of(paste0(pred_names,"_ref")),~gsub("_ref","",.x))
    RefDifference<-Predicted %>% 
      select(`log-scale`=p75_refdiff)
    Predicted<-Predicted %>% 
      select(`log-scale`=p75,any_of(paste0(pred_names,"_obs"))) %>% 
      rename_with(.cols=any_of(paste0(pred_names,"_obs")),~gsub("_obs","",.x))
    
    rng<-pretty(range(c(Predicted$`log-scale`,Observed$`log-scale`,RefPredicted$`log-scale`),na.rm=T),n=8)
    
    mx<-max(abs(RefDifference$`log-scale`),na.rm=T)
    
    rng2<-pretty(c(-mx,mx),n=8)
    rng2<-rng2[rng2!=0]
    
    mv<-mapview::mapview(Observed,
                         zcol=c("log-scale"),
                         at=(rng))+
      mapview::mapview(Predicted,
                       zcol=c("log-scale"),
                       at=(rng),
                       legend =T,
                       hide =T)+
      mapview::mapview(RefPredicted,
                       zcol=c("log-scale"),
                       at=(rng),
                       legend =T,
                       hide =T)+
      mapview::mapview(RefDifference,
                       zcol=c("log-scale"),
                       at=(rng2),
                       legend =T,
                       hide =T)
    
    mv@map
  })
  
  # map_pred_tab tab content --
  
  output$map_pred <- leaflet::renderLeaflet({
    req(input$sel_region)
    req(input$sel_taxa)
    req(input$sel_ep)
    validate(need(length(input$sel_region)<5,"Select up to 4 regions for mapping"))
    con <- DBI::dbConnect(RSQLite::SQLite(), fp)
    
    sel_strms<-sf::read_sf(fp,
                           query=paste0("SELECT * FROM AEC_Streams WHERE AEC_Region_sub IN ('",paste(input$sel_region,collapse="', '"),"')"))
    
    sel_modelpredictors<-tbl(con,"Predictor_Data") %>% 
      filter(gen_Region %in% local(input$sel_region)) %>%
      select(gen_ProvReachID,any_of(local(input$mapsel_pred))) %>% 
      collect()
    
    DBI::dbDisconnect(con)
    
    Predictors<-sel_strms %>% 
      left_join(sel_modelpredictors,
                by=c("ProvReachID"="gen_ProvReachID"))%>% 
      select(ProvReachID,Network_Line_Type,any_of(input$mapsel_pred), geom) %>% 
      sf::st_as_sf()
    
    rng<-pretty(range(Predictors[[input$mapsel_pred]],na.rm=T),n=8)
    
    mv<-mapview::mapview(Predictors,
                         zcol=c(input$mapsel_pred),
                         at=rng)
    
    mv@map
  })
  
  # predperf_tab tab content --
  output$predperf_out<-plotly::renderPlotly({
    req(input$sel_region)
    req(input$sel_taxa)
    req(input$sel_ep)
    #validate(need(length(input$sel_region)<5,"Select up to 4 regions"))
    con <- DBI::dbConnect(RSQLite::SQLite(), fp)
    
    sel_ProvReachID<-sf::read_sf(fp,
                                 query=paste0("SELECT * FROM AEC_Streams WHERE AEC_Region_sub IN ('",paste(input$sel_region,collapse="', '"),"')")) %>% 
      pull(ProvReachID)
    
    sel_modelOOSpredictions<-tbl(con,"OOS_Predictions") %>% 
      filter(tx_Taxa == local(input$sel_taxa)) %>% 
      filter(gen_ProvReachID %in% local(sel_ProvReachID)) %>%
      filter(endpoint == local(input$sel_ep)) %>% 
      collect()
    
    DBI::dbDisconnect(con)
    
    ttl<-paste(
      input$sel_taxa,
      case_when(input$sel_ep=="resp_Comm_Biomass"~ "Biomass (g/100m^2)",
                T ~ "Density (individuals/100m^2)")
    )
    
    rng<-range(c(sel_modelOOSpredictions$observed,sel_modelOOSpredictions$quant_0.75),na.rm=T)
    
    rng<-range(pretty(rng))
    
    plt<-ggplot(sel_modelOOSpredictions,
                aes(x=observed,y=quant_0.5))+
      geom_point()+
      geom_abline(slope=1,intercept=0)+
      geom_smooth(aes(x=observed,y=quant_0.5),se=F,method="gam",colour="black")+
      geom_smooth(aes(x=observed,y=quant_0.75),se=F,method="gam",colour="blue")+
      geom_smooth(aes(x=observed,y=quant_0.25),se=F,method="gam",colour="blue")+
      scale_x_continuous(breaks=scales::pretty_breaks())+
      scale_y_continuous(breaks=scales::pretty_breaks())+
      coord_cartesian(xlim=rng,ylim=rng)+
      theme_bw()+
      labs(x="Observed (ln(x+1)-scaled)",
           y="Predicted (ln(x+1)-scaled)",
           title=ttl)
    
    plotly::ggplotly(plt)
    
  })
  # predimp_tab tab content --
  
  output$predimp_out<-plotly::renderPlotly({
    req(input$sel_region)
    req(input$sel_taxa)
    req(input$sel_ep)
    #validate(need(length(input$sel_region)<5,"Select up to 4 regions"))
    con <- DBI::dbConnect(RSQLite::SQLite(), fp)
    
    sel_ProvReachID<-sf::read_sf(fp,
                                 query=paste0("SELECT * FROM AEC_Streams WHERE AEC_Region_sub IN ('",paste(input$sel_region,collapse="', '"),"')")) %>% 
      pull(ProvReachID)
    
    sel_modelShap<-tbl(con,"SHAP_scores")%>% 
      filter(endpoint == local(input$sel_ep)) %>%
      filter(sel_tx_Taxa == local(input$sel_taxa)) %>%
      filter(sel_gen_ProvReachID %in% local(sel_ProvReachID)) %>%
      select(shape_param,any_of(local(pred_names))) %>% 
      group_by(shape_param) %>% 
      summarise(across(everything(),~mean(abs(.x),na.rm=T))) %>% 
      collect() %>% 
      pivot_longer(c(everything(),-shape_param),names_to = "Predictors", values_to = "Importance")
    
    DBI::dbDisconnect(con)
    
    pred_imp<-sel_modelShap
    
    plt<-ggplot(pred_imp,aes(x=Importance,y=Predictors))+
      geom_point()+
      facet_wrap(~shape_param,scales="free_x")+
      xlab("Importance\n(mean absolute SHAP value)")+
      theme_bw()
    
    plotly::ggplotly(plt)
    
  })
  
  # predsurf_tab tab content --
  
  output$predsurf_out<-plotly::renderPlotly({
    req(input$sel_region)
    req(input$sel_taxa)
    req(input$sel_ep)
    req(input$shap_pred_sel)
    req(input$shap_col_sel)
    #validate(need(length(input$sel_region)<5,"Select up to 4 regions"))
    con <- DBI::dbConnect(RSQLite::SQLite(), fp)
    
    sel_ProvReachID<-sf::read_sf(fp,
                                 query=paste0("SELECT * FROM AEC_Streams WHERE AEC_Region_sub IN ('",paste(input$sel_region,collapse="', '"),"')")) %>% 
      pull(ProvReachID)
    
    sel_modelShap<-tbl(con,"SHAP_scores")%>% 
      filter(endpoint == local(input$sel_ep)) %>%
      filter(sel_tx_Taxa == local(input$sel_taxa)) %>%
      filter(sel_gen_ProvReachID %in% local(sel_ProvReachID)) %>% 
      select(shape_param,
             all_of(local(input$shap_pred_sel)),
             all_of(local(paste0("sel_",input$shap_pred_sel))),
             all_of(local(paste0("sel_",input$shap_col_sel)))) %>% 
      collect() %>% 
      setNames(c("shape_param","y","x","colour")) 
    
      
    DBI::dbDisconnect(con)
    
    if (!"colour" %in% colnames(sel_modelShap)) sel_modelShap$colour<-sel_modelShap$x
    
    
    plt<-ggplot(sel_modelShap,aes(x=x,y=y,colour=colour))+
      geom_point()+
      facet_wrap(~shape_param,scales="free")+
      geom_hline(yintercept = 0,linetype="dashed")+
      geom_smooth(aes(x=x,y=y),inherit.aes = F,se=F,colour="black")+
      labs(
        x=input$shap_pred_sel,
        y="SHAP Score",
        colour=input$shap_col_sel
      )+
      theme_bw()
    
    plotly::ggplotly(plt)
    
  })
  
  
  
}
