library(shinydashboard)
library(tidyverse)
library(sf)

strm<-readRDS("data/AEC_Streams.rds")
pred_names<-readRDS("data/pred_names.rds")

# Define server logic required to draw a histogram
function(input, output, session) {
  
  # Setup Data --------------------------------------------------------------
  
  data_object<-reactiveValues(
    sel_strms=NULL,
    sel_modelpredictions=NULL,
    sel_modelOOSpredictions=NULL,
    sel_modelpredictors=NULL,
    sel_modelShap=NULL
  )
  
  observeEvent(
    c(input$sel_region,input$sel_taxa,input$sel_ep),
    {
      req(input$sel_region)
      req(input$sel_taxa)
      req(input$sel_ep)
      
      validate(need(length(input$sel_region)<5,"Select up to 4 regions"))

      sel_w<-str_split(input$sel_region,"_",2,simplify = T)[,1]
      data_object$sel_strms<-bind_rows(strm[grepl(paste(sel_w,collapse = "|"),names(strm))]) %>% 
        unnest(cols = c(stream))
      
      data_object$sel_modelpredictions<-data.table::fread("data/Model_predictions.csv") %>% 
        filter(tx_Taxa == input$sel_taxa) %>% 
        filter(gen_Region %in% input$sel_region) %>% 
        select(gen_ProvReachID,contains(input$sel_ep))
      
      data_object$sel_modelOOSpredictions<-data.table::fread("data/OOS_Pred.csv") %>% 
        filter(tx_Taxa == input$sel_taxa) %>% 
        filter(gen_ProvReachID %in% data_object$sel_strms$ProvReachID) %>% 
        filter(endpoint == (input$sel_ep))
      
      data_object$sel_modelpredictors<-data.table::fread("data/Predictor_data.csv") %>% 
        filter(gen_Region %in% input$sel_region) %>% 
        select(-contains("resp_"))
      
      data_object$sel_modelShap<-data.table::fread("data/shap.csv") %>% 
        filter(endpoint == (input$sel_ep)) %>% 
        filter(sel_tx_Taxa == input$sel_taxa) %>% 
        filter(sel_gen_ProvReachID %in% data_object$sel_strms$ProvReachID) %>% 
        select(-contains("resp_"),-endpoint)
    }
  )
  
  # Setup Selectors ---------------------------------------------------------
  
  
  # Tab Contents ------------------------------------------------------------

  # map_bio_tab tab content --
  
  output$map_bio <- leaflet::renderLeaflet({
    req(data_object$sel_modelpredictions)

    Predicted<-data_object$sel_strms %>% 
      left_join(data_object$sel_modelpredictions,
                by=c("ProvReachID"="gen_ProvReachID")) %>% 
      #mutate(across(contains(c("quant_","observed","predicted")),~expm1(.x))) %>% 
      rename_with(~gsub(paste0(input$sel_ep,"_"),"",.x)) %>% 
      select(observed,P50=quant_0.5,p75=quant_0.75,Shape) %>% 
      sf::st_as_sf()
    
    Observed<-Predicted %>% select(`log-scale`=observed)
    Predicted50<-Predicted %>% select(`log-scale`=P50)
    Predicted75<-Predicted %>% select(`log-scale`=p75)
    
    rng<-pretty(range(c(Predicted75$`log-scale`,Observed$`log-scale`),na.rm=T),n=8)
    
    mv<-mapview::mapview(Observed,
                     zcol=c("log-scale"),
                     at=(rng))+
      mapview::mapview(Predicted50,
                       zcol=c("log-scale"),
                       at=(rng),
                       legend =T,
                       hide =T)+
      mapview::mapview(Predicted75,
                       zcol=c("log-scale"),
                       at=(rng),
                       legend =T,
                       hide =T)
    
    mv@map
  })
  
  # map_pred_tab tab content --
  
  output$map_pred <- leaflet::renderLeaflet({
    req(data_object$sel_modelpredictors)

    Predictors<-data_object$sel_strms %>% 
      left_join(data_object$sel_modelpredictors,
                by=c("ProvReachID"="gen_ProvReachID"))%>% 
      select(ProvReachID,Network_Line_Type, starts_with("hb_"),any_of(input$mapsel_pred),Shape) %>% 
      sf::st_as_sf()
    
    rng<-pretty(range(Predictors[[input$mapsel_pred]],na.rm=T),n=8)

    mv<-mapview::mapview(Predictors,
                         zcol=c(input$mapsel_pred),
                         at=rng)
    
    mv@map
  })
  
  # predperf_tab tab content --
  output$predperf_out<-plotly::renderPlotly({
    req(data_object$sel_modelOOSpredictions)
    
    ttl<-paste(
      input$sel_taxa,
      case_when(input$sel_ep=="resp_Comm_Biomass"~ "Biomass (g/100m^2)",
                T ~ "Density (individuals/100m^2)"),
      paste(input$sel_region)
    )
    
    sub_data<-data_object$sel_modelOOSpredictions
    
    rng<-range(c(sub_data$observed,sub_data$quant_0.75),na.rm=T)
    
    rng<-range(pretty(rng))
    
    plt<-ggplot(sub_data,
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
    req(data_object$sel_modelShap)

    pred_imp<-data_object$sel_modelShap %>% 
      select(shape_param,any_of(pred_names)) %>% 
      group_by(shape_param) %>% 
      summarise(across(everything(),~mean(abs(.x)))) %>% 
      pivot_longer(c(everything(),-shape_param),names_to = "Predictors", values_to = "Importance") 

    plt<-ggplot(pred_imp,aes(x=Importance,y=Predictors))+
      geom_point()+
      facet_wrap(~shape_param,scales="free_x")+
      xlab("Importance\n(mean absolute SHAP value)")+
      theme_bw()
    
    plotly::ggplotly(plt)
    
  })
  
  # predsurf_tab tab content --
  
  output$predsurf_out<-plotly::renderPlotly({
    req(data_object$sel_modelShap)
    req(input$shap_pred_sel)
    req(input$shap_col_sel)
    
    sub_data<-data_object$sel_modelShap %>% 
      select(all_of(input$shap_pred_sel),shape_param) %>% 
      setNames(c("y","shape_param")) %>% 
      bind_cols(
        data_object$sel_modelShap %>% 
          select(all_of(paste0("sel_",input$shap_pred_sel))) %>% 
          setNames("x")
      )%>% 
      bind_cols(
        data_object$sel_modelShap %>% 
          select(all_of(paste0("sel_",input$shap_col_sel))) %>% 
          setNames("colour") 
      ) 
    
    
    plt<-ggplot(sub_data,aes(x=x,y=y,colour=colour))+
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
