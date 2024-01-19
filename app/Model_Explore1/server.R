library(shinydashboard)
library(tidyverse)
library(sf)

parse_fn<-function(x){
  out<-tibble(
    fl=x
  ) %>% 
    mutate(
      Categorical=if_else(grepl("cat",fl),"Categorical\n(AUC Performance)","Continuous\n(RMSE Performance)"),
      Categorical2=if_else(grepl("cat",fl),"Categorical","Continuous"),
      Weighted=!grepl("unwghtd",fl),
      Percent=if_else(grepl("Perc",fl),"Percent (logit-transformed)","Raw (ln(x+1)-transformed)"),
      Percent2=if_else(grepl("Perc",fl),"Percent","Raw"),
      Endpoint_sub=if_else(grepl("Abundance",fl),"Abundance","Biomass"),
      Equalization=str_split(fl,"Eqlz",simplify = T)[,2]
    ) %>% 
    mutate(Equalization=gsub(".rds","",Equalization)) %>% 
    mutate(Endpoint=paste(Categorical2,Percent2,Endpoint_sub)) %>% 
    mutate(Equalization=factor(Equalization,levels=c("NONE","UP25","DWN25","UP50","DWN50","UP100","DWN100"))) %>% 
    arrange(Equalization, Categorical2,Percent2,Endpoint_sub)
  
  split(out %>% select(-fl),out$fl)
  
}

# Define server logic required to draw a histogram
function(input, output, session) {
  
  # Setup Data --------------------------------------------------------------
  
  fl<-list.files(file.path("fits"),full.names = T)
  tune=fl[grepl("Tune",fl)]
  
  
  fl<-tibble(
    fl=fl[!grepl("Tune",fl)]
  ) %>% 
    mutate(ep=parse_fn(fl)) %>% 
    unnest(ep)
  
  tx<-unique(readRDS(fl$fl[[1]])$tx_Taxa)
  
  fl <- reactiveValues(
    tune=tune,
    fl=fl,
    tx=tx
  )
  
  stm_lns <- reactiveValues(
    lns=readRDS("AEC_Streams.rds")
  )
  
  # Model Performance -------------------------------------------------------
  output$mod_pref <- plotly::renderPlotly({
    perf<-readRDS(fl$tune) %>% 
      group_by(file) %>% 
      mutate(rank=case_when(
        .metric=="roc_auc" ~ order(mean,decreasing = T),
        T ~ order(mean)
      )) %>% 
      filter(rank==1) %>% 
      mutate(ep=parse_fn(file)) %>% 
      unnest(ep)  %>% 
      mutate(`Mean Performance`=mean)
    
    plt<-ggplot(perf,aes(x=Equalization,y=`Mean Performance`,colour=Weighted))+
      geom_point(position = position_dodge(0.5))+
      geom_linerange(aes(ymin=mean-std_err,ymax=mean+std_err),position = position_dodge(0.5))+
      facet_grid(Categorical~Endpoint_sub+Percent,scales="free")+
      theme_bw()
    
    plotly::ggplotly(plt)
  })
  
  
  # Model Predictions ---------------------------------------------------------------
  
  output$pred1_selectors<-renderUI({
    fluidRow(
      column(width=3,
             selectInput("ep_sel","Endpoint",
                         unique(fl$fl$Endpoint))
      ),
      column(width=3,
             selectInput("eq_sel","Equalization Method",
                         levels(fl$fl$Equalization))
      ),
      column(width=3,
             selectInput("wt_sel","Weighted",
                         unique(fl$fl$Weighted))
      ),
      column(width=3,
             selectInput("tx_sel","Taxa",
                         unique(fl$tx))
      )
    )
  })
  
  output$pred2_selectors<-renderUI({
    req(input$ep_sel)
    
    ml<-names(stm_lns$lns)
    ml<-gsub("AEC_Core_Package","",ml) 
    ml<-gsub("_AEC_Core","",ml) 
    
    cl<-c("Predicted","Observed","Prediction Error")
    #if (grepl("Categorical",input$ep_sel)) cl<-c(cl,c(".pred_0 - 1",".pred_1 - 20",".pred_20 - 40",".pred_40 - 60",".pred_80 - 100"))
    
    fluidRow(
      column(width=4,
             selectInput("ml_sel","Map Region",
                         ml)
      ),
      column(width=4,
             selectInput("cl_sel","Mapped Variable",
                         cl)
      )
    )
  })
  
  
  # Raw Data
  output$rawdata1 <- renderUI({
    req(
      input$ep_sel,
      input$eq_sel,
      input$wt_sel,
      input$tx_sel
    )
    
    sel_data<-fl$fl %>% 
      filter(Endpoint==input$ep_sel,
             Equalization==input$eq_sel,
             Weighted==input$wt_sel
      ) %>% 
      pull(fl) %>% 
      readRDS(.) %>% 
      select(-contains("weight")) 
    
    DT::renderDT(sel_data,
                 filter = list(
                   position = 'top', clear = FALSE
                 ),
                 options = list(scrollX = TRUE,
                                filter = 'top',
                                autoWidth = TRUE
                 ))
  })
  
  # 1:1 plots
  output$pred1 <- plotly::renderPlotly({
    req(
      input$ep_sel,
      input$eq_sel,
      input$wt_sel
    )
    
    sel_data<-fl$fl %>% 
      filter(Endpoint==input$ep_sel,
             Equalization==input$eq_sel,
             Weighted==input$wt_sel
      ) %>% 
      pull(fl) %>% 
      readRDS(.) %>% 
      filter(tx_Taxa==input$tx_sel)
    
    validate(
      need(!grepl("Categorical",input$ep_sel), "Can't figure out a good way to do 1:1 plots for categorical Data. See `Map` and `Raw Data` tabs.")
    )
    
    sel_data1 <- sel_data %>% 
      select(any_of(c(".pred_class",".pred",sel_data$var_pred[[1]],"gen_StreamName","gen_SampleDate"))) %>% 
      setNames(c("Predicted","Observed","Stream","Date")) %>% 
      mutate(Sample=paste(Stream,Date)) %>% 
      select(-Stream,-Date)
    
    p1<-ggplot(sel_data1,aes(x=Observed,y=Predicted,text=Sample))+
      geom_quantile(aes(x=Observed,y=Predicted),quantiles =c(0.25,0.5,0.75),method = "rqss", lambda = 0.5,inherit.aes = F)+
      #geom_smooth(aes(x=Observed,y=Predicted),method="lm",se=F,inherit.aes = F)+
      geom_point(size=0.25)+
      geom_abline(slope=1,intercept = 0)+
      scale_x_continuous(breaks=scales::pretty_breaks())+
      scale_y_continuous(breaks=scales::pretty_breaks())+
      theme_bw()
    
    
    plotly::ggplotly(p1)
  })
  
  # Maps
  output$map1 <- leaflet::renderLeaflet({
    req(
      input$ep_sel,
      input$eq_sel,
      input$wt_sel,
      input$ml_sel,
      input$cl_sel
    )
    
    lns<-stm_lns$lns
    lns<-lns[grepl(input$ml_sel,names(lns))][[1]]
    
    sel_data<-fl$fl %>% 
      filter(Endpoint==input$ep_sel,
             Equalization==input$eq_sel,
             Weighted==input$wt_sel
      ) %>% 
      pull(fl) %>% 
      readRDS(.) %>% 
      filter(tx_Taxa==input$tx_sel)
    
    sel_data1 <- sel_data %>% 
      select(any_of(c(".pred_class",".pred",sel_data$var_pred[[1]],"gen_ProvSegmentID"))) %>% 
      setNames(c("Predicted","Observed","ProvSegmentID")) 
    
    if (grepl("Categorical",input$ep_sel)) {
      # sel_data2 <- sel_data %>%
      #   select(gen_ProvSegmentID,any_of(matches(".pred_\\d"))) %>%
      #   group_by(gen_ProvSegmentID) %>%
      #   summarize(across(everything(),~median(.x)),.groups="keep") %>%
      #   mutate(across(everything(),~(.x)/sum(which.max(c_across(starts_with(".pred_")))))) %>%
      #   mutate(Predicted=levels(sel_data$.pred_class)[which.max(c_across(starts_with(".pred_")))]) %>%
      #   mutate(Predicted=factor(Predicted,levels=levels(sel_data$.pred_class)))
      # 
      # sel_data3<-sel_data1 %>%
      #   select(-Predicted) %>%
      #   group_by(ProvSegmentID) %>%
      #   summarise(Observed=names(table(Observed)[which.max(table(Observed))]),.groups = "drop") %>%
      #   mutate(Observed=factor(Observed,levels=levels(sel_data$.pred_class))) %>%
      #   left_join(sel_data2,by=c("ProvSegmentID"='gen_ProvSegmentID')) %>%
      #   mutate(`Prediction Error` = as.numeric(Observed)-as.numeric(Predicted))
      
      #browser()
      
      sel_data3<-sel_data1 %>%
        group_by(ProvSegmentID) %>%
        summarise(Observed=round(median(as.numeric(Observed))),
                  Predicted=round(median(as.numeric(Predicted))),
                  .groups = "drop") %>%
        mutate(`Prediction Error`=Observed-Predicted) %>% 
        mutate(across(c(Observed,Predicted),~levels(sel_data$.pred_class)[.x])) #%>%
        #mutate(across(c(Observed,Predicted),~factor(.x,levels=c(levels(sel_data$.pred_class)))))
      

      Map<-lns %>%
        left_join(sel_data3,by="ProvSegmentID")%>%
        mutate(across(c(Observed,Predicted),~as.character(.x))) %>%
        mutate(across(c(Observed,Predicted),~case_when(is.na(.x) ~ "Missing",T ~ .x))) #%>%
        #mutate(across(c(Observed,Predicted),~factor(.x,levels=c(levels(sel_data$.pred_class),"Missing"))))

    } else {
      sel_data3<-sel_data1 %>%
        group_by(ProvSegmentID) %>%
        summarise(Observed=median(Observed),
                  Predicted=median(Predicted),
                  .groups = "drop") %>%
        mutate(`Prediction Error`=Observed-Predicted)

      Map<-lns %>%
        left_join(sel_data3,by="ProvSegmentID")
    }
    
    if (grepl("Categorical",input$ep_sel)){
      if (input$cl_sel %in% c("Observed","Predicted")){
        col_range<-levels(sel_data$.pred_class)
      } else {
        col_range<-pretty(sel_data3[[input$cl_sel]])
      }
    } else {
      if (input$cl_sel %in% c("Observed","Predicted")){
        col_range<-pretty(c(sel_data3[["Observed"]],sel_data3[["Predicted"]]))
      } else {
        col_range<-pretty(sel_data3[[input$cl_sel]])
      }
    }
    
    m1<-mapview::mapview(Map,
                         zcol=input$cl_sel,
                         at = col_range,
                         burst=grepl("Categorical",input$ep_sel),
                         na.color="gray")
    
    return(m1@map)
    
  })
}
