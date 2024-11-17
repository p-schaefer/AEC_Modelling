library(shinydashboard)
library(tidyverse)
library(sf)
# Set the application-level cache
#shinyOptions(cache = cachem::cache_disk("./bind-cache",max_size = 1024 * 1024^4))
#shinyOptions(cache = cachem::cache_disk("./cache"))

hydro_n_url<-"https://maps.geogratis.gc.ca/wms/hydro_network_en?request=GetCapabilities&service=WMS&version=1.3.0&layers=hydro_network&legend_format=image%2Fpng&feature_info_type=text%2Fhtml"
hydro_n<-xml2::read_html(hydro_n_url)
#xml2::xml_structure(hydro_n)
hydro_n<-xml2::xml_find_all(hydro_n,".//name")
hydro_n<-xml2::xml_text(hydro_n)
hydro_n<-hydro_n[ grepl("nhn:nhn:hydrography:hydro|nhn:nhn:hydrography:slwatercourse|nhn:nhn:hydrography:waterbody|nhn:nhn:drainageareas:nhnda",hydro_n)] #grepl("nhn:toponyms",hydro_n) 

fp<-file.path("data",paste0("Model_data_v5_dart.gpkg"))
con <- DBI::dbConnect(RSQLite::SQLite(), fp)

regions<-tbl(con,"Region_names") %>% collect() %>% pull(1)
taxa<-tbl(con,"Taxa_names") %>% collect() %>% pull(1)
CalcEP<-tbl(con,"CalcEP_names") %>% collect() %>% pull(1)
pred_names<-tbl(con,"Predictor_names") %>% collect() %>% pull(1)
ep<-list(Biomass = "resp_Comm_Biomass",
         Density = "resp_Comm_Abundance")

DBI::dbDisconnect(con)

loading_message<-function(session){
  shinyWidgets::sendSweetAlert(
    session = session,
    title = "Loading Data...",
    text = "Please be patient",
    type = "info",
    closeOnClickOutside = F,
    btn_labels = NA,
    backdrop = T
  )
}

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

pred_rn<-function(x) {
  case_when(
    x=="tx_Taxa" ~ "Species",
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
# Define server logic required to draw a histogram
function(input, output, session) {
  
  sel_strms<-reactive(
    sf::read_sf(fp,
                query=paste0("SELECT * FROM AEC_Streams WHERE AEC_Region_sub IN ('",paste(input$sel_region,collapse="', '"),"')")) %>% 
      sf::st_transform(4326)
  )
  
  # Tab Contents ------------------------------------------------------------
  
  # map_bio_tab -------------------------------------------------------------
  
  map_data<-reactive({
    req(input$sel_region)
    req(input$sel_taxa)
    req(input$sel_ep)
    validate(need(length(input$sel_region)<9,"Select up to 8 regions for mapping"))
    
    con <- DBI::dbConnect(RSQLite::SQLite(), fp)
    
    sel_modelpredictions<-tbl(con,"Model_Predictions") %>% 
      filter(tx_Taxa == local(input$sel_taxa)) %>%
      filter(gen_Region %in% local(input$sel_region)) %>%
      select(gen_ProvReachID,contains(input$sel_ep),contains(pred_names)) %>% 
      collect()
    
    DBI::dbDisconnect(con)
    
    out<- sel_strms() %>% 
      left_join(sel_modelpredictions,
                by=c("ProvReachID"="gen_ProvReachID")) %>% 
      #mutate(across(contains(c("quant_","observed","predicted")),~expm1(.x))) %>% 
      rename_with(~gsub(paste0(input$sel_ep,"_"),"",.x)) %>% 
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
      sf::st_as_sf() 
    
    return(out)
    
  })
  
  output$map_bio <- leaflet::renderLeaflet({
    req(input$sel_region)
    validate(need(length(input$sel_region)<9,"Select up to 8 regions for mapping"))

    sel_modelpredictions<-sel_strms()
    sel_modelpredictions<-suppressWarnings(sf::st_cast(sel_modelpredictions,"LINESTRING"))
    
    shiny::updateRadioButtons(session,inputId = "map_layer_sel",selected="Stream Lines")
    
    out <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE,
                                                              zoomSnap = 0.25,
                                                              zoomDelta = 1)) %>%
      leaflet::addTiles() %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group ="ESRI - Imagery") %>%
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap.Mapnik, group ="OpenStreetMap") %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group ="CartoDB") %>% 
      leaflet::setView(lng=mean(sf::st_bbox(sel_modelpredictions)[c(1,3)]),lat=mean(sf::st_bbox(sel_modelpredictions)[c(2,4)]),zoom=9) %>% 
      leaflet::addLayersControl(
        baseGroups = c("CartoDB",
                       "OpenStreetMap",
                       "ESRI - Imagery"),
        overlayGroups = c("NHN Drainage Areas",
                          "NHN Hydrography"),
        position = "topleft",
        options = leaflet::layersControlOptions(collapsed = F)
      ) %>%
      leafgl::addGlPolylines(
        data=sel_modelpredictions,
        weight=0.25,
        opacity=0.75,
        src =F,
        color=~"darkgrey"
      ) %>% 
      leaflet::hideGroup(c("NHN Drainage Areas","NHN Hydrography"))

    for (i in hydro_n[grepl("drainageareas",hydro_n)]){
      out <- out %>% 
        leaflet::addWMSTiles(baseUrl = "https://maps.geogratis.gc.ca/wms/hydro_network_en?",
                             layers = i,
                             options = leaflet::WMSTileOptions(format = "image/png", transparent = T),
                             group ="NHN Drainage Areas"
        ) 
    }
    for (i in hydro_n[grepl("hydrography",hydro_n)]){
      out <- out %>% 
        leaflet::addWMSTiles(baseUrl = "https://maps.geogratis.gc.ca/wms/hydro_network_en?",
                             layers = i,
                             options = leaflet::WMSTileOptions(format = "image/png", transparent = T),
                             group ="NHN Hydrography"
        )
    }
    
    return(out)
  })
  
  map_col_react <- reactive({
    req(input$sel_region)
    req(input$sel_taxa)
    req(input$sel_ep)
    req(input$map_layer_sel)
    req(input$map_breaks)
    req(input$map_layer_sel!="Stream Lines")
    validate(need(length(input$sel_region)<9,"Select up to 8 regions for mapping"))
    req(leaflet::leafletProxy("map_bio", session))
    
    sel_modelpredictions<-map_data() 
    sel_modelpredictions<-suppressWarnings(sf::st_cast(sel_modelpredictions,"LINESTRING"))
    
    if (input$const_col) {
      con <- DBI::dbConnect(RSQLite::SQLite(), fp)
      
      sel_taxa<-input$sel_taxa
      if (!sel_taxa %in% CalcEP) sel_taxa<-"tx_Taxa"
      
      brks<-tbl(con,"value_breaks") %>% 
        filter(tx_Taxa == local(sel_taxa)) %>% 
        filter(break_type %in% local(input$map_breaks)) %>% 
        select(contains(input$sel_ep)) %>% 
        collect() %>% 
        rename_with(~gsub(paste0(input$sel_ep,"_"),"",.x))
      
      DBI::dbDisconnect(con)
      
      rng <- brks$observed[!is.na(brks$observed)]
      rng2 <- brks$quant_0.75_refdiff[!is.na(brks$quant_0.75_refdiff)]

    } else {
      val_list<-c(sel_modelpredictions$`Observed`,sel_modelpredictions$`Predicted - Reference`,sel_modelpredictions$`Predicted - Current`)
      val_list<-val_list[!is.na(val_list)]
      diff_list<-sel_modelpredictions$`(Current - Reference)`
      diff_list<-diff_list[!is.na(diff_list)]
      
      if (length(val_list)==0) val_list<-0
      if (length(diff_list)==0) diff_list<-0
      
      val_list <- val_list[val_list!=0]
      val_list <- c(0,val_list)

      diff_list <- diff_list[diff_list!=0]
      diff_list <- c(0,diff_list)
      
      #browser()
      quant_fn<-function(x,n) quantile(x,probs = seq(0, 1, length.out = n + 1),na.rm=T)
      break_fn<-switch(input$map_breaks,
                       pretty=pretty,
                       quantile=quant_fn,
                       getJenksBreaks=BAMMtools::getJenksBreaks)
      
      diff_list<-c(-abs(diff_list),abs(diff_list))
      
      rng<-break_fn(val_list,8)
      rng2<-break_fn(diff_list,8)
      rng2<-rng2[rng2!=0]
      
      if (length(rng)==0) rng<-0
      if (length(rng2)==0) rng2<-0
      
      rng<-as.numeric(scales::number(rng))
      rng2<-as.numeric(scales::number(rng2))
      
      rng<-unique(rng)
      rng2<-unique(rng2)
      
      if (length(rng)<2) rng<-c(0,1,2,3,4)
      if (length(rng2)<2) rng2<-c(-4,-3,-2,-1,1,2,3,4)
      
    }
    
    col.pal <- leaflet::colorBin("viridis", bins = rng, na.color = "grey",reverse=F)
    col.pal2 <- leaflet::colorBin("Spectral", bins = rng2, na.color = "grey")
    
    if (input$map_layer_sel == "(Current - Reference)"){
      col.pal.sel<-col.pal2
      rng.sel<-rng2
    } else {
      col.pal.sel<-col.pal
      rng.sel<-rng
    }
    
    return(
      list(
        sel_modelpredictions=sel_modelpredictions,
        col.pal=col.pal,
        col.pal2=col.pal2,
        col.pal.sel=col.pal.sel,
        rng=rng,
        rng2=rng2,
        rng.sel=rng.sel
      )
    )
  })
  
  observeEvent(
    c(input$map_layer_sel,input$sel_ep,input$sel_taxa,input$map_breaks,input$const_col),
    ignoreInit=F,
    {
      
      map_col <- map_col_react()
      sel_modelpredictions <- map_col$sel_modelpredictions
      col.pal <- map_col$col.pal
      col.pal2 <- map_col$col.pal2
      col.pal.sel <- map_col$col.pal.sel
      rng <- map_col$rng
      rng2 <- map_col$rng2
      rng.sel <- map_col$rng.sel
      
      #browser()
      leaflet::leafletProxy("map_bio", session) %>%
        leafgl::clearGlLayers() %>% 
        leaflet::clearControls() %>% 
        leaflet::addLayersControl(
          baseGroups = c("CartoDB",
                         "OpenStreetMap",
                         "ESRI - Imagery"),
          overlayGroups = c("NHN Drainage Areas",
                            "NHN Hydrography"),
          position = "topleft",
          options = leaflet::layersControlOptions(collapsed = F)
        ) %>% 
        leaflet::addLegend(
          title="Observed/Predicted Values",
          labels=rng,
          colors=col.pal(rng)
        )%>%
        leaflet::addLegend(
          title="(Current - Reference)",
          labels=rng2,
          colors=col.pal2(rng2)
        ) %>%
        leafgl::addGlPolylines(
          data=sel_modelpredictions,
          weight=0.5,
          opacity=0.9,
          src =F,
          color=~col.pal.sel(sel_modelpredictions[[input$map_layer_sel]])
        ) %>% 
        leaflet::hideGroup(c("NHN Drainage Areas","NHN Hydrography"))
      
    })
  
  
  
  # map_bio_tab Observer ----------------------------------------------------
  
  sel_reach<-reactive({
    if (is.null(input$map_bio_glify_click)) return(NULL)
    sel_reach<-sf::st_as_sf(tibble(lat=input$map_bio_glify_click$lat,long=input$map_bio_glify_click$lng),
                            coords = c("long","lat"),
                            crs=4326)
    
    sel_reach<-suppressMessages(nngeo::st_nn(sel_reach,sel_strms(),k=1,maxdist=Inf,parallel =1,progress =F)[[1]])
    sel_strms()$ProvReachID[sel_reach]
  })
  
  pnt_react <- reactive({
    validate(need(input$map_layer_sel %in% c("Predicted - Current","Predicted - Reference"),message="Select a stream line in the Current or Reference Layer to see the prediction breakdown"))
    validate(need(!input$sel_taxa %in% CalcEP,message="Calculated Endpoints Don't Have SHAP scores yet..."))
    validate(need(sel_reach(),message="No stream selected"))
    
    pnt <- sel_strms() %>%
      filter(ProvReachID == sel_reach()) 
    
    pnt <- suppressWarnings(sf::st_cast(pnt,"POINT") )
  })
  
  observeEvent(input$map_bio_glify_click,{
    leaflet::leafletProxy("map_bio", session) %>%
      leaflet::clearGroup("sel_pnts")
  })
  
  observeEvent(input$map_bio_glify_click,{
    validate(need(input$map_layer_sel %in% c("Predicted - Current","Predicted - Reference"),message="Select a stream line in the Current or Reference Layer to see the prediction breakdown"))
    validate(need(!input$sel_taxa %in% CalcEP,message="Calculated Endpoints Don't Have SHAP scores yet..."))
    validate(need(sel_reach(),message="No stream selected"))
    
    leaflet::leafletProxy("map_bio", session) %>%
      leaflet::clearGroup("sel_pnts") %>% 
      leaflet::addCircles(
        radius = 12,
        group = "sel_pnts",
        color = "red",
        weight = 8,
        fillColor ="red",
        #fillOpacity = 0.5,
        data=pnt_react()
      )
  })
  
  SHAP_breakdown_react <- reactive({
    req(input$map_bio_glify_click)
    validate(need(input$map_layer_sel %in% c("Predicted - Current","Predicted - Reference"),message="Select a stream line in the Current or Reference Layer to see the prediction breakdown"))
    validate(need(!input$sel_taxa %in% CalcEP,message="Calculated Endpoints Don't Have SHAP scores yet..."))
    validate(need(sel_reach(),message="No stream selected"))
    
    sel_time<-case_when(
      input$map_layer_sel == "Predicted - Current" ~ c("Current Mean","Current Presence/Absence"),
      input$map_layer_sel == "Predicted - Reference" ~ c("Reference Mean","Reference Presence/Absence")
    )
    
    con <- DBI::dbConnect(RSQLite::SQLite(), fp)
    
    sel_modelShap<-tbl(con,"SHAP_scores") %>%
      filter(endpoint == local(input$sel_ep)) %>%
      filter(sel_tx_Taxa == local(input$sel_taxa)) %>%
      filter(sel_gen_ProvReachID %in% local(sel_reach())) %>%
      filter(shape_param %in% local(sel_time)) %>%
      collect()
    
    if (nrow(sel_modelShap)==0){
      shinyWidgets::closeSweetAlert()
      validate(need(nrow(sel_modelShap)>0,message="Virtual Stream Connector Selected"))
    }
    
    reach_shap<-sel_modelShap %>%
      select(-starts_with("sel_")) %>%
      pivot_longer(c(everything(),-shape_param,-endpoint),
                   names_to="Predictors",
                   values_to = "SHAP Score") %>%
      left_join(
        sel_modelShap %>%
          select(shape_param,endpoint,starts_with("sel_"),-sel_gen_Region,-sel_gen_link_id,-sel_gen_ProvReachID) %>%
          mutate(across(everything(),~as.character(.x))) %>%
          pivot_longer(c(everything(),-shape_param,-endpoint),
                       names_to="Predictors",
                       values_to = "Value") %>%
          mutate(Predictors=gsub("sel_","",Predictors)),
        by = join_by(shape_param, endpoint, Predictors)
      )
    
    reach_shap <- reach_shap %>%
      mutate(Predictors=pred_rn(Predictors))
    
    DBI::dbDisconnect(con)
    
    pt<-ggplot(reach_shap,aes(y=Predictors,xmin=0,xmax=`SHAP Score`,colour=`SHAP Score`>0)) +
      geom_linerange(linewidth=3) +
      geom_vline(xintercept = 0) +
      scale_x_continuous(breaks=scales::pretty_breaks(),labels=function(x) scales::comma(x)) +
      #scale_y_discrete(sec.axis = sec_axis(transform=~.,labels=reach_shap$Value, name="Values"))+
      xlab("SHAP Scores")+
      ggtitle(paste("Reach: ",sel_reach()))+
      theme_bw() +
      facet_wrap(~shape_param,scales="free_x")+
      theme(legend.position="none") +
      theme(text=element_text(size=17))
    
    return(pt)
  })
  
  observeEvent(input$map_bio_glify_click,{
    # output$SHAP_breakdown1 <- renderPlot({
    #   SHAP_breakdown_react()
    # })
    output$SHAP_breakdown2 <- renderPlot({
      SHAP_breakdown_react()
    })
  })
  
  observeEvent(input$map_bio_glify_click,{
    
    # shinyWidgets::show_alert(
    #   title = NULL,
    #   text = tags$div(
    #     plotOutput("SHAP_breakdown1",height = "800px"),
    #     "Plot retained below..."
    #   ),
    #   html = FALSE,
    #   width = "80%"
    # ) 
    
    
    # output$SHAP_breakdown<-shiny::renderPlot({
    #   validate(need(input$map_layer_sel %in% c("Predicted - Current","Predicted - Reference"),message="Select a stream line in the Current or Reference Layer to see the prediction breakdown"))
    #   validate(need(!input$sel_taxa %in% CalcEP,message="Calculated Endpoints Don't Have SHAP scores yet..."))
    #   
    #   loading_message(session)
    #   
    #   sel_time<-case_when(
    #     input$map_layer_sel == "Predicted - Current" ~ c("Current Mean","Current Presence/Absence"),
    #     input$map_layer_sel == "Predicted - Reference" ~ c("Reference Mean","Reference Presence/Absence")
    #   )
    #   
    #   con <- DBI::dbConnect(RSQLite::SQLite(), fp)
    #   
    #   sel_modelShap<-tbl(con,"SHAP_scores") %>% 
    #     filter(endpoint == local(input$sel_ep)) %>%
    #     filter(sel_tx_Taxa == local(input$sel_taxa)) %>%
    #     filter(sel_gen_ProvReachID %in% local(sel_reach())) %>% 
    #     filter(shape_param %in% local(sel_time)) %>% 
    #     collect() 
    #   
    #   if (nrow(sel_modelShap)==0){
    #     shinyWidgets::closeSweetAlert()
    #     validate(need(nrow(sel_modelShap)>0,message="Virtual Stream Connector Selected"))
    #   }
    #   
    #   reach_shap<-sel_modelShap %>% 
    #     select(-starts_with("sel_")) %>% 
    #     pivot_longer(c(everything(),-shape_param,-endpoint),
    #                  names_to="Predictors",
    #                  values_to = "SHAP Score") %>% 
    #     left_join(
    #       sel_modelShap %>% 
    #         select(shape_param,endpoint,starts_with("sel_"),-sel_gen_Region,-sel_gen_link_id,-sel_gen_ProvReachID) %>% 
    #         mutate(across(everything(),~as.character(.x))) %>% 
    #         pivot_longer(c(everything(),-shape_param,-endpoint),
    #                      names_to="Predictors",
    #                      values_to = "Value") %>% 
    #         mutate(Predictors=gsub("sel_","",Predictors)),
    #       by = join_by(shape_param, endpoint, Predictors)
    #     ) 
    #   
    #   reach_shap <- reach_shap %>% 
    #     mutate(Predictors=pred_rn(Predictors))
    #   
    #   DBI::dbDisconnect(con)
    #   
    #   pt<-ggplot(reach_shap,aes(y=Predictors,xmin=0,xmax=`SHAP Score`,colour=`SHAP Score`>0)) +
    #     geom_linerange(linewidth=3) +
    #     geom_vline(xintercept = 0) +
    #     scale_x_continuous(breaks=scales::pretty_breaks(),labels=function(x) scales::comma(x)) +
    #     #scale_y_discrete(sec.axis = sec_axis(transform=~.,labels=reach_shap$Value, name="Values"))+
    #     xlab("SHAP Scores")+
    #     ggtitle(paste("Reach: ",sel_reach()))+
    #     theme_bw() +
    #     facet_wrap(~shape_param,scales="free_x")+
    #     theme(legend.position="none") +
    #     theme(text=element_text(size=18))
    #   
    #   shinyWidgets::closeSweetAlert()
    #   pt
    # })
    
  })
  
  # map_pred_tab ------------------------------------------------------------
  output$map_pred <- leaflet::renderLeaflet({
    req(input$sel_region)
    req(input$sel_taxa)
    req(input$sel_ep)
    validate(need(length(input$sel_region)<9,"Select up to 8 regions for mapping"))
    
    loading_message(session)
    
    con <- DBI::dbConnect(RSQLite::SQLite(), fp)
    
    sel_modelpredictors<-tbl(con,"Predictor_Data") %>% 
      filter(gen_Region %in% local(input$sel_region)) %>%
      select(gen_ProvReachID,any_of(local(input$mapsel_pred))) %>% 
      collect()
    
    DBI::dbDisconnect(con)
    
    Predictors<-sel_strms() %>% 
      left_join(sel_modelpredictors,
                by=c("ProvReachID"="gen_ProvReachID"))%>% 
      select(ProvReachID,Network_Line_Type,any_of(input$mapsel_pred), geom) %>% 
      sf::st_as_sf()
    
    rng<-pretty(range(Predictors[[input$mapsel_pred]],na.rm=T),n=8)
    
    mv<-mapview::mapview(Predictors,
                         zcol=c(input$mapsel_pred),
                         at=rng)
    
    shinyWidgets::closeSweetAlert()
    mv@map
  }) %>% 
    bindCache(input$mapsel_pred,
              input$sel_region)
  
  # predperf_tab ------------------------------------------------------------
  output$predperf_out<-shiny::renderPlot({
    req(input$sel_region)
    req(input$sel_taxa)
    req(input$sel_ep)
    
    loading_message(session)
    
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
    
    rng<-range(c(sel_modelOOSpredictions$observed,sel_modelOOSpredictions$quant_0.5),na.rm=T)
    
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
    
    shinyWidgets::closeSweetAlert()
    plt
    
  }) %>% 
    bindCache(input$sel_region,
              input$sel_taxa,
              input$sel_ep)
  
  # predimp_tab -------------------------------------------------------------
  output$predimp_out<-shiny::renderPlot({
    req(input$sel_region)
    req(input$sel_taxa)
    req(input$sel_ep)
    validate(need(!input$sel_taxa %in% CalcEP,message="Calculated Endpoints Don't Have SHAP scores yet..."))
    
    con <- DBI::dbConnect(RSQLite::SQLite(), fp)
    loading_message(session)
    
    q10<-function(x) quantile(x,0.10,na.rm = T)
    mn<-function(x) mean(x,na.rm=T)
    q90<-function(x) quantile(x,0.90,na.rm = T)
    
    sel_ProvReachID<-sf::read_sf(fp,
                                 query=paste0("SELECT * FROM AEC_Streams WHERE AEC_Region_sub IN ('",paste(input$sel_region,collapse="', '"),"')")) %>% 
      pull(ProvReachID)
    
    sel_modelShap<-tbl(con,"SHAP_scores")%>% 
      filter(endpoint == local(input$sel_ep)) %>%
      filter(sel_tx_Taxa == local(input$sel_taxa)) %>%
      filter(sel_gen_ProvReachID %in% local(sel_ProvReachID)) %>%
      select(shape_param,starts_with("tx_"),any_of(local(pred_names))) %>% 
      group_by(shape_param) %>% 
      mutate(across(everything(),~abs(.x))) %>% 
      collect() %>% 
      summarise(across(everything(),list(Importance=mn,q10=q10,q90=q90),.names = "{.fn}_{.col}")) %>% 
      pivot_longer(c(everything(),-shape_param),names_to = "Predictors", values_to = "Importance") %>% 
      mutate(Summary=stringr::str_split(Predictors,"_",n=2,simplify=T)[,1]) %>% 
      mutate(Predictors=str_replace(Predictors,Summary,""))%>% 
      mutate(Predictors=str_replace(Predictors,"^_","")) %>% 
      pivot_wider(names_from=Summary,values_from = Importance)
    
    DBI::dbDisconnect(con)
    
    pred_imp<-sel_modelShap %>% 
      mutate(Predictors=pred_rn(Predictors))
    
    plt<-ggplot(pred_imp,aes(x=Importance,y=Predictors,xmin=q10,xmax=q90))+
      geom_point()+
      geom_linerange()+
      facet_wrap(~shape_param,scales="free_x")+
      xlab("Importance\n(mean absolute SHAP value | 10th-90th range)")+
      theme_bw()+
      theme(text=element_text(size=21))
    
    shinyWidgets::closeSweetAlert()
    
    plt
    
  }) %>% 
    bindCache(input$sel_region,
              input$sel_taxa,
              input$sel_ep)
  
  # predsurf_tab ------------------------------------------------------------
  output$predsurf_out<-shiny::renderPlot({
    req(input$sel_region)
    req(input$sel_taxa)
    req(input$sel_ep)
    req(input$shap_pred_sel)
    req(input$shap_col_sel)
    validate(need(!input$sel_taxa %in% CalcEP,message="Calculated Endpoints Don't Have SHAP scores yet..."))
    
    con <- DBI::dbConnect(RSQLite::SQLite(), fp)
    
    loading_message(session)
    
    # sel_reach<-input$map_bio_shape_click$id
    # sel_reach<-gsub("\\.\\d$|\\.\\d$","",sel_reach)
    
    sel_ProvReachID<-sf::read_sf(fp,
                                 query=paste0("SELECT * FROM AEC_Streams WHERE AEC_Region_sub IN ('",paste(input$sel_region,collapse="', '"),"')")) %>% 
      pull(ProvReachID)
    
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
    
    
    DBI::dbDisconnect(con)
    
    if (!"colour" %in% colnames(sel_modelShap)) sel_modelShap$colour<-sel_modelShap$x
    
    ax_brk<-function(x){
      ax_brk<-scales::pretty_breaks(5)(abs(x))
      sort(c(-ax_brk,0,ax_brk))
    }
    
    ax_lm<-function(x){
      #browser()
      ax_brk<-scales::pretty_breaks(3)(abs(x))
      range(sort(c(-ax_brk,0,ax_brk)))
    }
    
    plt<-ggplot(sel_modelShap,aes(x=x,y=y,colour=colour,text=ProvReachID))+
      geom_point()+
      geom_hline(yintercept = 0,linetype="dashed")+
      geom_smooth(aes(x=x,y=y),inherit.aes = F,se=F,colour="black")+
      labs(
        x=pred_rn(input$shap_pred_sel),
        y="SHAP Score",
        colour=pred_rn(input$shap_col_sel)
      )+
      theme_bw()+
      theme(text=element_text(size=21))+
      scale_y_continuous(breaks=ax_brk,labels=scales::comma,limits=ax_lm)+ #,limits=ax_lm,expand=c(0,0)
      facet_wrap(~shape_param,scales="free",ncol=2)
    
    if (length(sel_reach())>0){
      plt<-plt+
        geom_point(
          data=filter(sel_modelShap,ProvReachID==sel_reach()),
          size=5,
          stroke =3,
          colour="red"
        )
    }
    
    shinyWidgets::closeSweetAlert()
    
    plt
    
  }) %>% 
    bindCache(input$sel_region,
              input$sel_taxa,
              input$sel_ep,
              input$shap_pred_sel,
              input$shap_col_sel,
              sel_reach())
  
}
