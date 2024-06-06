library(shinydashboard)
library(tidyverse)
library(sf)
# Set the application-level cache
shinyOptions(cache = cachem::cache_disk("./bind-cache",max_size = 1024 * 1024^4))
#shinyOptions(cache = cachem::cache_disk("./cache"))

fp<-file.path("data",paste0("Model_data_v2.gpkg"))
con <- DBI::dbConnect(RSQLite::SQLite(), fp)

regions<-tbl(con,"Region_names") %>% collect() %>% pull(1)
taxa<-tbl(con,"Taxa_names") %>% collect() %>% pull(1)
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
    validate(need(length(input$sel_region)<5,"Select up to 4 regions for mapping"))
    
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
             P50=quant_0.5,
             p75=quant_0.75,
             p75_ref=quant_0.75_ref,
             p75_refdiff=quant_0.75_refdiff,
             contains(pred_names),
             geom) %>% 
      mutate(
        `Observed`=observed,
        `Predicted - Reference`=p75_ref,
        `Predicted - Current`=p75,
        `(Current - Reference)`=p75_refdiff
      ) %>% 
      sf::st_as_sf() %>% 
      sf::st_transform(4326)
    
    return(out)
    
  })
  
  output$map_bio <- leaflet::renderLeaflet({
    req(input$sel_region)
    req(input$sel_taxa)
    req(input$sel_ep)
    #req(input$map_layer_sel)
    validate(need(length(input$sel_region)<5,"Select up to 4 regions for mapping"))
    
    sel_modelpredictions<-map_data() 
    
    sel_modelpredictions<-suppressWarnings(sf::st_cast(sel_modelpredictions,"LINESTRING"))
    
    #browser()
    
    rng<-pretty(range(c(sel_modelpredictions$`Observed`,sel_modelpredictions$`Predicted - Reference`,sel_modelpredictions$`Predicted - Current`),na.rm=T),n=8)
    mx<-max(abs(sel_modelpredictions$`(Current - Reference)`),na.rm=T)
    rng2<-pretty(c(-mx,mx),n=8)
    rng2<-rng2[rng2!=0]
    
    col.pal <- leaflet::colorBin("viridis", bins = rng, na.color = "grey")
    col.pal2 <- leaflet::colorBin("viridis", bins = rng2, na.color = "grey")
    
    sel_modelpredictions_sub<-sel_modelpredictions %>% 
      tibble::as_tibble() %>% 
      select(ProvReachID,
             any_of(c("Observed",
                      "Predicted - Current",
                      "Predicted - Reference",
                      "(Current - Reference)")),
             starts_with("LDI"),
             starts_with("hb_"),
             -ends_with("_ref"))
    
    #browser()
    # if (input$map_layer_sel == "(Current - Reference)"){
    #   col.pal<-col.pal2
    #   rng<-rng2
    # }
    
    leaflet::leaflet(data=sel_modelpredictions) %>%
      leaflet::addTiles() %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group ="ESRI - Imagery") %>%
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap.Mapnik, group ="OpenStreetMap") %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group ="CartoDB") %>% 
      leaflet::setView(lng=mean(sf::st_bbox(sel_modelpredictions)[c(1,3)]),lat=mean(sf::st_bbox(sel_modelpredictions)[c(2,4)]),zoom=8) %>% 
      leaflet::addLayersControl(
        baseGroups = c("CartoDB",
                       "OpenStreetMap",
                       "ESRI - Imagery"),
        # overlayGroups  = c("Observed",
        #                    "Predicted - Current",
        #                    "Predicted - Reference",
        #                    "(Current - Reference)"
        # ),
        position = "topleft",
        options = leaflet::layersControlOptions(collapsed = F)
      ) %>% 
      # leaflet::hideGroup(c("Predicted - Current",
      #                      "Predicted - Reference",
      #                      "(Current - Reference)")) %>%
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
      leafgl::addGlPolylines(layerId=~ProvReachID,
                             data=sel_modelpredictions,
                             group="Observed",
                             label="ProvReachID",
                             weight=0.5,
                             opacity=1,
                             #popup=colnames(sel_modelpredictions_sub),
                             src =F,
                             digits=5,
                             color=~col.pal(sel_modelpredictions[["Observed"]])
      ) #%>%
    # leafgl::addGlPolylines(#layerId=~paste0("Observer_",ProvReachID),
    #                        data=sel_modelpredictions,
    #                        group="Observed",
    #                        label="ProvReachID",
    #                        weight=0.5,
    #                        opacity=1,
    #                        popup=colnames(sel_modelpredictions_sub),
    #                        src =F,
    #                        digits=5,
    #                        color=~col.pal(sel_modelpredictions[["Observed"]])
    # ) %>%
    # leafgl::addGlPolylines(#layerId=~paste0("Predicted - Current_",ProvReachID),
    #                        data=sel_modelpredictions,
    #                        group="Predicted - Current",
    #                        label="ProvReachID",
    #                        weight=0.5,
    #                        opacity=1,
    #                        popup=F,
    #                        src =F,
    #                        digits=5,
    #                        color=~col.pal(sel_modelpredictions[["Predicted - Current"]])
    # ) %>%
    # leafgl::addGlPolylines(#layerId=~paste0("Predicted - Reference_",ProvReachID),
    #                        data=sel_modelpredictions,
    #                        group="Predicted - Reference",
    #                        label="ProvReachID",
    #                        weight=0.5,
    #                        opacity=1,
    #                        popup=F,
    #                        src =F,
    #                        digits=5,
    #                        color=~col.pal(sel_modelpredictions[["Predicted - Reference"]])
    # ) %>%
    # leafgl::addGlPolylines(#layerId=~paste0("(Current - Reference)_",ProvReachID),
    #                        data=sel_modelpredictions,
    #                        group="(Current - Reference)",
    #                        label="ProvReachID",
    #                        weight=0.5,
    #                        opacity=1,
    #                        popup=F,
    #                        src =F,
    #                        digits=5,
    #                        color=~col.pal2(sel_modelpredictions[["(Current - Reference)"]])
    # ) %>%
    # leaflet::addPolylines(layerId=~paste0("Observer_",ProvReachID),
    #                       group="Observed",
    #                       weight=2,
    #                       opacity=1,
    #                       popup=leafpop::popupTable(sel_modelpredictions_sub),
    #                       color=~col.pal(sel_modelpredictions[["Observed"]])
    # ) %>%
    # leaflet::addPolylines(layerId=~paste0("Predicted - Current_",ProvReachID),
    #                       group="Predicted - Current",
    #                       weight=2,
    #                       opacity=1,
    #                       popup=leafpop::popupTable(sel_modelpredictions_sub),
    #                       color=~col.pal(sel_modelpredictions[["Predicted - Current"]])
    # ) %>%
    # leaflet::addPolylines(layerId=~paste0("Predicted - Reference_",ProvReachID),
    #                       group="Predicted - Reference",
    #                       weight=2,
    #                       opacity=1,
    #                       popup=leafpop::popupTable(sel_modelpredictions_sub),
    #                       color=~col.pal(sel_modelpredictions[["Predicted - Reference"]])
    # ) %>%
    # leaflet::addPolylines(layerId=~paste0("(Current - Reference)_",ProvReachID),
    #                       group="(Current - Reference)",
    #                       weight=2,
    #                       opacity=1,
    #                       popup=leafpop::popupTable(sel_modelpredictions_sub),
    #                       color=~col.pal2(sel_modelpredictions[["(Current - Reference)"]])
    # ) %>%
    
    
    
  })
  
  observeEvent(input$map_layer_sel,
               ignoreInit=T,{
                 req(input$sel_region)
                 req(input$sel_taxa)
                 req(input$sel_ep)
                 #req(input$map_layer_sel)
                 validate(need(length(input$sel_region)<5,"Select up to 4 regions for mapping"))
                 
                 sel_modelpredictions<-map_data() 
                 
                 sel_modelpredictions<-suppressWarnings(sf::st_cast(sel_modelpredictions,"LINESTRING"))
                 
                 #browser()
                 
                 rng<-pretty(range(c(sel_modelpredictions$`Observed`,sel_modelpredictions$`Predicted - Reference`,sel_modelpredictions$`Predicted - Current`),na.rm=T),n=8)
                 mx<-max(abs(sel_modelpredictions$`(Current - Reference)`),na.rm=T)
                 rng2<-pretty(c(-mx,mx),n=8)
                 rng2<-rng2[rng2!=0]
                 
                 col.pal <- leaflet::colorBin("viridis", bins = rng, na.color = "grey")
                 col.pal2 <- leaflet::colorBin("viridis", bins = rng2, na.color = "grey")
                 
                 sel_modelpredictions_sub<-sel_modelpredictions %>% 
                   tibble::as_tibble() %>% 
                   select(ProvReachID,
                          any_of(c("Observed",
                                   "Predicted - Current",
                                   "Predicted - Reference",
                                   "(Current - Reference)")),
                          starts_with("LDI"),
                          starts_with("hb_"),
                          -ends_with("_ref"))
                 
                 if (input$map_layer_sel == "(Current - Reference)"){
                   col.pal<-col.pal2
                   rng<-rng2
                 }
                 
                 #browser()
                 
                 leaflet::leafletProxy("map_bio", session) %>%
                   leafgl::clearGlLayers() %>% 
                   leafgl::addGlPolylines(#layerId=~ProvReachID, #current this is bugged unfortunately
                     data=sel_modelpredictions,
                     #group=~input$map_layer_sel,
                     label="ProvReachID",
                     weight=0.5,
                     opacity=1,
                     #popup=colnames(sel_modelpredictions_sub),
                     src =F,
                     digits=5,
                     color=~col.pal(sel_modelpredictions[[input$map_layer_sel]])
                   )
                 
               })
  
  
  observeEvent(input$map_bio_groups,
               ignoreInit=F,
               {
                 req(F)
                 req(input$sel_region)
                 req(input$sel_taxa)
                 req(input$sel_ep)
                 req(input$map_bio_groups)
                 validate(need(length(input$sel_region)<5,"Select up to 4 regions for mapping"))
                 browser()
                 
                 
                 sel_modelpredictions<-map_data() 
                 
                 rng<-pretty(range(c(sel_modelpredictions$`Observed`,sel_modelpredictions$`Predicted - Reference`,sel_modelpredictions$`Predicted - Current`),na.rm=T),n=8)
                 mx<-max(abs(sel_modelpredictions$`(Current - Reference)`),na.rm=T)
                 rng2<-pretty(c(-mx,mx),n=8)
                 rng2<-rng2[rng2!=0]
                 
                 #if (input$map_layer_sel=="(Current - Reference)") rng<-rng2
                 
                 # var <- input$map_layer_sel
                 # #var<-var[var!="Observed"]
                 # for (i in var){
                 #   rng_sel<-rng
                 #   if (i=="(Current - Reference)") rng_sel<-rng2
                 # 
                 #   col.pal <- leaflet::colorBin("viridis", bins = rng_sel, na.color = "grey")
                 #   
                 #   leaflet::leafletProxy("map_bio") %>% 
                 #       leaflet::addPolylines(data=sel_modelpredictions,
                 #                             layerId="ProvReachID",
                 #                             color=col.pal(sel_modelpredictions[[i]]))
                 #     
                 # }
                 
                 # i <- input$map_bio_groups
                 # 
                 # i <- i[i %in% c("Observed",
                 #                 "Predicted - Current",
                 #                 "Predicted - Reference",
                 #                 "(Current - Reference)")]
                 # 
                 # rng_sel<-rng
                 # if (i=="(Current - Reference)") rng_sel<-rng2
                 # 
                 # col.pal <- leaflet::colorBin("viridis", bins = rng_sel, na.color = "grey")
                 # 
                 # leaflet::leafletProxy("map_bio") %>% 
                 #   leaflet::clearShapes() %>% 
                 #   leaflet::addPolylines(data=sel_modelpredictions,
                 #                         #layerId="ProvReachID",
                 #                         #group=i,
                 #                         weight=2,
                 #                         opacity=1,
                 #                         color=col.pal(sel_modelpredictions[[i]])
                 #   )
                 # 
                 
                 
               })
  
  # observeEvent(input$map_layer_sel,
  #              ignoreInit=T,{
  #   req(input$sel_region)
  #   req(input$sel_taxa)
  #   req(input$sel_ep)
  #   req(input$map_layer_sel)
  #   validate(need(length(input$sel_region)<5,"Select up to 4 regions for mapping"))
  #   #browser()
  #   
  #   loading_message(session)
  #   
  #   sel_modelpredictions<-map_data()
  #   
  #   rng<-pretty(range(c(sel_modelpredictions$`Observed`,sel_modelpredictions$`Predicted - Reference`,sel_modelpredictions$`Predicted - Current`),na.rm=T),n=8)
  #   mx<-max(abs(sel_modelpredictions$`(Current - Reference)`),na.rm=T)
  #   rng2<-pretty(c(-mx,mx),n=8)
  #   rng2<-rng2[rng2!=0]
  #   
  #   #if (input$map_layer_sel=="(Current - Reference)") rng<-rng2
  #   
  #   var <- input$map_layer_sel
  #   #var<-var[var!="Observed"]
  #   for (i in var){
  #     rng_sel<-rng
  #     if (i=="(Current - Reference)") rng_sel<-rng2
  #     
  #     tmap::tmapProxy("map_bio", session, {
  #       #tmap::tm_remove_layer(401) +
  #         tmap::tm_shape(sel_modelpredictions) +
  #         tmap::tm_lines(col=i,
  #                        breaks = rng_sel,
  #                        lwd=1.75,
  #                        palette="viridis",
  #                        popup.vars=c("ProvReachID",i),
  #                        group=i,
  #                        id="ProvReachID"#,
  #                        #zindex=401
  #         )
  #     })
  #   }
  #   
  #   
  #   shinyWidgets::closeSweetAlert()
  # })
  
  
  #output$map_bio <- leaflet::renderLeaflet({
  output$map_bio2 <- tmap::renderTmap({
    req(input$sel_region)
    req(input$sel_taxa)
    req(input$sel_ep)
    validate(need(length(input$sel_region)<5,"Select up to 4 regions for mapping"))
    
    
    # con <- DBI::dbConnect(RSQLite::SQLite(), fp)
    # 
    # sel_modelpredictions<-tbl(con,"Model_Predictions") %>% 
    #   filter(tx_Taxa == local(input$sel_taxa)) %>%
    #   filter(gen_Region %in% local(input$sel_region)) %>%
    #   select(gen_ProvReachID,contains(input$sel_ep),contains(pred_names)) %>% 
    #   collect()
    # 
    # DBI::dbDisconnect(con)
    # 
    # Predicted<-sel_strms() %>% 
    #   left_join(sel_modelpredictions,
    #             by=c("ProvReachID"="gen_ProvReachID")) %>% 
    #   #mutate(across(contains(c("quant_","observed","predicted")),~expm1(.x))) %>% 
    #   rename_with(~gsub(paste0(input$sel_ep,"_"),"",.x)) %>% 
    #   select(ProvReachID,
    #          observed,
    #          P50=quant_0.5,
    #          p75=quant_0.75,
    #          p75_ref=quant_0.75_ref,
    #          p75_refdiff=quant_0.75_refdiff,
    #          contains(pred_names),
    #          geom) %>% 
    #   sf::st_as_sf()
    # 
    # Observed<-Predicted %>%
    #   select(ProvReachID,`Segment Median`=observed)
    # Reference<-Predicted %>%
    #   select(ProvReachID,`Predicted`=p75_ref,any_of(paste0(pred_names,"_ref"))) %>%
    #   rename_with(.cols=any_of(paste0(pred_names,"_ref")),~gsub("_ref","",.x))
    # Difference<-Predicted %>%
    #   select(ProvReachID,`(Current - Reference)`=p75_refdiff)
    # Current<-Predicted %>% 
    #   select(ProvReachID,`Predicted`=p75,any_of(paste0(pred_names,"_obs"))) %>% 
    #   rename_with(.cols=any_of(paste0(pred_names,"_obs")),~gsub("_obs","",.x))
    
    sel_modelpredictions<-map_data()
    
    rng<-pretty(range(c(Current$`Predicted`,Observed$`Segment Median`,Reference$`Predicted`),na.rm=T),n=8)
    
    mx<-max(abs(Difference$`(Current - Reference)`),na.rm=T)
    
    rng2<-pretty(c(-mx,mx),n=8)
    rng2<-rng2[rng2!=0]
    
    mv <- tmap::tm_shape(Observed)+
      tmap::tm_lines(col="Segment Median",
                     breaks =rng,
                     lwd=1.75,
                     palette="viridis",
                     popup.vars=c("ProvReachID","Segment Median"),
                     group="Observed",
                     id="ProvReachID")+
      tmap::tm_shape(Current)+
      tmap::tm_lines(col="Predicted",
                     breaks =rng,
                     lwd=1.75,
                     palette="viridis",
                     popup.vars=colnames(Current)[colnames(Current)!="geom"],
                     group="Predicted - Current",
                     id="ProvReachID")+
      tmap::tm_shape(Reference)+
      tmap::tm_lines(col="Predicted",
                     breaks =rng,
                     lwd=1.75,
                     palette="viridis",
                     popup.vars=colnames(Reference)[colnames(Reference)!="geom"],
                     group="Predicted - Reference",
                     id="ProvReachID")+
      tmap::tm_shape(Difference)+
      tmap::tm_lines(col="(Current - Reference)",
                     breaks =rng2,
                     lwd=1.75,
                     midpoint = 0,
                     #palette="viridis",
                     popup.vars=c("ProvReachID","(Current - Reference)"),
                     group="Predicted - (Current - Reference)",
                     id="ProvReachID")
    # 
    # mv<-mv %>%
    #   tmap::tmap_leaflet(in.shiny = TRUE) %>%
    #   leaflet::hideGroup(c("Predicted - Current","Predicted - Reference","Predicted - (Current - Reference)"))
    
    #browser()
    
    # col.bins <-rng
    # col.pal <- leaflet::colorBin("viridis", bins = col.bins, na.color = "grey")
    # 
    # mv<-leaflet::leaflet() %>%
    #   #leaflet::addTiles() %>%
    #   #leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group ="ESRI") %>% 
    #   leaflet::addPolylines(data=Observed,
    #                         color=col.pal(Observed$`Segment Median`),
    #                         #breaks =rng,
    #                         #lwd=1.75,
    #                         #palette="viridis",
    #                         #popup=c("ProvReachID","Segment Median"),
    #                         group="Observed",
    #                         layerId="ProvReachID")
    
    # mv<-mapview::mapview(Observed,
    #                      layer.name="Observed",
    #                      #layerId=Observed$ProvReachID,
    #                      zcol=c("Segment Median"),
    #                      at=(rng))+
    #   mapview::mapview(Current,
    #                    layer.name="Predicted - Current",
    #                    #layerId=Current$ProvReachID,
    #                    zcol=c("Predicted"),
    #                    at=(rng),
    #                    legend =T,
    #                    hide =T)+
    #   mapview::mapview(Reference,
    #                    layer.name="Predicted - Reference",
    #                    #layerId="ProvReachID",
    #                    zcol=c("Predicted"),
    #                    at=(rng),
    #                    legend =T,
    #                    hide =T)+
    #   mapview::mapview(Difference,
    #                    layer.name="Predicted - (Current - Reference)",
    #                    #layerId=Difference$ProvReachID,
    #                    zcol=c("(Current - Reference)"),
    #                    at=(rng2),
    #                    legend =T,
    #                    hide =T)
    
    
    
    return(mv)
    # return(mv@map)
  }) %>% 
    bindCache(input$sel_region,
              input$sel_taxa,
              input$sel_ep)
  
  
  # map_bio_tab Observer ----------------------------------------------------
  
  sel_reach<-reactive({
    #if (is.null(input$map_bio_shape_click)) return(NULL)
    if (is.null(input$map_bio_glify_click)) return(NULL)
    #browser()
    #input$map_bio_glify_click$id
    #str_split(input$map_bio_glify_click$id,"_")[[1]][[2]]
    
    sel_reach<-sf::st_as_sf(tibble(lat=input$map_bio_glify_click$lat,long=input$map_bio_glify_click$lng),
                            coords = c("long","lat"),
                            crs=4326)
    
    sel_reach<-suppressMessages(nngeo::st_nn(sel_reach,sel_strms(),k=1,maxdist=Inf,parallel =1,progress =F)[[1]])
    sel_strms()$ProvReachID[sel_reach]
  })
  
  # observeEvent(input$map_bio_shape_click,{
  #   browser()
  #   sel_reach<-sf::st_as_sf(tibble(lat=input$map_bio_shape_click$lat,long=input$map_bio_shape_click$lng),
  #                           coords = c("long","lat"),
  #                           crs="+proj=longlat +datum=WGS84")
  #   
  #   sel_reach<-suppressMessages(nngeo::st_nn(sel_reach,sel_strms(),k=1,maxdist=Inf,parallel =1,progress =F)[[1]])
  #   sel_reach$sel_reach<-sel_strms()$ProvReachID[sel_reach]
  # })
  
  #observeEvent(input$map_bio_shape_click,{
  observeEvent(input$map_bio_glify_click,{
    # browser()
    output$SHAP_breakdown<-shiny::renderPlot({
      #validate(need(input$map_bio_glify_click$group %in% c("Predicted - Current","Predicted - Reference"),message="Select a stream line in the Current or Reference Layer to see the prediction breakdown"))
      validate(need(input$map_layer_sel %in% c("Predicted - Current","Predicted - Reference"),message="Select a stream line in the Current or Reference Layer to see the prediction breakdown"))
      
      loading_message(session)
      
      #sel_reach<-input$map_bio_shape_click$id
      #sel_reach<-gsub("\\.\\d$|\\.\\d$","",sel_reach)
      # sel_time<-case_when(
      #   input$map_bio_glify_click$group == "Predicted - Current" ~ c("Current Mean","Current Presence/Absence"),
      #   input$map_bio_glify_click$group == "Predicted - Reference" ~ c("Reference Mean","Reference Presence/Absence")
      # )
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
        theme(text=element_text(size=18))
      
      shinyWidgets::closeSweetAlert()
      pt
    })
    
  })
  
  # map_pred_tab ------------------------------------------------------------
  output$map_pred <- leaflet::renderLeaflet({
    req(input$sel_region)
    req(input$sel_taxa)
    req(input$sel_ep)
    validate(need(length(input$sel_region)<5,"Select up to 4 regions for mapping"))
    
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
    con <- DBI::dbConnect(RSQLite::SQLite(), fp)
    loading_message(session)
    
    q25<-function(x) quantile(x,0.10,na.rm = T)
    mn<-function(x) mean(x,na.rm=T)
    q75<-function(x) quantile(x,0.90,na.rm = T)
    
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
      summarise(across(everything(),list(Importance=mn,q25=q25,q75=q75),.names = "{.fn}_{.col}")) %>% 
      pivot_longer(c(everything(),-shape_param),names_to = "Predictors", values_to = "Importance") %>% 
      mutate(Summary=stringr::str_split(Predictors,"_",n=2,simplify=T)[,1]) %>% 
      mutate(Predictors=str_replace(Predictors,Summary,""))%>% 
      mutate(Predictors=str_replace(Predictors,"^_","")) %>% 
      pivot_wider(names_from=Summary,values_from = Importance)
    
    DBI::dbDisconnect(con)
    
    pred_imp<-sel_modelShap
    
    plt<-ggplot(pred_imp,aes(x=Importance,y=Predictors,xmin=q25,xmax=q75))+
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
