library(shinydashboard)
library(dplyr)
library(dbplyr)

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
    x=="LDI_HAiFLS_mean" ~ "OLCC Land Disturbance Index - HAiFLS",
    x=="LDI_HAiFLO_mean" ~ "OLCC Land Disturbance Index - HAiFLO",
    T ~ x
  )
}

fp<-file.path("data",paste0("Model_data_v4_dart.gpkg"))
con <- DBI::dbConnect(RSQLite::SQLite(), fp)

regions<-tbl(con,"Region_names") %>% collect() %>% pull(1)
taxa<-tbl(con,"Taxa_names") %>% collect() %>% pull(1)
CalcEP<-tbl(con,"CalcEP_names") %>% collect() %>% pull(1)
pred_names<-tbl(con,"Predictor_names") %>% collect() %>% pull(1)
ep<-list(Density = "resp_Comm_Abundance",
         Biomass = "resp_Comm_Biomass")

names(taxa) <- ep_rn(taxa)
names(pred_names) <- pred_rn(pred_names)

DBI::dbDisconnect(con)

fluidPage(
  
  dashboardPage(
    dashboardHeader(title = "Fish Modeling"),
    dashboardSidebar(
      #p("1994 Filter"),
      sidebarMenu(
        menuItem("Welcome", tabName = "home"),
        menuItem("Fish Map", tabName = "map_bio_tab"),
        menuItem("Predictor Map", tabName = "map_pred_tab"),
        menuItem("Predictive Accuracy", tabName = "predperf_tab"),
        menuItem("Predictor Importance", tabName = "predimp_tab"),
        menuItem("Predictor Response", tabName = "predsurf_tab")
      ),
      #box(
      #width=12,
      selectInput("sel_region","Region (up to 8 for mapping)",regions,multiple=T,selected = "w03_Lake_Ontario_West"),
      selectInput("sel_taxa",div("Taxa"),list(`Derived Endpoints`=CalcEP,`Modeled Taxa`=taxa),multiple=F,selected = "Brook (speckled) Trout"),
      h6(style = "text-align: center;margin-left: 15px;margin-right: 15px;", "(Note: 'Derived Endpoints' are calculated only using 'Modeled Taxa')"),
      selectInput("sel_ep","Endpoint",ep,multiple=F),
      #),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      column(
        width=10,offset=1,
        tags$a(href="https://buymeacoffee.com/ecopulse", 
               "Buy me a coffee",
               target="_blank")
      )
      
    ),
    dashboardBody(
      tabItems(
        # home tab content
        tabItem(tabName = "home",
                h3("Ontario Aquatic Ecosystem Classification Biotic Modeling"),
                p("The goal of this project is to use data from Ontario’s Aquatic Ecosystem Classification (AEC)
                  together with Flowing Waters Information System (FWIS) to develop predictive models of fish 
                  communities across Ontario. The biomass and density of 16 fish taxa are modeled using AEC
                  variables, as well as landcover summaries from the Ontario Landcover Compilation (OLC). Landcover
                  types were summarized into different groups, as well as a Landscape Disturbance Index (LDI), which
                  quantifies the potential impacts of different landcover types to aquartic ecosystems. Predictions 
                  of current populations are available across all tributaries to the Great Lakes and St. Lawrence River.
                  Predictions to simulated 'Reference' landscapes are available as well. Reference landscapes were
                  approximated by removing urban and agricultural landcovers, and proportionally increasing all natural
                  landcovers in the catchment to account for those removed. Additionally, the LDI was set to nearly 0 for
                  urban and agiculture landcovers in the simulated 'reference' landscapes."),
                br(),
                p("Results presented here are preliminary."),
                br(),
                tags$a(href="https://github.com/p-schaefer/AEC_Modelling", 
                       "The code for the project is available on GitHub",
                       target="_blank"),
                br(),
                tags$a(href="https://github.com/StatMixedML/LightGBMLSS", 
                       "LighGBMLSS models (An extension of LightGBM to probabilistic modelling) were used.",
                       target="_blank"),
                br(),
                tags$a(href="https://shap.readthedocs.io/en/latest/", 
                       "SHAP scores were used for model interpretation",
                       target="_blank"),
                br(),
                tags$a(href="https://github.com/StatMixedML/LightGBMLSS/blob/master/lightgbmlss/distributions/ZAGamma.py", 
                       "Models were fit to Zero-adjusted Gamma distributions",
                       target="_blank"),
                br(),
                tags$a(href="https://geohub.lio.gov.on.ca/maps/mnrf::aquatic-ecosystem-classification-aec-for-ontario/about", 
                       "Aquatic ecosystem classification (AEC) for Ontario",
                       target="_blank"),
                br(),
                tags$a(href="https://geohub.lio.gov.on.ca/documents/7aa998fdf100434da27a41f1c637382c/about", 
                       "Ontario Land Cover Compilation v.2.0",
                       target="_blank"),
                br(),
                tags$a(href="https://github.com/p-schaefer/ihydro", 
                       "Landscape data were processed using the ihydro R package",
                       target="_blank")
        ),# map_bio_tab tab content
        tabItem(tabName = "map_bio_tab",
                fluidPage(
                  column(width=12,
                         shinydashboard::box(
                           collapsed = T,collapsible =T,width=12,
                           title = "Observed and Predicted Fish Distributions",
                           p("The map below shows the locations of observed and predicted fish biomass and densities. When multiple observations
                              are present on a segment, the median is shown. By default, the observed values are shown, and model predictions can be
                              shown from the layers menu."),
                           p("Predictions of observed populations are available across all tributaries to the Great Lakes and St. Lawrence River.
                              Predictions to simulated 'Reference' landscapes are available as well. Reference landscapes were
                              approximated by removing urban and agricultural landcovers, and proportionally increasing all natural
                              landcovers in the catchment to account for those removed. Additionally, the LDI was set to nearly 0 for
                              urban and agiculture landcovers in the simulated 'reference' landscapes. Finally, the difference between observed
                              and reference communities are mapped, and expressed as (Current - Reference; i.e., positive values indicate
                              present day predictions are higher than simulated reference, and negative values indicate present day predictions are
                              lower than simulated reference)."),
                           p("Selecting a stream segment with either the 'Current' or 'Reference' layers selected
                              will show the predictor values associated with that segment for the 'Current' or 'Reference' predictions respectively.
                              Selecting a stream segment in the 'Current' or 'Reference' predicted layers will also present a 'SHAP Breakdown' figure 
                              showing the contributions of each predictor variable to the final predicted outcome. A positive effect on the SHAP score
                              of the mean suggests that predictor value is increasing the mean prediction,
                              whereas a positive effect on the SHAP score of the presence/absence suggests that the predictor values is increasing
                              the likelihood of a presence. Once a reach is selected, that reach will also be highlighted in the 'Predictor Response' tab.")
                         ),
                         fluidPage(
                           fluidRow(
                             column(width=6,
                                    shiny::radioButtons("map_layer_sel",
                                                        "Layers",
                                                        list(
                                                          `Stream Lines`="Stream Lines",
                                                          Observed="Observed",
                                                          `Predicted - Current`="Predicted - Current",
                                                          `Predicted - Reference`="Predicted - Reference",
                                                          `(Current - Reference)`="(Current - Reference)"
                                                        ),
                                                        selected=NULL,
                                                        inline=T)),
                             column(width=3,
                                    offset=1,
                                    shiny::radioButtons("map_breaks",
                                                        "Colour Breaks",
                                                        list(
                                                          Equal="pretty",
                                                          Quantile="quantile",
                                                          `Jenks Natural Breaks`="getJenksBreaks"),
                                                        selected="getJenksBreaks",
                                                        inline=T))
                             
                           ),
                           fluidRow(
                             #shinyjqui::jqui_resizable(box(width=9,leaflet::leafletOutput("map_bio", height = "800px"))),
                             shinyjqui::jqui_resizable(box(width=9,leafgl::leafglOutput("map_bio", height = "800px"))),
                             #shinyjqui::jqui_resizable(box(width=9,tmap::tmapOutput("map_bio", height = "800px"))),
                             shinyjqui::jqui_resizable(box(width=3,shiny::plotOutput("SHAP_breakdown", height = "800px")))
                           )
                         )
                  )
                )
        ),
        # map_pred_tab tab content
        tabItem(tabName = "map_pred_tab",
                fluidPage(
                  h3("Distributions of Predictor Variables"),
                  p("The map below shows the spatial distributions of predictors used to model the current distributions."),
                  column(
                    width=6,
                    offset=1,
                    selectInput("mapsel_pred","Predictor",pred_names,multiple=F)
                  ),
                  leaflet::leafletOutput("map_pred", height = "750px")
                )
        ),
        # predperf_tab tab content
        tabItem(tabName = "predperf_tab",
                fluidPage(
                  h3("Model Performance"),
                  p("The figure below shows Observed vs Predicted values. The data shown are filtered to the data selected on the side panel.
                  The solid black line is fit to the 50th percentile of
                     each observations predicted conditional distribition, and the blue lines are fit to the 25th and 75th percentiles.
                     All values are shown on the log-scale."),
                  shiny::plotOutput("predperf_out",  height = "750px")
                )
        ),
        # predimp_tab tab content
        tabItem(tabName = "predimp_tab",
                fluidPage(
                  h3("Predictor Importance"),
                  p("The figure below shows the relative importance of each predictor in describing whether or not taxa are
                    present or absent from a sample, as well as the mean. The data shown are filtered to the data selected on the side panel."),
                  shiny::plotOutput("predimp_out",  height = "1500px")
                )
        ),
        # predsurf_tab tab content
        tabItem(tabName = "predsurf_tab",
                fluidPage(
                  h3("Predictor Response Surfaces"),
                  p("The figure below shows the effect of a predictor variable on the presence/absence and mean predicted outcome. 
                    The data shown are filtered to the data selected on the side panel. The predicted effects can be colour by a separate variable to identify interactions among predictors.
                    A positive effect on the SHAP score of the mean suggests that predictor value is increasing the mean prediction,
                    whereas a positive effect on the SHAP score of the presence/absence suggests that the predictor values is increasing
                    the likelihood of a presence. If a reach was selected in the 'Fish Map' tab, it will be highlighted with a red circle."),
                  fluidRow(
                    column(4,offset = 1,selectInput("shap_pred_sel","Predictor",pred_names,multiple=F)),
                    column(4,offset = 1,selectInput("shap_col_sel","Colour",pred_names,multiple=F))
                  ),
                  shiny::plotOutput("predsurf_out",  height = "750px")
                )
        )
      )
    )
  )
  
)
