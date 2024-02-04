library(shinydashboard)

regions<-readRDS("data/regions.rds")
taxa<-readRDS("data/taxa.rds")
pred_names<-readRDS("data/pred_names.rds")
ep<-list(Biomass = "resp_Comm_Biomass",
         Density = "resp_Comm_Abundance")

fluidPage(
  
  dashboardPage(
    dashboardHeader(title = "Fish Modeling"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Welcome", tabName = "home"),
        menuItem("Fish Map", tabName = "map_bio_tab"),
        menuItem("Predictor Map", tabName = "map_pred_tab"),
        menuItem("Predictive Accuracy", tabName = "predperf_tab"),
        menuItem("Predictor Importance", tabName = "predimp_tab"),
        menuItem("Predictor Response", tabName = "predsurf_tab")
      ),
      column(
        width=12,
        selectInput("sel_region","Region",regions,multiple=F,selected = "w03_Lake_Ontario_West"),
        selectInput("sel_taxa","Taxa",taxa,multiple=F,selected = "Brook (speckled) Trout"),
        selectInput("sel_ep","Endpoint",ep,multiple=F)
      )
    ),
    dashboardBody(
      tabItems(
        # home tab content
        tabItem(tabName = "home",
                h3("Ontario Aquatic Ecosystem Classification Biotic Modeling"),
                p("The goal of this project is to use data from Ontario’s Aquatic Ecosystem Classification (AEC)
                  together with Flowing Waters Information System (FWIS) to develop predictive models of fish and
                  benthic invertebrate communities across Ontario."),
                br(),
                # p("Fish communities were represented as biomass (g/m^2) and density (individual/m^2). All taxa were included in a single model"),
                # br(),
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
                       target="_blank")
        ),# map_bio_tab tab content
        tabItem(tabName = "map_bio_tab",
                fluidPage(
                  h3("Observed and Predicted Fish Distributions"),
                  p("The map below shows the locations of observed and predicted fish biomass and densities. When multiple observations
                  are present on a segment, the mean is shown. By default, the observed values are shown, and model predictions can be
                  shown from the layers menu. Predictions can be shown from the 50th and 75th percentiles of the expected distribution
                  for each reach."),
                  leaflet::leafletOutput("map_bio", height = "750px")
                )
        ),
        # map_pred_tab tab content
        tabItem(tabName = "map_pred_tab",
                fluidPage(
                  h3("Distributions of Predictor Variables"),
                  p("The map below shows the spatial distributions of predictors used in the model."),
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
                  p("The figure below shows Observed vs Predicted values. The solid black line is fit to the 50th percentile of
                     each observations predicted conditional distribition, and the blue lines are fit to the 25th and 75th percentiles.
                    "),
                  plotly::plotlyOutput("predperf_out",  height = "750px")
                )
        ),
        # predimp_tab tab content
        tabItem(tabName = "predimp_tab",
                fluidPage(
                  h3("Predictor Importance"),
                  p("The figure below shows the relative importance of each predictor in describing whether or not taxa are
                    present or absent from a sample, as well as the mean."),
                  plotly::plotlyOutput("predimp_out",  height = "750px")
                )
        ),
        # predsurf_tab tab content
        tabItem(tabName = "predsurf_tab",
                fluidPage(
                  h3("Predictor Response Surfaces"),
                  p("The figure below shows the predicted effect of a predictor variable on the presence/absence and mean. 
                    The predicted effects can be colour by a third variable to identify interactions among predictors"),
                  fluidRow(
                    column(4,offset = 1,selectInput("shap_pred_sel","Predictor",pred_names,multiple=F)),
                    column(4,offset = 1,selectInput("shap_col_sel","Colour",pred_names,multiple=F))
                  ),
                  plotly::plotlyOutput("predsurf_out",  height = "750px")
                )
        )
      )
    )
  )
  
)