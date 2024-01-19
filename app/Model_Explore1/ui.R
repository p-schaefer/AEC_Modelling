library(shinydashboard)

fluidPage(
  
  dashboardPage(
    dashboardHeader(title = "Model Explainer 1"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Model Performance", tabName = "mod_pref"),
        menuItem("Model Predictions", tabName = "pred1")
      )
    ),
    dashboardBody(
      tabItems(
        # mod_pref tab content
        tabItem(tabName = "mod_pref",fluidPage(
          h3("Model Performance"),
          p("The figure below shows model performance of 5-fold cross-validation. Four endpoints are listed as columns.
      Categorical and continuous endpoints are listed as rows. The y-axis shows the mean RMSE (for continuous endpoints) and AUC (for categorical endpoints).
      Note that RMSE scores for percent and raw endpoints can't be directly compared. The x-axis shows 5 different equalization
      strategies to upsample or downsample different percentiles of the data in order to equalize distribution of observations for model learning.
      Colours represent whether weighting was used. Weights were applied to samples at the segment level (i.e., if 2 samples were collected
      within a segment, each would be weighted with 0.5 so each segment recieved equal weights). 
      Percent endpoints have converted the raw abundance/biomass to a percent of the sample. The raw endpoints are
      standardized to abundance/biomass per 100m2^2. The Categorical endpoint converts raw and percent endpoints to one of
      5 categories based and percentiles of the data: 0-1, 1-20, 20-40, 40-60, 60-80, 80-100."),
          br(),
          plotly::plotlyOutput("mod_pref",height ="600px"))
        ),
        # pred1 tab content
        tabItem(tabName = "pred1",
                fluidPage(
                  h3("Model Predictions"),
                  uiOutput("pred1_selectors"),
                  tabBox(
                    width=12,
                    height="600px",
                    title = NULL,
                    id = "tabset1",
                    tabPanel("Observed\nvs.\nPredicted", fluidRow(plotly::plotlyOutput("pred1",height  = "600px"))),
                    tabPanel("Map", fluidPage(uiOutput("pred2_selectors"),leaflet::leafletOutput("map1", height = "600px"))),
                    tabPanel("Raw Data", uiOutput("rawdata1"))
                  ),
                  
                )
        )
      )
    )
  )
  
)
