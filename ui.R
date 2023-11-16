library(leaflet)
fluidPage(
  # Application title
  titlePanel("NAICS Viewer"),
  
sidebarLayout(
  # Sidebar with a slider and selection inputs
  sidebarPanel(
    selectInput("NAICS_selection", "Choose NAICS code:",
                choices = uniqueNAICS,
                selected = 0),
    selectInput("POI_selection", "Select Individual POI: ",
                choices = NULL),
    sliderInput("Month_selection",
                "Dates:",
                min = min(as.Date(overall_trips$month)),
                max = max(as.Date(overall_trips$month)),
                value=as.Date("2019-12-01"),
                timeFormat="%Y-%m"),
    plotOutput("plot_months"),
  ),

  # Show Word Cloud
  mainPanel(
    leafletOutput("outputmap",height = 1000)
  )
),
#Data Explorer tab
tabPanel("Data Explorer",
  fluidRow(
    column(3,
      selectInput("data_naics", "Naics:",
                  c("All Naics"="",uniqueNAICS),
                  multiple=TRUE)
    ),
    column(3,
      selectInput("data_cities", "City:",
                  c("All cities"=""),
                  multiple=TRUE)  
    ),
    column(3,
      conditionalPanel("input.data_naics",
        selectInput("data_POI", "Health POI:",
                    c("All POIs"=""),
                    multiple=TRUE))
    ),
    column(3,
      sliderInput("data_month", "Months:",
                  min = min(as.Date(overall_trips$month)),
                  max = max(as.Date(overall_trips$month)),
                  value = as.Date("2019-12-01"),
                  timeFormat = "%Y-%m")
    )
  ),
  fluidRow(
    column(1,
      
    )
  )
)

)
