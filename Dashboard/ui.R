library(shiny)
library(shinyjs)
library(billboarder)
library(shinycssloaders)

# Define UI for application that draws a histogram
navbarPage("Irish Covid19 Dashboard", id="nav",
           tabPanel("Interactive Map", 
                    fluidRow(
                      column(4),
                      column(4,
                             dateInput("date", "Select the date for the regional map and statistics", 
                                       last.date, 
                                       min = as.Date("2020-03-23"),
                                       max = last.date),
                             align="center"
                             )
                    ),
                    fluidRow(
                      column(12,
                             includeCSS(here::here("Dashboard/styles.css")),
                        leaflet::leafletOutput("map", height = 710),
                        absolutePanel(id = "controls", class = "panel panel-default",
                                      fixed = FALSE, top = 15, draggable = TRUE,
                                      left = "auto", right = 60, bottom = "auto",
                                      width = 330, height = "auto", cursor = "default",
                                      p(),
                                      # Change Condition
                                      conditionalPanel(condition = "output.type == true",
                                                       selectInput("field", "Data for the national graph",
                                                                   choices = c("Hospitalised", "In ICU",
                                                                               "Dead", "Clusters","In Clusters",
                                                                               "Imported", "Healthcare", "Total"),
                                                                   selected = "Total")),
                                      p(),
                                      plotly::plotlyOutput("dynamic", height = "300px"),
                                      checkboxInput("log", "Log scale", value = TRUE)
                        ),
                        absolutePanel(id = "bestworst", class = "panel panel-default",
                                      fixed = FALSE, top = 15, draggable = TRUE,
                                      left = 60, right = "auto", bottom = "auto",
                                      width = 400, height = "auto", cursor = "default",
                                      p(), p(),
                                      plotly::plotlyOutput("best.worst.plot", height = "450px")
                        )
                      )
                    )
           ),
           tabPanel("Data Explorer",
                    fluidRow(
                      column(4,
                             selectInput("regions", "Select the Regions of analysis",
                                         c("Ireland"= "",
                                           unique(Data$covid.regions$County)),
                                         multiple=TRUE),
                             align="center"),
                      column(4,
                             dateRangeInput("date.range", "Select the date range of analysis",
                                            start = as.Date("2020-03-23"),
                                            end = last.date,
                                            min = as.Date("2020-03-23"),
                                            max = last.date),
                             align="center"
                      ),
                      column(4,
                             selectInput("data.field", "Select the fields to analyse",
                                         c("Total" = ""),
                                         multiple=TRUE, selected = "Total"),
                             align="center"
                      )
                    ),
                    fluidRow(
                      column(1,
                             p(strong("Absolute figures"))
                      ),
                      column(1,
                             checkboxInput("data_increments", "Show daily increments", value = TRUE)
                      ),
                      column(1,
                             conditionalPanel(condition = "!input.data_increments",
                                              checkboxInput("data.log", "log scale", value = TRUE))
                      )
                    ),
                    fluidRow(
                      uiOutput("plots")
                    ),
                    fluidRow(
                      column(1,
                             p(strong("Relative increments"))
                      )
                    ),
                    fluidRow(
                      uiOutput("plots.ratio")
                    )
           )
)