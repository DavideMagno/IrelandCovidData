shinyServer(function(input, output, session) {
    
# GLOBAL REACTIVES ---------------------------------------------------------------

    selection <- reactiveValues(n = NA)
    
    observeEvent(input$map_shape_click, {
        selection$n <- input$map_shape_click$id
    })
    observeEvent({
        input$map_click
    }, {
        selection$n <- NA
    })
    
    field <- reactive({
        if (grepl("National", map.type())) {
            input$field
        } else {
            "Total"
        }
    })
    
    map.type <- reactive({if(is.na(selection$n)) "National" else "County"})
    
    output$type <- reactive({
        return(ifelse(grepl("National", map.type()), TRUE, FALSE))
    })
    outputOptions(output, 'type', suspendWhenHidden = FALSE)
    
    logscale <- reactive({if(input$log) "log" else "linear"})
    data.logscale <- reactive({if(input$data_increments | !input$data.log) 
        "linear" else "log"
        })
    
    increments <- reactive({
        if(input$data_increments) "Increment" else "Cumulative"
    })
    

# MAP ---------------------------------------------------------------------

    IrelandMap <- reactive({
        FilterAndPrepareToPlot(Data, input$date, "County", "Total")
        })
    
    output$map <- leaflet::renderLeaflet({
        leaflet::leaflet() %>% 
            leaflet::addProviderTiles("Esri.WorldTerrain") %>% 
            leaflet::setView(-7.5, 53.2, zoom = 7)})
    
    observe({
        map.features <- DrawProxyMap(IrelandMap(), "County", "Total")
        leaflet::leafletProxy("map", data = IrelandMap(), session) %>% 
            leaflet::clearShapes(.) %>% 
            leaflet::removeControl("legend") %>% 
            leaflet::addPolygons(fillColor= ~map.features$pal(cases),
                                 fillOpacity = 0.5, 
                                 weight = 2, 
                                 color = "grey",
                                 popup = map.features$polygon_popup,
                                 label = ~name,
                                 layerId = ~name) %>%
            leaflet::addLegend("bottomleft", pal = map.features$pal, 
                               values = ~cases,
                               labFormat = leaflet::labelFormat(digits = 0),
                               opacity = 0.5,
                               title = "Legend",
                               layerId = "legend")
    })
    

# MAP GRAPH ---------------------------------------------------------------

    
    
    name <- reactive({if(is.na(selection$n)) "Ireland" else selection$n})
    
    
    plot.data <- reactive({
        if (grepl("Ireland", name())) {
            filter.by <- ""
            type <- "National"
        } else {
            filter.by <- name()
            type <- "County"
        }
        Extract(Data = Data, filter_by = filter.by, type = type, 
                select_field = field())
    })

    best.worst.data <- reactive({
        Extract(Data = Data, filter_by = "", type = "County",
                select_field = "Total", select_method = "Ratio",
                start_date = input$date, end_date = input$date)
    })

    observe({
        if (grepl("Total", field())) {
            text <- paste(field(), "Positive Cases")
        } else {
            text <- paste("Positive Cases", field())
        }
        output$dynamic <- plotly::renderPlotly({
            plot.data() %>%
                as.data.frame() %>%
                plotly::plot_ly(x = ~Date) %>%
                plotly::add_trace(y = ~get(field()), type = "scatter",
                                  mode = "lines+markers") %>%
                plotly::layout(
                    title = paste("Dynamic in", name()),
                    yaxis = list(title = text,
                                 type = logscale())
                ) %>%
                plotly::config(displayModeBar = FALSE)
        })
    })

    observe({
        wrangle.data <- best.worst.data()
        wrangle.data %<>%
            dplyr::top_n(13) %>%
            dplyr::mutate(Group = "Top 13 changes") %>%
            dplyr::bind_rows(wrangle.data %>%
                                 dplyr::top_n(-13) %>%
                                 dplyr::mutate(Group = "Lower 10 changes"))
        
        output$best.worst.plot <- plotly::renderPlotly({
            wrangle.data %>% 
                dplyr::mutate(County = forcats::fct_reorder(County, Total)) %>% 
                plotly::plot_ly(y = ~County, color = ~Group) %>%
                plotly::add_trace(x = ~Total, type = "bar",
                                  orientation = 'h') %>%
                plotly::layout(
                    title = paste0("Daily % change at ", input$date),
                    yaxis = list(title = "County"),
                    xaxis = list(tickformat = ".2%"),
                    font = list(size = 10),
                    legend = list(orientation = 'h')
                ) %>%
                plotly::config(displayModeBar = FALSE)
        })
    })

# DATA ANALYSIS -----------------------------------------------------------

    observe({
        fields <- if (is.null(input$regions)) {
            c("Hospitalised", "In ICU",
              "Dead", "Clusters","In Clusters",
              "Imported", "Healthcare", "Total")
        } else {
            c("Total")
        }
        stillSelected <- isolate(input$data.field[input$data.field %in% fields])
        updateSelectizeInput(session, "data.field", choices = fields,
                             selected = stillSelected, server = TRUE)
    })

    analysis.table <- reactive({
        input.data <- PrepareDataForExtraction(input$regions, input$date.range, 
                                               last.date, input$data.field)
        data <- Extract(Data, filter_by = input.data$filter_by,
                        select_method = increments(), type = input.data$type,
                        select_field = input.data$data.field,
                        start_date = input.data$start_date,
                        end_date = input.data$end_date)

        return(list(input.data = input.data, data = data))
    })

    output$plots <- renderUI({
        get_plot_output_list_div(analysis.table()$input.data$data.field,
                                 analysis.table()$data,
                                 data.logscale())
    })

    observe({
        data <- Extract(Data, filter_by = analysis.table()$input.data$filter_by,
                        select_method = "Ratio",
                        type = analysis.table()$input.data$type,
                        select_field = analysis.table()$input.data$data.field,
                        start_date = analysis.table()$input.data$start_date,
                        end_date = analysis.table()$input.data$end_date)

        output$plots.ratio <- renderUI({
            get_plot_output_list_div(analysis.table()$input.data$data.field,
                                     data,"linear", TRUE)
            })
    })

    output$analysis_table <- DT::renderDataTable({
        DT::datatable(analysis.table()$data %>%
                          dplyr::arrange(desc(Date)),
                      rownames = FALSE,
                      options = list(bFilter=0,autoWidth = TRUE,
                                     columnDefs = list(list(width = '200px',
                                                            className = 'dt-center',
                                                            targets = "_all"))))
    })
})
