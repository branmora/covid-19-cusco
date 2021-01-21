## app.R ##
function(input, output, session) {
  
  # Load scripts/modules -----
  
  # reading data
  source("01_scripts/read_data_cssegis.R")
  source("01_scripts/aggregate_data.R")
  source("01_scripts/read_populations.R")
  
  # Read traffic light data
  source("01_scripts/traffic_light.R")  
  
  # forecasting
  source("01_scripts/forecasting.R")
  
  # Read process data -----
  
  data1 <- fread("https://raw.githubusercontent.com/branmora/covid-19-cusco/master/1.%20ACTAS.csv", sep2 = ";", header = TRUE)
  data_bar <- fread("https://raw.githubusercontent.com/branmora/covid-19-cusco/master/data/bar_chart.csv", sep2 = ",", header = TRUE)

    
      # menu -----
  output$Side_dash <- renderMenu({
    
    sidebarMenu(
      id = "sideBar_Menu",
      menuItem("Semaforo Regional",
               icon = icon("globe"),
               tabName = "regionTab",
               startExpanded = F,
               selected = T
      ),
      menuItem("Semaforo Provincial",
               icon = icon("chart-line"),
               tabName = "corTab",
               startExpanded = F,
               selected = F
               ),
      menuItem("Seguimiento de Procesos",
               icon = icon("globe"),
               tabName = "Acuerdos",
               startExpanded = F,
               selected = F
               ),
      menuItem("Sala Situacional",
               icon = icon("globe"),
               tabName = "Sala",
               startExpanded = F,
               selected = F
      )
      )
  })
  
  observe({
    
    query <- parseQueryString(session$clientData$url_search)
    
    query1 <- paste(names(query), query, sep = "=", collapse = ", ")
    
    # print(query1)
    
    if (query1 == "tab=corTab") {
      
      updateTabItems(session, inputId = "sideBar_Menu", selected = "corTab")
      
    } else if (query1 == "tab=regionTab") {
      
      updateTabItems(session, inputId = "sideBar_Menu", selected = "regionTab")
      
    }
    
  })
  
  # informative text for this app -----
  output$informative_text <- renderUI({
    
    tags$html(tags$p("Esta aplicacion es para fines informativos y analiticos"),
              tags$p("La data viene de:",
                     tags$a(href = 'http://www.diresacusco.gob.pe/new/salacovid-2',
                            target="_blank", "Sala Situacional COVID-19 Region Cusco")),
              tags$p("El modelo de pronostico es el ETS (suavizado exponencial) implementado en un paquete del lenguaje R,
                       se utilizan datos historicos de la serie temporal objetivo (extrapolacion)."
                     )
             )

  })
  
  # informative text for this app -----
  output$texto_semaforo <- renderUI({
    
    tags$html(tags$p("A continuacion se presenta el tablero de Mando e indicadores principales"),
              tags$p("La data viene de:",
                     tags$a(href = 'http://www.diresacusco.gob.pe/new/salacovid-2',
                            target="_blank", "Sala Situacional COVID-19 Region Cusco")),
              tags$p("El modelo de pronostico es el ETS (suavizado exponencial) implementado en un paquete del lenguaje R,
                       se utilizan datos historicos de la serie temporal objetivo (extrapolacion)."
              )
    )
    
  })
  
    
  # read the data ----
  data_corona <- reactive({
    
    data_res <- join_all_corona_data()
    
    data_pop <- read_populations()

    data_res[data_pop,
             on = .(Country),
             Population := i.Population]
    
    data_res
    
  })
  
  #read beds data
  
  data_beds <- reactive({
    data_camas <- read_data_beds()
  })
  
  
  #read data 2
  
  data_cuscot <- reactive({
    data_cusco <- read_data_cusco()
  })
  
    
  # read traffic light data
  data_traffic <- reactive({
    
    data_trat <- traffic_light()
    
  })
  
  # date of update -----
  output$text_date_update <- renderUI({
    
    tags$html(tags$p(tags$b("Actualizado hasta: "), data_corona()[, max(DateRep)]
                     )
              )
    
  })
    
  # Country selector -----
  output$selector_country <- renderUI({
    
    pickerInput(
      inputId = "country",
      label = "Elige una provincia:", 
      choices = data_corona()[, unique(Country)],
      selected = "Cusco",
      options = list(
        `live-search` = TRUE,
         style = "btn-info",
         maxOptions = 7
        )
      )
    
  })
  
  # N days forecast slider ----
  output$slider_n_days_forec <- renderUI({
    
    sliderInput(
      inputId = "n_days_forec", 
      label = "Establezca con cuantos dias de anticipacion crear una prevision:",
      min = 1,
      max = 30,
      value = 7
    )
    
  })
  
  # Latest stats ----
  data_countries_stats <- reactive({
    
    data_res <- copy(data_corona())
    
    data_res_latest <- copy(data_res[,
                                     .SD[DateRep == max(DateRep)],
                                     by = .(Country)]
                            )
    
    setorder(data_res_latest, -Active_cases_cumsum)
    
    data_res_latest
    
  })
  
  # DT of most infected countries ----
  output$dt_countries_cases <- renderDataTable({
    
    data_res_latest <- copy(data_countries_stats())
    
    DT::datatable(data_res_latest[, .(Country,
                                      'Casos totales' = Cases_cumsum,
                                      'Fallecimientos totales' = Deaths_cumsum
                                      # 'Casos activos' = Active_cases_cumsum,
                                      #'Nuevos casos' = Cases
                                      # 'Casos Activos/ Poblacion Miles' = ceiling((Active_cases_cumsum / Population) * 1e6)
                                      # 'Tasa de positivos' = round((Cases_cumsum / TotalTests) * 100, 2),
                                      # 'Tasa/ Poblacion Miles' = Tests_1M_Pop
                                      )],
                  selection = "single",
                  class = "compact",
                  extensions = c('Buttons', 'Scroller'),
                  options = list(
                    pageLength = 10,
                    dom = 'Bfrtip',
                    deferRender = TRUE,
                    scrollY = 270,
                    scroller = TRUE,
                    buttons = c('csv', 'excel'),
                    scrollX = TRUE
                  ))
  })
  
  # Subset data by country ----
  data_country <- reactive({
    
    shiny::req(input$country)
    
    data_res <- copy(data_corona()[.(input$country), on = .(Country)])
    
    data_res
    
  })

  # Subset traffic by country ----
  data_select_traffic <- reactive({
    
    shiny::req(input$country)
    
    data_trat <- copy(data_traffic()[.(input$country), on = .(Country)])
    
    data_trat
    
  })
  
  # informative text for cases -----
  output$cases_text <- renderUI({
    
    tags$html(tags$p("Casos = Nuevos casos confirmados en el dia."),
              tags$p("Cumulativo de casos = Casos confirmados aculutados totales en el dia."),
              tags$p("Cumulativo Recuperados = Casos acumulados recuperados totales en el dia.")
              )
    
  })
  
  # Show cases of the selected country ----
  
  colMax <- function(data) sapply(data, max, na.rm = TRUE)
  
  # output$dygraph_country_cases <- renderDygraph({
  #   
  #   shiny::req(input$country)
  #   
  #   dygraph(data_country()[, .(DateRep, 'Cases cumulative' = Cases_cumsum, Cases)],
  #           main = input$country) %>%
  #     # dyAxis("y", label = "Cases") %>%
  #     dyRangeSelector(dateWindow = c(data_country()[, max(DateRep) - 50], data_country()[, max(DateRep) + 1]),
  #                     fillColor = "#5bc0de", strokeColor = "#222d32") %>%
  #     dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
  #               fillGraph = FALSE, fillAlpha = 0.4,
  #               drawPoints = TRUE, pointSize = 3,
  #               pointShape = "circle",
  #               colors = c("#5bc0de", "#FF6347", "#228b22")) %>%
  #     dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
  #     dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
  #   
  # })
  
  output$dygraph_country_new_cases <- renderDygraph({
    
    shiny::req(input$country)
    
    dygraph(data_country()[, .(DateRep, Cases)],
            main = input$country) %>%
      # dyAxis("y", label = "Cases") %>%
      dyRangeSelector(dateWindow = c(data_country()[, max(DateRep) - 50], data_country()[, max(DateRep) + 1]),
                      fillColor = "#5bc0de", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = FALSE, fillAlpha = 0.4,
                drawPoints = FALSE, pointSize = 3,
                pointShape = "circle",
                colors = c("#5bc0de")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE) %>%
      dyRoller(showRoller = FALSE, rollPeriod = 7) %>%
      dyShading(from = data_select_traffic()[, .(cases_q0)], to = data_select_traffic()[, .(cases_q1)], color = "#CCEBD6", axis = "y") %>%
      dyShading(from = data_select_traffic()[, .(cases_q1)], to = data_select_traffic()[, .(cases_q2)], color = "#f0f2c4", axis = "y") %>%
      dyShading(from = data_select_traffic()[, .(cases_q2)], to = data_select_traffic()[, .(cases_q3)], color = "#FFE6E6", axis = "y") 
    
  })
  
  # informative text for deaths -----
  output$death_text <- renderUI({
    
    tags$html(tags$p("Fallecidos = Nuevos casos confirmados de fallecimiento ese dia."),
              tags$p("Cumulativo de fallecimientos = Total de fallecimientos confirmados acumulados en ese dia.")
              )
    
  })
  
  # Show deaths of the selected country ----
  output$dygraph_country_new_deaths <- renderDygraph({
    
    shiny::req(input$country)
    
    dygraph(data_country()[, .(DateRep, Deaths)],
            main = input$country) %>%
      # dyAxis("y", label = "Deaths") %>%
      dyRangeSelector(dateWindow = c(data_country()[, max(DateRep) - 50], data_country()[, max(DateRep) + 1]),
                      fillColor = "#5bc0de", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = FALSE, pointSize = 3,
                pointShape = "circle",
                colors = c("#5bc0de")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE) %>%
      dyRoller(showRoller = FALSE, rollPeriod = 7)  %>%
      dyShading(from = data_select_traffic()[, .(deaths_q0)], to = data_select_traffic()[, .(deaths_q1)], color = "#CCEBD6", axis = "y") %>%
      dyShading(from = data_select_traffic()[, .(deaths_q1)], to = data_select_traffic()[, .(deaths_q2)], color = "#f0f2c4", axis = "y") %>%
      dyShading(from = data_select_traffic()[, .(deaths_q2)], to = data_select_traffic()[, .(deaths_q3)], color = "#FFE6E6", axis = "y") 
    
  })
  
  # output$dygraph_country_deaths <- renderDygraph({
  #   
  #   shiny::req(input$country)
  #   
  #   dygraph(data_country()[, .(DateRep, 'Deaths cumulative' = Deaths_cumsum, Deaths)],
  #           main = input$country) %>%
  #     # dyAxis("y", label = "Deaths") %>%
  #     dyRangeSelector(dateWindow = c(data_country()[, max(DateRep) - 50], data_country()[, max(DateRep) + 1]),
  #                     fillColor = "#5bc0de", strokeColor = "#222d32") %>%
  #     dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
  #               fillGraph = TRUE, fillAlpha = 0.4,
  #               drawPoints = TRUE, pointSize = 3,
  #               pointShape = "circle",
  #               colors = c("#5bc0de", "#228b22")) %>%
  #     dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
  #     dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
  #   
  # })
  # 
  #### Compute forecasts --------
  
  # Forecasting Cases cumulative -----
  data_cases_cumsum_forec <- reactive({
    
    req(input$country, input$n_days_forec)
    
    data_res <- copy(data_country())
    
    data_forec <- forec_cases_cumsum(data_res, input$n_days_forec)
    
    data_res <- rbindlist(list(
      data_res,
      data.table(DateRep = seq.Date(data_res[, max(DateRep) + 1],
                                    data_res[, max(DateRep) + input$n_days_forec],
                                    by = 1),
                 Cases_cumsum_mean = round(data_forec$forecast, digits = 0),
                 Cases_cumsum_lwr = floor(data_forec$forecast),
                 Cases_cumsum_upr = data_forec$upper
                 )
      ), fill = TRUE, use.names = TRUE
      )
    
    data_res[, Model := data_forec$model]
    
    data_res
    
  })
  
  # informative text for forecasted cases -----
  output$cases_forec_text <- renderUI({
    
    tags$html(
              tags$p("Casos cumulativos = Casos totales acumulados confirmados en el dia.")
              )
    
  })
  
  # Show forecasted cases of the selected country ----
  output$dygraph_country_cases_forecast <- renderDygraph({
    
    shiny::req(input$country, input$n_days_forec)
    
    data_res <- copy(data_cases_cumsum_forec())
    
    dygraph(data_res[, .(DateRep, 'Cases cumulative' = Cases_cumsum,
                         Cases_cumsum_mean, Cases_cumsum_lwr, Cases_cumsum_upr)],
            main = paste0(input$country,
                          ", model: ",
                          data_res[, unique(Model)])) %>%
      # dyAxis("y", label = "Cases - cumulative") %>%
      dySeries('Cases cumulative') %>%
      dySeries(c("Cases_cumsum_lwr", "Cases_cumsum_mean", "Cases_cumsum_upr"),
               label = "Cases cumulative - forecast") %>%
      dyRangeSelector(dateWindow = c(data_res[, max(DateRep) - input$n_days_forec - 10],
                                     data_res[, max(DateRep) + 1]),
                      fillColor = "#5bc0de", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = TRUE, pointSize = 3,
                pointShape = "circle",
                colors = c("#5bc0de", "#228b22")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyEvent(data_res[is.na(Cases_cumsum_mean), max(DateRep)],
              "Forecasting origin", labelLoc = "bottom") %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
    
  })

  #### Region aggregated -----------
  
  data_region <- reactive({
    
    data_res <- aggregate_data(data_corona())
    
    data_res
    
  })
  
  # Value boxes of world statistics -----
  
  output$valuebox_total_cases_world <- renderValueBox({
    
    valueBox(
      format("15.19%", nsmall=1, big.mark=","),
      "Tasa de positividad",
      icon = icon("ambulance"),
      color = "orange"
    )
    
  })
  
  output$valuebox_total_deaths_world <- renderValueBox({
    
    valueBox(
      format("1.72%", nsmall=1, big.mark=","),
      "Tasa de letalidad",
      icon = icon("skull"),
      color = "red"
    )
    
  })

  output$valuebox_total_recov_world <- renderValueBox({
    
    valueBox(
      format("74%", nsmall=1, big.mark=","),
      "Porcentaje de camas disponibles en nivel III de atencion",
      icon = icon("star-of-life"),
      color = "green"
    )
    
  })
  
  output$valuebox_total_active_world <- renderValueBox({
    
    valueBox(
      format("69,293", nsmall=1, big.mark=","),
      "Pruebas rapidas realizadas",
      icon = icon("hospital-alt"),
      color = "yellow"
      )
    
  })
  
  
  
  # informative text for cases -world -----
  output$cases_text_world <- renderUI({
    
    tags$html(tags$p("Casos = Nuevos casos confirmados en el dia."),
              tags$p("Casos cumulativo = Casos confirmados totales acumulados en el dia."),
              tags$p("Cumulativo recuperado = Casos acumulados recuperados totales en el dia.")
              )
    
  })
  
  # Show cases of the world ----
  # output$dygraph_world_cases <- renderDygraph({
  #   
  #   dygraph(data_region()[, .(DateRep, 'Cases cumulative' = Cases_cumsum, Cases)],
  #           main = "Region Cusco") %>%
  #     # dyAxis("y", label = "Cases") %>%
  #     dyRangeSelector(dateWindow = c(data_region()[, max(DateRep) - 80], data_country()[, max(DateRep) + 1]),
  #                     fillColor = "#5bc0de", strokeColor = "#222d32") %>%
  #     dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
  #               fillGraph = TRUE, fillAlpha = 0.4,
  #               drawPoints = TRUE, pointSize = 3,
  #               pointShape = "circle",
  #               colors = c("#5bc0de", "#FF6347", "#228b22")) %>%
  #     dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
  #     dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
  #   
  # })
  
  output$dygraph_world_new_cases <- renderDygraph({
    
    dygraph(data_cuscot()[, .(fecha_resultado, positivos)],
            main = "Region Cusco") %>%
      # dyAxis("y", label = "Cases") %>%
      dyRangeSelector(dateWindow = c(data_cuscot()[, max(fecha_resultado) - 80], data_cuscot()[, max(fecha_resultado) + 1]),
                      fillColor = "#5bc0de", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = FALSE, fillAlpha = 0.4,
                drawPoints = TRUE, pointSize = 1,
                pointShape = "circle",
                colors = c("#5bc0de")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE) %>%
      dyRoller(showRoller = FALSE, rollPeriod = 7) %>%
      dyShading(from = "0", to = "372.85", color = "#CCEBD6", axis = "y") %>%
      dyShading(from = "372.85", to = "1118.355", color = "#f0f2c4", axis = "y") %>%
      dyShading(from = "1118.355", to = "1491.14", color = "#FFE6E6", axis = "y")
    
  })
  
  # informative text for deaths - world -----
  output$death_text_world <- renderUI({
    
    tags$html(tags$p("Fallecimientos = Fallecimientos nuevos confirmadas en el dia."),
              tags$p("Cumulativo de fallecimientos = Fallecimientos cumulativos totales acumuladas en el dia.")
    )
    
  })
  
  # Show deaths of the world ----
  # output$dygraph_world_deaths <- renderDygraph({
  # 
  #   dygraph(data_region()[, .(DateRep, 'Deaths cumulative' = Deaths_cumsum, Deaths)],
  #           main = "Region Cusco") %>%
  #     # dyAxis("y", label = "Deaths") %>%
  #     dyRangeSelector(dateWindow = c(data_region()[, max(DateRep) - 80], data_country()[, max(DateRep) + 1]),
  #                     fillColor = "#5bc0de", strokeColor = "#222d32") %>%
  #     dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
  #               fillGraph = FALSE, fillAlpha = 0.4,
  #               drawPoints = TRUE, pointSize = 1,
  #               pointShape = "circle",
  #               colors = c("#5bc0de", "#228b22")) %>%
  #     dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
  #     dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
  #   
  # })
  
  output$dygraph_world_new_deaths <- renderDygraph({
    
    dygraph(data_cuscot()[, .(fecha_resultado, defunciones)],
            main = "Region Cusco") %>%
      # dyAxis("y", label = "Deaths") %>%
      dyRangeSelector(dateWindow = c(data_cuscot()[, max(fecha_resultado) - 80], data_cuscot()[, max(fecha_resultado) + 1]),
                      fillColor = "#5bc0de", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = FALSE, fillAlpha = 0.4,
                drawPoints = TRUE, pointSize = 1,
                pointShape = "circle",
                colors = c("#5bc0de")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)  %>%
      dyRoller(showRoller = FALSE, rollPeriod = 7) %>%
      dyShading(from = "0", to = "6.965", color = "#CCEBD6", axis = "y") %>%
      dyShading(from = "6.965", to = "20.895", color = "#f0f2c4", axis = "y") %>%
      dyShading(from = "20.895", to = "27.86", color = "#FFE6E6", axis = "y")
    
  })

  # Show beds data ----
  output$dygraph_region_camas <- renderDygraph({
    
    dygraph(data_beds()[, .(DateRep, UCI, NOUCI, NIVELII)],
            main = "Region Cusco") %>%
      dyRangeSelector(dateWindow = c(data_beds()[, max(DateRep) - 80], data_beds()[, max(DateRep) + 1]),
                      fillColor = "#5bc0de", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = FALSE, fillAlpha = 0.4,
                drawPoints = TRUE, pointSize = 1,
                pointShape = "circle",
                colors = c("#5bc0de", "#2069e0", "#192734")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE) %>%
      dyShading(from = "0", to = "0.25", color = "#CCEBD6", axis = "y") %>%
      dyShading(from = "0.25", to = "0.65", color = "#f0f2c4", axis = "y") %>%
      dyShading(from = "0.65", to = "1.5", color = "#FFE6E6", axis = "y")
      
  })
 
    
  # Forecasting Cases cumulative world -----
  data_cases_cumsum_forec_world <- reactive({
    
    data_res <- copy(data_region())
    
    data_forec <- forec_cases_cumsum(data_res, 10)
    
    data_res <- rbindlist(list(
      data_res,
      data.table(DateRep = seq.Date(data_res[, max(DateRep) + 1],
                                    data_res[, max(DateRep) + 10],
                                    by = 1),
                 Cases_cumsum_mean = round(data_forec$forecast, digits = 0),
                 Cases_cumsum_lwr = floor(data_forec$forecast),
                 Cases_cumsum_upr = data_forec$upper
      )
    ), fill = TRUE, use.names = TRUE
    )
    
    data_res[, Model := data_forec$model]
    
    data_res
    
  })
  
  # Show forecasted cases of the world ----
  output$dygraph_world_cases_forecast <- renderDygraph({
    
    data_res <- copy(data_cases_cumsum_forec_world())
    
    dygraph(data_res[, .(DateRep, 'Cases cumulative' = Cases_cumsum,
                         Cases_cumsum_mean, Cases_cumsum_lwr, Cases_cumsum_upr)],
            main = paste0("Region Cusco",
                          ", model: ",
                          data_res[, unique(Model)])) %>%
      # dyAxis("y", label = "Cases - cumulative") %>%
      dySeries('Cases cumulative') %>%
      dySeries(c("Cases_cumsum_lwr", "Cases_cumsum_mean", "Cases_cumsum_upr"),
               label = "Cases cumulative - forecast") %>%
      dyRangeSelector(dateWindow = c(data_res[, max(DateRep) - 10 - 10],
                                     data_res[, max(DateRep) + 1]),
                      fillColor = "#5bc0de", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = TRUE, pointSize = 3,
                pointShape = "circle",
                colors = c("#5bc0de", "#228b22")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyEvent(data_res[is.na(Cases_cumsum_mean), max(DateRep)],
              "Forecasting origin", labelLoc = "bottom") %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
    
  })
  
  # Forecasting Deaths cumulative for world -----
  data_deaths_cumsum_forec_world <- reactive({
    
    data_res <- copy(data_region())
    
    data_forec <- forec_deaths_cumsum(data_res, 10)
    
    data_res <- rbindlist(list(
      data_res,
      data.table(DateRep = seq.Date(data_res[, max(DateRep) + 1],
                                    data_res[, max(DateRep) + 10],
                                    by = 1),
                 Deaths_cumsum_mean = round(data_forec$forecast, digits = 0),
                 Deaths_cumsum_lwr = floor(data_forec$forecast),
                 Deaths_cumsum_upr = data_forec$upper
      )
    ), fill = TRUE, use.names = TRUE
    )
    
    data_res[, Model := data_forec$model]
    
    data_res
    
  })
  
  # Show forecasted deaths of the world ----
  output$dygraph_world_deaths_forecast <- renderDygraph({

    data_res <- copy(data_deaths_cumsum_forec_world())
    
    dygraph(data_res[, .(DateRep, 'Deaths cumulative' = Deaths_cumsum,
                         Deaths_cumsum_mean, Deaths_cumsum_lwr, Deaths_cumsum_upr)],
            main = paste0("Region Cusco",
                          ", model: ",
                          data_res[, unique(Model)])) %>%
      # dyAxis("y", label = "Deaths - cumulative") %>%
      dySeries('Deaths cumulative') %>%
      dySeries(c("Deaths_cumsum_lwr", "Deaths_cumsum_mean", "Deaths_cumsum_upr"),
               label = "Deaths cumulative - forecast") %>%
      dyRangeSelector(dateWindow = c(data_res[, max(DateRep) - 10 - 10],
                                     data_res[, max(DateRep) + 1]),
                      fillColor = "#5bc0de", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = TRUE, pointSize = 3,
                pointShape = "circle",
                colors = c("#5bc0de", "#228b22")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyEvent(data_res[is.na(Deaths_cumsum_mean), max(DateRep)],
              "Forecasting origin", labelLoc = "bottom") %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
    
  })
  
  # Tabla de Procesos Comando COVID
  
  output$table2 <- renderReactable({
    reactable(data1,
              groupBy = "Reunion",
              searchable = TRUE,
              filterable = TRUE,
              resizable = TRUE,
              highlight = TRUE,
              outlined = TRUE,
              striped = TRUE,
              compact = TRUE,
              defaultSortOrder = "desc",
              minRows = 10,
              columns = list(
                Acuerdo = colDef(minWidth = 250),
                Status = colDef(cell = function(value) {
                  class <- paste0("tag status-", tolower(value))
                  htmltools::div(class=class,value)
                })
              ))
  })
  
  
  output$graph <- renderPlotly({
    plot_ly(data_bar, x = ~Reunion, y = ~Porcentaje, type = 'bar')
  })
  
}
