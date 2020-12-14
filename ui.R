## ui.R ##
header <- dashboardHeader(title = span(tagList(icon("diagnoses"), "Tablero de Mando COVID-19")),
                          tags$li(a(href = 'https://bran.mora.github.io/',
                                    target="_blank", "Comando de Operaciones COVID-19 Region Cusco",
                                    img(src = 'favicon_trans.png',
                                    title = "Comando COVID", height = "25px"),
                                    style = "padding-top:13px; padding-bottom:9px;"),
                                  class = "dropdown")
                          )

sidebar <- dashboardSidebar(
  sidebarMenuOutput("Side_dash")
  )

css <- ".shiny-output-error { visibility: hidden; }
        .shiny-output-error:before { visibility: hidden; }
       "


body <- dashboardBody(
  
  useShinyjs(),
  
  tags$head(tags$link(rel = "shortcut icon", href = "favicon_trans.png")),

  tags$style(make_css(list('.dygraph-legend', 
                           c('left', 'background-color'), 
                           c('70px !important', 'transparent !important')))),
  
  tags$head(
    tags$style(
      HTML(
        ".checkboxgroup-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
         .checkboxgroup-inline+.checkboxgroup-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
        "
      )
    )
  ),
  
  tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
  
  shinyEffects::setShadow(class = "box"),
  shinyEffects::setShadow(id = "my-progress"),
  
  shinyWidgets::useSweetAlert(),

  setSliderColor(color = "DeepSkyBlue",
                 sliderId = 1),
  
  chooseSliderSkin("Flat"),
  
  tags$style(type="text/css", css),
  
  tabItems(
    tabItem(tabName = "corTab",
            class = 'active',
            fluidRow(
              column(width = 4,
              box(title = span(icon("magic"), " Selecciona provincia y horizonte de proyeccion"),
                  solidHeader = F, status = "info",
                  collapsible = TRUE, width = NULL,
                  uiOutput("selector_country"),
                  uiOutput("slider_n_days_forec"),
                  htmlOutput("text_date_update")
              ),
              box(
                title = span(icon("share-alt"), " Data en repositorio:"),
                status = "info",
                solidHeader = F,
                footer = "Enlace para el repositorio en github.",
                collapsible = TRUE, width = NULL, collapsed = TRUE,
                # htmlOutput("text_accounts"),
                socialButton(
                  url = "https://github.com/branmora/covid-19-cusco",
                  type = "github"
                )
              )
              ),
              box(title = span(icon("table"), " Tabla de provincias"),
                  footer = "Tabla esta ordenada por casos activos totales. Nuevos casos = casos en el dia pasado (24h).",
                  status = "info",
                  solidHeader = F,
                  collapsible = TRUE, width = 4, collapsed = F,
                  DTOutput("dt_countries_cases")
                  ),
              box(
                title = span(icon("info-circle"), " Informacion acerca del tablero y proyeccion"),
                status = "info",
                solidHeader = T,
                collapsible = FALSE, width = 4, collapsed = F,
                htmlOutput("informative_text")
              )
            ),
            fluidRow(
              tabBox(
                title = span(icon("chart-area"), " Casos para la provincia seleccionada"),
                id = "tabset1",
                tabPanel("Solo nuevos casos",
                         # htmlOutput("cases_text"),
                         dygraphOutput("dygraph_country_new_cases") %>% withSpinner(color = "#5bc0de")
                         )
                # tabPanel("Casos cumulativos",
                #          htmlOutput("cases_text"),
                #          dygraphOutput("dygraph_country_cases") %>% withSpinner(color = "#5bc0de")
                #          )

              ),
              box(title = span(icon("chart-line"), " Casos cumulativos totales proyectados para el pais seleccionado +
                               90% intervalo de prediccion"),
                  solidHeader = F, status = "primary",
                  collapsible = TRUE, width = 6,
                  htmlOutput("cases_forec_text"),
                  dygraphOutput("dygraph_country_cases_forecast") %>% withSpinner(color = "#5bc0de")
              )
            ),
            fluidRow(
              tabBox(
                title = span(icon("chart-area"), " Muertes para la provincia seleccionada"),
                id = "tabset2",
                tabPanel("Solo nuevas muertes",
                         # htmlOutput("death_text"),
                         dygraphOutput("dygraph_country_new_deaths") %>% withSpinner(color = "#5bc0de")
                )
                # tabPanel("Muertes cumulativas",
                #          htmlOutput("death_text"),
                #          dygraphOutput("dygraph_country_deaths") %>% withSpinner(color = "#5bc0de")
                #          )
              )
            )
            ),
    tabItem(tabName = "regionTab",
            fluidRow(
              box(
                title = span(icon("info-circle"), " Informacion acerca del tablero y proyeccion"),
                status = "info",
                solidHeader = T,
                collapsible = TRUE, width = 12, collapsed = F,
                htmlOutput("texto_semaforo")
              ),
              gradientBox(
                title = "Evaluacion Global: Verde",
                width = 12,
                height = 100,
                icon = "fa fa-microchip",
                gradientColor = "green", 
                boxToolSize = "lg", 
                closable = FALSE,
                collapsible = FALSE,
                footer = "Tres de los cuatro indicadores se encuentran en estado verde"
              ),
              box(title = span(icon("table"), " Indicadores regionales"),
                  solidHeader = F, status = "warning",
                  collapsible = FALSE, width = 12,
                  valueBoxOutput("valuebox_total_cases_world") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_total_deaths_world") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_total_active_world") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_total_recov_world") %>% withSpinner(color = "#5bc0de"),
              )
            ),
            fluidRow(
              tabBox(
                title = span(icon("chart-area"), " Casos para la region"),
                id = "tabset3",
                tabPanel("Casos nuevos",
                         dygraphOutput("dygraph_world_new_cases") %>% withSpinner(color = "#5bc0de")
                )
                # tabPanel("Casos acumulados",
                #          htmlOutput("cases_text_world"),
                #          dygraphOutput("dygraph_world_cases") %>% withSpinner(color = "#5bc0de")
                # )

              ),
              box(title = span(icon("chart-line"), " Casos acumulados totales pronosticados para la region +
                               90% intervalo de prediccion superior"),
                  solidHeader = F, status = "primary",
                  collapsible = TRUE, width = 6,
                  dygraphOutput("dygraph_world_cases_forecast") %>% withSpinner(color = "#5bc0de")
              )
            ),
            fluidRow(
              tabBox(
                title = span(icon("chart-area"), " Fallecimientos en la region"),
                id = "tabset4",
                tabPanel("Solo nuevos fallecimientos",
                         dygraphOutput("dygraph_world_new_deaths") %>% withSpinner(color = "#5bc0de")
                )
                # tabPanel("Cumulativo de fallecimientos",
                #          htmlOutput("death_text_world"),
                #          dygraphOutput("dygraph_world_deaths") %>% withSpinner(color = "#5bc0de")
                # )
              ),
              box(title = span(icon("chart-line"), " Fallecimientos acumulados totales pronosticados para la region +
                               90% intervalo de prediccion superior"),
                  solidHeader = F, status = "primary",
                  collapsible = TRUE, width = 6,
                  dygraphOutput("dygraph_world_deaths_forecast") %>% withSpinner(color = "#5bc0de")
                  
              )
            ),
            fluidRow(
              tabBox(
                title = span(icon("chart-area"), "% de Camas Disponibles"),
                id = "tabset5",
                tabPanel("Camas UCI",
                         dygraphOutput("dygraph_region_camas") %>% withSpinner(color = "#5bc0de")
                         )
              )
            )
        ),
    
  tabItem(tabName = "Acuerdos",
          fluidRow(
            plotlyOutput("graph")
          ),
          fluidRow(
            reactableOutput("table2")
            )
          ),
  tabItem(tabName = "Sala",
          fluidRow(
            tags$iframe(
              seamless = "seamless", 
              src = "https://diresacusco2020.maps.arcgis.com/apps/opsdashboard/index.html#/a28819de66a14c059498b7e02f08a781", 
              height = 1200, width = 1900)
          )
  )
  )
)

ui <- function(req) {
  
  dashboardPagePlus(
    title = "COVIDCusco",
    enable_preloader = TRUE,
    loading_duration = 2.1,
    header = header,
    sidebar = sidebar,
    body = body,
    skin = "blue-light",
    md = TRUE
  )
  
}
