#
# VISUALIZACION DE LOS DATOS - TRABAJO FINAL
# RAMO: Manejo y Visualizacion de Datos - Maestria en Estadistica Aplicada - UNR
#
# ALUMNA: Maria Soledad Gutierrez
#

############################ CONSIDERACIONES ###################################

# Se requiere tener instalados los siguientes paquetes:
#     - readr
#     - dplyr
#     - shiny
#     - shinythemes
#     - ggplot2
#     - plotly
#     - leafet
#     - geojsonio
#     - htmltools
#     - tidyr

################################################################################
##################### CARGA Y/O INSTALACION DE PAQUETES ########################

# Instalo paquetes necesarios en caso de no estar instalados previamente
paquetes <- c("readr", "dplyr", "tidyr", "DT",
              "shiny", "shinythemes", "shinydashboard",
              "ggplot2", "plotly", "leaflet", "geojsonio",
              "htmltools"
              )
instalados <- paquetes %in% rownames(installed.packages())
if (any(instalados == FALSE)) {
  install.packages(paquetes[!instalados])
}

library(shiny, quietly = T)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(plotly, quietly = T)
library(leaflet)
library(dplyr)
options(dplyr.summarise.inform = FALSE)
library(htmltools)


################################################################################
############################### CARGA DE DATOS #################################

# cargo los datos provenientes del scraping
ruta_data <- file.path(getwd(), "data/clean_data.csv") # armo la ruta al archivo ya que lo tengo en otra carpeta
data <- readr::read_delim(file = ruta_data, delim = '|', show_col_types = F)


# datos geo json, los cuales fueron descargados de: https://github.com/johan/world.geo.json
ruta_poly <- file.path(getwd(), "geodata/countries.geo.json") 
WorldCountry <-geojsonio::geojson_read(ruta_poly, what = "sp")


######################## OBJETOS PARA EL GRAFICO DE MAPA #######################

orden <- list("Argentina", "Australia", "Belgium", "Brazil", "Canada", "Switzerland", "Cameroon", "Costa Rica",
              "Germany", "Denmark", "Ecuador", "Spain", "France","United Kingdom",  "Ghana", "Croatia",
              "Iran", "Japan", "South Korea", "Morocco", "Mexico", "Netherlands", "Poland", "Portugal",
              "Qatar", "Saudi Arabia", "Senegal","Republic of Serbia", "Tunisia","Uruguay", "United States of America")

names(orden) <- c("Argentina", "Australia", "Bélgica", "Brasil", "Canadá", "Suiza", "Camerún", "Costa Rica",
                  "Alemania", "Dinamarca", "Ecuador", "España", "Francia", "Inglaterra","Ghana", "Croacia",
                  "Irán", "Japón", "Corea del Sur", "Marruecos", "México", "Países Bajos", "Polonia",  "Portugal",
                  "Qatar", "Arabia Saudita",  "Senegal", "Serbia", "Túnez", "Uruguay", "Estados Unidos")

# filtro por los paises del mundial (falta Gales)
data_Map <- WorldCountry[WorldCountry$name %in% orden, ]

# gerero el input de data que usaré para visualizar dentro del mapa
data_summary <- data %>% 
  # filtro gales porque no tengo su polígono
  filter(EQUIPO != 'Gales') %>%
  # calculo promedios por equipos
  group_by(EQUIPO) %>%
  summarise(across(c("PUESTO", "PTS", "ALTURA (CM)", "IMC (KG/M^2)", "PESO (KG)",
                     "PJE_GLOBAL", "PJE_ATAQUE", "PJE_MEDIOCAMPO", "PJE_DEFENSA"),
                   list(mean = mean))) %>%
  # renombro las columnas
  rename("PUESTO" = "PUESTO_mean",
         "PUNTOS"= "PTS_mean",
         "PROMEDIO ALTURA (CM)" = "ALTURA (CM)_mean",
         "PROMEDIO IMC (KG/M^2)" = "IMC (KG/M^2)_mean",
         "PROMEDIO PESO (KG)" = "PESO (KG)_mean",
         "PUNTAJE GLOBAL" = "PJE_GLOBAL_mean",
         "PUNTAJE ATAQUE" = "PJE_ATAQUE_mean",
         "PUNTAJE MEDIOCAMPO" = "PJE_MEDIOCAMPO_mean",
         "PUNTAJE DEFENSA" = "PJE_DEFENSA_mean") %>%
  # ordeno por paises siguiendo como esta puesto en el json
  arrange(factor(EQUIPO, levels = names(orden)))

data_summary_map <- data_summary %>%
  tidyr::gather(VARIABLE, VALOR, PUESTO:`PUNTAJE DEFENSA`) %>%
  # ordeno por paises siguiendo como esta puesto en el json
  arrange(factor(EQUIPO, levels = names(orden)))


##################### OBJETOS PARA LAS TARJETAS ###############################

long_team_data <- data %>% 
  # calculo promedios por equipos
  group_by(EQUIPO) %>%
  summarise(across(c("ALTURA (CM)", "IMC (KG/M^2)", "PESO (KG)", 
                     "EDAD", "VALORACION_GENERAL", "POTENCIAL"),
                   list(mean = mean))) %>%
  # renombro las columnas
  rename("ALTURA (CM)" = "ALTURA (CM)_mean",
         "IMC (KG/M^2)" = "IMC (KG/M^2)_mean",
         "PESO (KG)" = "PESO (KG)_mean", 
         "EDAD" = "EDAD_mean",
         "VALORACION_GENERAL" = "VALORACION_GENERAL_mean",
         "POTENCIAL" = "POTENCIAL_mean") %>%
  # paso a formato largo 
  tidyr::gather(VARIABLE, VALOR, `ALTURA (CM)`:POTENCIAL)

long_player_data <- data %>%
  # selecciono solo las columnas a usar para el formato largo
  select(c("ID_JUGADOR", "NOMBRE", "EQUIPO",
         "ALTURA (CM)", "IMC (KG/M^2)", "PESO (KG)",
         "EDAD", "VALORACION_GENERAL", "POTENCIAL")) %>%
  # paso a formato largo
  tidyr::gather(VARIABLE, VALOR, `ALTURA (CM)`:POTENCIAL)


######################### OBJETOS PARA EL BOXPLOT #############################

long_data_boxplot <- data %>%
  # selecciono solo las columnas a usar para el formato largo
  select(c("ID_JUGADOR", "MEJOR_POSICION_DESC",
           "ALTURA (CM)", "IMC (KG/M^2)", "PESO (KG)",
           "EDAD", "VALORACION_GENERAL", "POTENCIAL")) %>%
  # paso a formato largo
  tidyr::gather(VARIABLE, VALOR, `ALTURA (CM)`:POTENCIAL)


####################### OBJETOS PARA EL HISTOGRAMA #############################

long_data_histo <- data %>%
  # selecciono solo las columnas a usar
  select(c("EQUIPO","ALTURA (CM)", "IMC (KG/M^2)", "PESO (KG)", "EDAD" )) %>%
  # paso a formato largo
  tidyr::gather(VARIABLE, VALOR, `ALTURA (CM)`:EDAD)



################################################################################
############################ INTERFAZ DE USUARIO ###############################
ui <- fluidPage(
  theme = shinytheme("flatly"),

  navbarPage(
    title = "World Cup Qatar 2022 ",
    id = "main_navbar",
    tabPanel("Dashboard General", 
             sidebarPanel(
               width = 3,
               img(src="https://i.pinimg.com/originals/ec/82/a5/ec82a51e2d701406118f5d010b7fa0ab.png",
                   width=250,
                   align = "center"),
               br(),
               h2(strong("Filtros disponibles")),
               
               br(),
               h3("MAPA"),
               selectInput(
                 inputId = "metrica_equipo", 
                 label = "Seleccione una métrica general de equipo:",
                 choices = c(colnames(data_summary))[2:length(data_summary)], 
                 selected = "PUESTO", 
               ),
               br(),
               h3("TARJETAS Y BOXPLOT"),
               radioButtons(
                 inputId = "metrica_jugador", # ID del widget
                 label = "Seleccone una métrica de jugador:", # Título a mostrar en la app
                 choices = c("ALTURA (CM)", "IMC (KG/M^2)", "PESO (KG)", "EDAD", "VALORACION_GENERAL", "POTENCIAL"), # Opciones disponibles
                 selected = "POTENCIAL" # Opción seleccionada inicialmente
               ),
               br(),
               em(p("Esta aplicación es parte del trabajo final para el curso Manejo y",
                 strong("\"Visualización de Datos\""), "de la Maestría en Estadística Aplicada de la U.N.R."),
               p("El código de desarrollo puede encontrarse en el siguiente enlace: ", 
                 a(href = "https://github.com/msoleg/myvdd2022_tf_gutierrez", "repo GitHub"))),
               
             ),
             mainPanel(
                fluidRow(
                 tags$hr(style="border-color: purple;"),
                 column(12, 
                        leafletOutput("mapa", height = 350)
                      ),
                 em("El país \"Gales\" no fué incluido en el mapa por ausencia del polígono asociado dentro de los datos."),
                 tags$hr(style="border-color: purple;"),
                ),  
               fluidRow(
                 br(),
                column(4, align="center",
                       br(),
                       h5(strong(textOutput("titulo_1_max"))),
                       h3(strong(textOutput("nombre_equipo_max"))),
                       br(),
                       uiOutput("src_logo_equipo_max"),
                       br(),
                       br(),
                       ),
                column(4, align="center",
                       br(),
                       h5(strong(textOutput("titulo_2_max"))),
                       h3(strong(textOutput("nombre_jugador_max"))),
                       br(),
                       uiOutput("src_jugador_max"),
                       br(),
                       br(),
                       ),
                column(4, align="center",
                      br(),
                      h5(strong(textOutput("titulo_3_min"))),
                      h3(strong(textOutput("nombre_jugador_min"))),
                      br(),
                      uiOutput("src_jugador_min"),
                      br(),
                      br(),
                      ),
                    ),
               
               fluidRow(
                 column(12, style ="height:650px;", 
                        br(),
                        plotlyOutput("boxplot"),
                        br()
                        )
                 
                 ),
               fluidRow(
                 tags$hr(style="border-color: purple;"),
                 column(12,
                   h4(strong("INSIGHTS:")),
                   p("- Los paises con mejores puntajes globales son los que quedaron en los puestos más altos de la copa mundial."),
                   p("- Si bien Brasil es el equipo de mejor promedio valoración general y potencial no fué el equipo ganador."),
                   p("- Messi se encuentra en el podio de mejor valoración general."),
                   p("- Los porteros están dentro de los jugadores más altos."),
                   p("- Los carrilleros están dentro de los jugadores más jóvenes."),
                   br(),
                   br()
                   
                 )
               ),
             ),
            ), # end tab General
          
    tabPanel("Dashboard por Equipo",
        sidebarPanel(
         width = 3,
         img(src="https://i.pinimg.com/originals/ec/82/a5/ec82a51e2d701406118f5d010b7fa0ab.png",
             width=250,
             align = "center"),
         br(),
         h2(strong("Filtros disponibles")),
         
         br(),
         
         h3(strong("EQUIPOS")),
         selectInput(
           inputId = "equipo", 
           label = "Seleccione un equipo participante de la Copa:",
           choices = unique(data$EQUIPO), 
           selected = "Argentina", 
         ),
         br(),
         br(),
         br(),
         h3("DISTRIBUCIÓN"),
         radioButtons(
           inputId = "distribucion", # ID del widget
           label = "Seleccione una variable para analizar su distribución: \n (arrastra el filtro de equipo)", # Título a mostrar en la app
           choices = c("ALTURA (CM)", "IMC (KG/M^2)", "PESO (KG)", "EDAD"),
           selected = "EDAD",
         ),
         br(),
         
         ),
        mainPanel(
          # fila de nombre de equipo
          fluidRow(
            column(2, 
                   uiOutput("equipo_seleccionado_logo"),
                   tags$hr(style="border-color: purple;")
                   ),
            column(2,
                   uiOutput("equipo_seleccionado_flag"),
                   tags$hr(style="border-color: purple;")
            ),
            column(8, 
                   h1(strong(textOutput("equipo_seleccionado")))
            ),
          fluidRow(
              
              )
          ),
          # fila de tarjetas
          fluidRow(
            column(2,
                   br(),
                   box(title = "PUESTO", 
                       # status = "info",
                       # solidHeader = TRUE,
                       # background = "light-blue",
                       height = "100px",
                       textOutput("puesto")
                   )
              ),
              column(2,
                   br(),
                   box(title = "PUNTOS", 
                       # status = "info",
                       # solidHeader = TRUE,
                       # background = "light-blue",
                       height = "100px",
                       textOutput("puntos")
                   )
              ),
            column(2,
                   br(),
                   box(title = "PARTIDOS GANADOS", 
                       # status = "info",
                       # solidHeader = TRUE,
                       # background = "light-blue",
                       height = "100px",
                       textOutput("pg")
                   )
            ),
            column(2,
                   br(),
                   box(title = "PARTIDOS PERDIDOS", 
                       # status = "info",
                       # solidHeader = TRUE,
                       # background = "light-blue",
                       # height = "100px",
                       textOutput("pp")
                   )
            ),
            column(2,
                   br(),
                   box(title = "PARTIDOS EMPATADOS", 
                       # status = "info",
                       # solidHeader = TRUE,
                       # background = "light-blue",
                       height = "100px",
                       textOutput("pe")
                   )
            ),
            column(2,
                   br(),
                   box(title = "PARTIDOS JUGADOS", 
                       # status = "info",
                       # solidHeader = TRUE,
                       # background = "light-blue",
                       height = "100px",
                       textOutput("pj")
                   )
            ),

          ),
          # fila de tabla
          fluidRow(
            tags$hr(style="border-color: purple;"),
            br(),
            column(12,
                   style ="height:600px;",
                   DT::dataTableOutput("tabla_equipo"))
          ),
          # fila de graficos
          fluidRow(
            # columna histograma
            column(6, #style ="height:400;",
                   plotlyOutput("histograma"),
                   br(),
                   br(),
                   tags$hr(style="border-color: purple;"),
                   ),
            # columna scatter
            column(6, #style ="height:450;",
                   plotlyOutput("scatter_edad_potencial"),
                   em("En general a mayor edad, menor potencial"),
                   br(),
                   br(),
                   tags$hr(style="border-color: purple;"),
                   
                   ),
            
          ),
          
      )
    ), # end tab Equipo
    
    
    tabPanel("Comentarios",
      sidebarPanel(
        width = 12,
        img(src="https://i.pinimg.com/originals/ec/82/a5/ec82a51e2d701406118f5d010b7fa0ab.png",
            width=250,
            align = "center"),
        br(),
      ),
      mainPanel(
        
        h4("La metodología elegida para el trabajo final se resume en los siguientes pasos:"),
          br(),
          h5(strong("1. OBTENCIÓN DE LA DATA Y PROCESAMIENTO INICIAL")), 
          p("Se realizó un scraping de la página oficial de la ",
          a(href = "https://sofifa.com/teams?keyword=", "FIFA"), 
          "(en francés Fédération Internationale de Football Association) con el fin de obtener información sobre los 32 equipos participantes de la",
          em("Copa Mundial de Fútbol Qatar 2022."),
          "También se unió lo obtenido a la tabla de posiciones finales obtenida desde",
          a(href="https://www.sportingnews.com/ar/futbol/news/posiciones-finales-mundial-qatar-2022/crblvcbnrg0yxbkkucxvzb9r", "SPORTINGNEWS")), 

          em("Herramientas:"),
          p("- Librería RSelenium para la navegación entre las landing pages de los equipos."),
          p("- Librería rvest para extraer tablas HTML completas."),
          p("- Librería dplyr para la manipulación y transformación básica de los datos. "),

          br(),
        
          h5(strong("2. VALIDACIÓN Y PROCESAMIENTO DE LOS DATOS")),
          p("Se realizó una validación de los datos obtenidos en la etapa anterior para asegurar la correcta recolección.
          Luego, se aplicó la limpieza y procesamiento de los mismos.
          En esta etapa se incluyen tareas tales como quitar contenidos extras del nombre del jugador,
          separar la altura y peso en las distintas unidades, cambiar a español las posiciones de los jugadores, 
          quitar campos que no serán utilizados (tales como valor y sueldo que se encuentran en 0), etc. "),

          br(),
        
          h5(strong("3. VISUALIZACIÓN")),
          p("Se opta por realizar una aplicación web Shiny para mostrar la exploración del dataset.")

        
      )
    ) # end tab Comentarios
        
  )
             
)


################################ SERVIDOR ######################################

servidor <- function(input, output) {

  #............................................................................
  # GRAFICO DE MAPA
  #............................................................................
  atributo_equipo_reactivo <- reactive({
    data_summary_map %>%
      filter(VARIABLE == input$metrica_equipo)
    })
  
  output$mapa <- renderLeaflet({
    # colores para el mapa
    paleta <- colorNumeric(palette = c("#DF0101","#8E3279", "#BE87C6", "#E9D6E9"),
                           domain = atributo_equipo_reactivo()$VALOR)
    
    titulo <- tags$div(
      tags$style(HTML("
        .leaflet-control.map-title { 
          transform: translate(10%, -140%);
          padding-left: 10px;
          padding-right: 20px;
          padding-top: 10px;
          background: rgba(255,255,255,0.75);
          font-weight: bold;
          font-size: 16px;}")),
            HTML(paste(input$metrica_equipo, "DE LOS EQUIPOS DE LA COPA MUNDIAL 2023"))
    )  
    leaflet(data_Map) %>% 
      addProviderTiles("OpenStreetMap.DE") %>%
      addPolygons(
        color = ~paleta(atributo_equipo_reactivo()$VALOR),
        weight = 2,
        opacity = 1,
        dashArray = '3',
        fillOpacity = 0.5,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        # texto a mostrar al pasar con el cursor:
        label = paste0(
          "<b>País:</b> ",data_summary$EQUIPO,
          "<br><b>PUESTO EN LA COPA MUNDIAL:</b> ", data_summary$PUESTO,
          "<br><b>VALOR SELECCIONADO:</b> ", round(atributo_equipo_reactivo()$VALOR , 2)
        ) %>% lapply(HTML)
        ,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px")) %>%
      addControl(titulo, position = "topleft", className="map-title") %>%
      addLegend(position = "bottomright", title="",
                pal = paleta, values = ~atributo_equipo_reactivo()$VALOR, opacity = 1)
  })
  

  #............................................................................
  # VISUALIZACIONES DE TARJETAS/LOGOS
  #............................................................................
  jugador_reactivo <- reactive({
    long_player_data %>%
      filter(VARIABLE == input$metrica_jugador)
    })
  
  equipo_reactivo <- reactive({
    long_team_data %>%
      filter(VARIABLE == input$metrica_jugador)
  })


  ## OUTPUTS DE TARJETAS:
  output$titulo_1_max <- renderText({paste("El país de mayor valor promedio de ", input$metrica_jugador, " es: ")})
  output$nombre_equipo_max <- renderText({
    as.character(
      equipo_reactivo() %>% slice_max(VALOR) %>% slice(1) %>% select("EQUIPO")
    )})
  output$src_logo_equipo_max <- renderUI({
    eq <- equipo_reactivo() %>% slice_max(VALOR) %>% slice(1) %>% select("EQUIPO")
    tags$img(
      src = unique(data[data$EQUIPO == eq[[1]],]$SRC_LOGO),
      width = 80,
      height = 80
    )
  })

  output$titulo_2_max <- renderText({paste("El jugador de mayor valor de ", input$metrica_jugador, " es: ")})
  output$nombre_jugador_max <- renderText({
    id_jug <- jugador_reactivo() %>% slice_max(VALOR) %>% slice(1) %>% select("ID_JUGADOR")
    paste(
    as.character(data[data$ID_JUGADOR == id_jug[[1]],]$NOMBRE),
    paste0("(", 
    as.character(data[data$ID_JUGADOR == id_jug[[1]],]$EQUIPO),
    ")"))})
  output$src_jugador_max <- renderUI({
    id_jug <- jugador_reactivo() %>% slice_max(VALOR) %>% slice(1) %>% select("ID_JUGADOR")
    tags$img(
      src = data[data$ID_JUGADOR == id_jug[[1]],]$SRC_JUGADOR,
      width = 80,
      height = 80
    )
  })
  
  output$titulo_3_min <- renderText({paste("El jugador de menor valor de ", input$metrica_jugador, " es: ")})
  output$nombre_jugador_min <- renderText({
    id_jug <- jugador_reactivo() %>% slice_min(VALOR) %>% slice(1) %>% select("ID_JUGADOR")
    paste(
      as.character(data[data$ID_JUGADOR == id_jug[[1]],]$NOMBRE),
      paste0("(", 
             as.character(data[data$ID_JUGADOR == id_jug[[1]],]$EQUIPO),
             ")"))})
  output$src_jugador_min <- renderUI({
    id_jug <- jugador_reactivo() %>% slice_min(VALOR) %>% slice(1) %>% select("ID_JUGADOR")
    tags$img(
      src = data[data$ID_JUGADOR == id_jug[[1]],]$SRC_JUGADOR,
      width = 80,
      height = 80
      )
    })
  
  #............................................................................
  # GRAFICO BOXPLOT
  #............................................................................
  data_boxplot <- reactive({
    long_data_boxplot %>%
      filter(VARIABLE == input$metrica_jugador) %>%
      tidyr::spread(VARIABLE, VALOR) %>%
      rename(VAR = input$metrica_jugador)
  })
  
  output$boxplot <- renderPlotly({
    var_name <- colnames(data_boxplot())[3]
    grafico <- ggplot(data_boxplot()) +
      aes(x = MEJOR_POSICION_DESC, y = VAR) +
      geom_boxplot(show.legend = F, fill = "#A0C9D6") +
      labs(x = "MEJOR POSICION", y = paste(input$metrica_jugador))  +
      ggtitle(paste("DISTRIBUCIÓN DE ", input$metrica_jugador, " SEGÚN POSICIÓN")) +
      theme_bw() + 
      theme(axis.title=element_text(size=15, color="#147196"),
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size=12),
            plot.title=element_text(size=17, face="bold", color="#0B4861", hjust=0.5))

    ggplotly(grafico, height = 600)
    
    })

  
  #............................................................................
  # VALORES PARA EL EQUIPO SELECCIONADO
  #............................................................................
  
  filtro_equipo <- reactive({
    data %>%
      filter(EQUIPO == input$equipo) %>%
      slice(1)
  })
  
  output$equipo_seleccionado_logo <- renderUI({
    tags$img(src = filtro_equipo()$SRC_LOGO, height = 80)
  })
  output$equipo_seleccionado_flag <- renderUI({
    tags$img(src = filtro_equipo()$SRC_FLAG, height = 80)
  })
  output$equipo_seleccionado <- renderText({input$equipo})  
  output$puesto <- renderText({filtro_equipo()$PUESTO})
  output$puntos <- renderText({filtro_equipo()$PTS})
  output$pj <- renderText({filtro_equipo()$PJ})
  output$pg <- renderText({filtro_equipo()$PG})
  output$pp <- renderText({filtro_equipo()$PP})
  output$pe <- renderText({filtro_equipo()$PE})
  
  
  #............................................................................
  # VISUALIZACION TABLA DE EQUIPO
  #............................................................................
  data_equipo <- reactive({
    data %>%
      filter(EQUIPO == input$equipo) %>%
      select(c("NOMBRE", "EDAD", "VALORACION_GENERAL", "POTENCIAL", "EQUIPO_ACTUAL",
               "CONTRATO", "PIE_FAVORITO", "MEJOR_POSICION_DESC"))
  })
  output$tabla_equipo <- DT::renderDataTable(data_equipo())
  
  
  #............................................................................
  # GRAFICO HISTOGRAMA
  #............................................................................  
  data_histograma <- reactive({
    long_data_histo %>%
      # filtro por el equipo y por la variable seleccionada por usuario
      filter((EQUIPO == input$equipo)&(VARIABLE == input$distribucion))
  })
  
  
  output$histograma <- renderPlotly({
    
    var_name <- unique(data_histograma()$VARIABLE)
    
    grafico <- ggplot(data_histograma()) +
      aes(x = VALOR, y = ..density..) +
      geom_histogram(fill = "#A0C9D6", col = "black", binwidth = 3) +
      labs(x = var_name, y = "CANTIDAD DE JUGADORES") +
      ggtitle(paste("Distribución de la variable ", var_name )) +
      theme_bw() + 
      theme(axis.title=element_text(size=11, color="#147196"),
            plot.title=element_text(size=13, face="bold", color="#0B4861", hjust=0.5))

    ggplotly(grafico, height = 400)

  })
  
  
  #............................................................................
  # GRAFICO DISPERSION
  #............................................................................
  output$scatter_edad_potencial <- renderPlotly({
    
    grafico <- ggplot(data_equipo()) +
      aes(x = EDAD, y = POTENCIAL) +
      geom_jitter(width = 0.5, height = 0, alpha = 0.5, color = "#147196") +
      ggtitle(" Relación entre la edad y el potencial \n de los jugadores") +
      theme_bw() + 
      theme(axis.title=element_text(size=11, color="#147196"),
            plot.title=element_text(size=13, face="bold", color="#0B4861", hjust=0.5))
    
    ggplotly(grafico, height = 400)
    
  })
  


}


################################################################################
################################## RUN APP #####################################

shinyApp(ui = ui, server = servidor)



