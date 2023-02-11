#
# VISUALIZACION DE LOS DATOS - TRABAJO FINAL
# RAMO: Manejo y Visualizacion de Datos - Maestria en Estadistica Aplicada - UNR
#
# ALUMNA: Maria Soledad Gutierrez
#

############################ CONSIDERACIONES ###################################

# Se requiere tener instalados los siguientes paquetes:
#     - readr
#     - shiny
#



############################ CARGA DE PAQUETES ################################

# Instalo paquetes necesarios en caso de no estar instalados previamente
paquetes <- c("readr", "shiny", "ggplot2", "plotly")
instalados <- paquetes %in% rownames(installed.packages())
if (any(instalados == FALSE)) {
  install.packages(paquetes[!instalados])
}

library(shiny)
library(ggplot2)
library(plotly, quietly = T)

###############################################################################
############################# CODIGO PRINCIPAL ################################

# armo la ruta al archivo ya que lo tengo en otra carpeta
ruta_data <- file.path(getwd(), "data/clean_data.csv")
# cargo los datos
data <- readr::read_delim(file = ruta_data, delim = '|', show_col_types = F)

MiInterfaz <- fluidPage( #Estructura general de la web
  titlePanel("App test"), #Título de la web
  sidebarLayout( #Función para crear paneles
    sidebarPanel(), #Panel secundario
    mainPanel() #Panel principal
  )
)

MiServidor <- function(input, output) {
  
  output$MiGrafico <- renderPlotly({
    
    grafico <- data %>% 
      filter(
        GRUPO == "GRUPO A"
      ) %>% 
      ggplot() +
      aes(x = EQUIPO, y = POTENCIAL, text = EQUIPO) +
      geom_point(color = "red") +
      ggtitle("Películas estrenadas en 2009 de género Drama") +
      theme_bw()
    
    ggplotly(grafico)
    
  })
}

shinyApp(ui = MiInterfaz, server = MiServidor)



