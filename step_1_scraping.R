#
# DATA ACQUISITION - TRABAJO FINAL
# RAMO: Manejo y Visualizacion de Datos - Maestria en Estadistica Aplicada - UNR
#
# ALUMNA: Maria Soledad Gutierrez
#

########################### CONSIDERACIONES ###################################

# 1. Es nesario tener instalado Java JDK (Yo tengo la version 19.0.2)
#    y debe estar incluido el path de JavaJDK a las variable de sistema.
#
# 2. El navegador elegido en la configuracion es firefox.
#    Mi version es 109.0.1 (64-bit).
#
# 3. Se requiere tener instalados los siguientes paquetes:
#     - readr  
#     - RSelenium
#     - rvest
#     - dplyr
#
# 4. Los acentos del archivo fueron omitidos.


############################ CARGA DE PAQUETES ################################

# Instalo paquetes necesarios en caso de no estar instalados previamente
paquetes <- c("readr","RSelenium", "rvest", "dplyr")
instalados <- paquetes %in% rownames(installed.packages())
if (any(instalados == FALSE)) {
  install.packages(paquetes[!instalados])
}

library(RSelenium)  # para realizar el scraping dinamico
library(rvest)      # para realizar scraping de tablas
library(dplyr)


############################### FUNCIONES ######################################
"-------------------------------------------------------------------------------
Funcion go_team_page
--------------------

Mediante RSelenium permite acceder a la pagina particular del equipo

Argumentos:
   - equipo: string, nombre del equipo a buscar

-------------------------------------------------------------------------------"
go_team_page <- function(equipo){
  # realizamos la busqueda por nombre de equipo
  search_box <- remDr$findElement(using = "name", "keyword")
  search_box$sendKeysToElement(list(equipo, key = "enter"))
  
  # doy tiempo a la pag que termine de mostrar los resultados de la busqueda
  Sys.sleep(1) 
  
  # cliqueamos en la primer opcion
  team_object <- remDr$findElement(using = "xpath", "/html/body/div[1]/div/div[2]/div/table/tbody/tr/td[2]/a[1]/div")
  team_object$clickElement()
  
  # doy tiempo a la pag que termine de cargar la informacion del equipo
  Sys.sleep(1) 
}


"------------------------------------------------------------------------------
Funcion get_table_team
----------------------

Cuando estamos dentro de la pagina del equipo, extrae la informacion presente
en la tabla de jugadores.

Salida:
  - dataframe con los campos:
          - NOMBRE
          - EDAD
          - VALORACION GENERAL
          - POTENCIAL
          - EQUIPO Y CONTRATO
          - VALOR
          - SALARIO
          - EQUIPO

-------------------------------------------------------------------------------"
get_table_team <- function(){
  
  Sys.sleep(2)
  data_table <- remDr$findElement(using = "xpath", "/html/body/div[3]/div/div[2]/div[2]/table")
  data_table_html <- data_table$getPageSource()
  page <- read_html(data_table_html %>% unlist())
  # convierto a objeto dataframe 
  df_team_raw <- html_table(page)[[1]]
  
  # filtro campos
  if(ncol(df_team_raw)==13){
    df_team <- df_team_raw[, c(1:12)]
  }
  else if(ncol(df_team_raw)==14){
    df_team <- df_team_raw[, c(2:13)]
  }
  else{
    print("ERROR")
  }
    
  # renombro los campos (a espanol)
  colnames(df_team) <- c("NOMBRE", "EDAD", "VALORACION_GENERAL", "POTENCIAL", "EQUIPO_Y_CONTRATO",  "ID_JUGADOR",
                         "ALTURA", "PESO", "PIE_FAVORITO", "MEJOR_POSICION", "VALOR", "SALARIO")
  return(df_team)
}


"-------------------------------------------------------------------------------
Funcion get_text
-----------------

Obtiene el texto a partir del xpath
-------------------------------------------------------------------------------"
get_text <- function(cliente, v_xpath){
  obj <- cliente$findElement(using = "xpath", v_xpath)
  return(obj$getElementText()[[1]])
}


"-------------------------------------------------------------------------------
Funcion get_src
---------------

Obtiene la url de una imagen a partir del xpath
-------------------------------------------------------------------------------"
get_src <- function(cliente, v_xpath){
  obj_scr <- cliente$findElement(using = "xpath", v_xpath)
  scr <- obj_scr$getElementAttribute('src')
  return(scr[[1]])
}


################################################################################
############################# CODIGO PRINCIPAL #################################

# inicio el server y creo el cliente
rd <- rsDriver(browser = "firefox",
                 chromever = NULL,
                 verbose = FALSE,
                 port = 5059L
                 )
remDr <- rd$client

# navego a la pagina de busqeuda de equipos
remDr$navigate("https://sofifa.com/teams?keyword=&r=202200&set=true")

# maximizamos la ventana
remDr$maxWindowSize()

# defino la lista de equipos a buscar
teams_worldcup <- c( 'Qatar', 'Ecuador', 'Senegal', 'Netherlands',
                     'England', 'Iran', 'United States', 'Wales',
                     'Argentina', 'Saudi Arabia', 'Mexico', 'Poland',
                     'France', 'Australia', 'Denmark', 'Tunisia',
                     'Spain', 'Costa Rica', 'Germany', 'Japan', 
                     'Belgium', 'Canada', 'Morocco', 'Croatia',
                     'Brazil', 'Serbia', 'Switzerland', 'Cameroon',  
                     'Portugal', 'Ghana', 'Uruguay', 'Korea Republic')

# inicializo el objeto en donde guardare las tablas de equipos
df_teams <- NULL

# itero sobre cada equipo de la copa mundial
for(idx in seq_along(teams_worldcup)){
  
  team = teams_worldcup[[idx]]
  print(team)
  
  # accedo a la pagina del equipo
  go_team_page(team)
  
  # la 1ra vez en la pag del equipo incluyo atributos a la tabla de jugadores
  if(idx==1){
    # abro dropdown de atributos de jugador
    remDr$findElement(using="xpath", "/html/body/div[3]/div/div[1]/details[1]/summary")$clickElement()
  
    # me posiciono en el buscador
    search <- remDr$findElement(using="xpath", "/html/body/div[3]/div/div[1]/details[1]/form/div[1]/div/div[1]/input")
    search$clickElement()
    
    # agrego atributos de interes
    for(atrib in c('ID', 'HEIGHT', 'WEIGHT', 'PREFERRED FOOT', 'BEST POSITION')){
      search$sendKeysToElement(list(atrib, key = "enter"))
      Sys.sleep(1)
    }
    
    # hago click en algun lugar
    remDr$findElement(using="xpath", "/html/body/div[3]/div/div[2]/div[1]/h5")$clickElement()
    
    # aplico los campos
    apply <- remDr$findElement(using = "xpath", "/html/body/div[3]/div/div[1]/details[1]/form/div[2]/button")
    apply$clickElement()
    Sys.sleep(1)
  }
  
  print('extrayendo tabla...')
  # extraigo la tabla de jugadores del equipo
  df_team <- get_table_team() %>% 
              # le pego nuevos campos referidos al equipo y a la img del jugador
              mutate(
                SRC_JUGADOR = paste("https://cdn.sofifa.net/players/" ,
                                    substr(ID_JUGADOR, 1, 3), "/",
                                    substr(ID_JUGADOR, 4, 6), "/23_60.png",
                                    sep=""),
                EQUIPO = team,
                URL_EQUIPO = remDr$getCurrentUrl()[[1]],
                ID_EQUIPO = strsplit(URL_EQUIPO, split='/')[[1]][5],
                SRC_LOGO = get_src(remDr, "/html/body/div[1]/div/div[2]/div/img"),
                SRC_FLAG = get_src(remDr, "/html/body/div[1]/div/div[2]/div/div[1]/div/a[1]/img"),
                PJE_GLOBAL = get_text(remDr, "/html/body/div[1]/div/div[2]/div/section/div[1]/div/span"),
                PJE_ATAQUE = get_text(remDr, "/html/body/div[1]/div/div[2]/div/section/div[2]/div/span"),
                PJE_MEDIOCAMPO = get_text(remDr, "/html/body/div[1]/div/div[2]/div/section/div[3]/div/span"),
                PJE_DEFENSA = get_text(remDr, "/html/body/div[1]/div/div[2]/div/section/div[4]/div/span")
              )
  Sys.sleep(1)
  
  # apendo la tabla al data.frame general
  df_teams <- bind_rows(df_teams, df_team)
  
  # vuelvo a la pagina de busqueda
  remDr$goBack()
  Sys.sleep(1)
}

# le agrego la información de grupos
df_teams$GRUPO <- rep(c("GRUPO A", "GRUPO B", "GRUPO C", "GRUPO D",
                        "GRUPO E", "GRUPO F", "GRUPO G", "GRUPO H"),
                      c(26*4, 26*4-1, rep(26*4, 6)))


# mapeo a espanol los nombres de paises
# to
ls_equipos <- c("Qatar", "Ecuador", "Senegal", "Países Bajos",
                "Inglaterra", "Irán", "Estados Unidos", "Gales",
                "Argentina", "Arabia Saudita", "México", "Polonia", 
                "Francia", "Australia", "Dinamarca", "Túnez",
                "España", "Costa Rica", "Alemania", "Japón",
                "Bélgica", "Canadá", "Marruecos", "Croacia",  
                "Brasil", "Serbia", "Suiza", "Camerún",
                "Portugal", "Ghana", "Uruguay", "Corea del Sur")
# from
names(ls_equipos) <- c('Qatar', 'Ecuador', 'Senegal', 'Netherlands',
                       'England', 'Iran', 'United States', 'Wales',
                       'Argentina', 'Saudi Arabia', 'Mexico', 'Poland',
                       'France', 'Australia', 'Denmark', 'Tunisia',
                       'Spain', 'Costa Rica', 'Germany', 'Japan', 
                       'Belgium', 'Canada', 'Morocco', 'Croatia',
                       'Brazil', 'Serbia', 'Switzerland', 'Cameroon',  
                       'Portugal', 'Ghana', 'Uruguay', 'Korea Republic')
df_teams$EQUIPO <- recode(df_teams$EQUIPO, !!!ls_equipos)


# busco información de tabla final de resultados en otra pagina:
remDr$navigate("https://www.sportingnews.com/ar/futbol/news/posiciones-finales-mundial-qatar-2022/crblvcbnrg0yxbkkucxvzb9r")
Sys.sleep(2)

tabla_pos_final <- remDr$findElement(using = "xpath", "/html/body/div[1]/div/main/div/div[1]/div[2]/div/div/div[5]/div/div[1]/table")
data_table_html <- tabla_pos_final$getPageSource()

df_pos_final <- html_table(read_html(data_table_html %>% unlist()))[[1]] %>% 
  rename(EQUIPO = "Selección",
         PUESTO = "Puesto",
         INSTANCIA_ALCANZADA = "Instancia alcanzada")

# hago el join final entre los 2 dataframes:
data_final <- df_teams %>% left_join(df_pos_final, by=c('EQUIPO'))

# cierro 
remDr$close()

# creo el subdirectorio (en caso de no existir) y exporto los datos a un csv
dir.create("data", showWarnings = FALSE)
readr::write_delim(data_final, file = "data/raw_data.csv", delim = '|')



