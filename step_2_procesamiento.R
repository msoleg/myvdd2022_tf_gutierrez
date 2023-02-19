#
# PREPARACION DE LA DATA - TRABAJO FINAL
# RAMO: Manejo y Visualizacion de Datos - Maestria en Estadistica Aplicada - UNR
#
# ALUMNA: Maria Soledad Gutierrez
#

############################ CONSIDERACIONES ###################################

# Se requiere tener instalados los siguientes paquetes:
#     - readr
#     - dplyr
#     - tidyr
#

############################ CARGA DE PAQUETES ################################

# Instalo paquetes necesarios en caso de no estar instalados previamente
paquetes <- c("readr", "dplyr", "tidyr")
instalados <- paquetes %in% rownames(installed.packages())
if (any(instalados == FALSE)) {
  install.packages(paquetes[!instalados])
}

library(dplyr, quietly = T)

###############################################################################
############################ CARGA DE PAQUETES ################################
# cargo la data cruda
raw_data <- readr::read_delim(file = "step_3_ShinyApp/data/raw_data.csv", delim = '|', show_col_types = F)

head(raw_data) 

###############################################################################
###################### CHEQUEOS DE CONSISTENCIA ###############################

# nombres en español y claros que coinciden con los campos buscados
colnames(raw_data)

# no hay duplicados
sum(duplicated(raw_data)) == 0 # TRUE

# no hay valores null
sum(is.na(raw_data))+sum(is.null(raw_data)) == 0 # TRUE

# la PK de la tabla es el ID del jugador
n_distinct(raw_data$ID_JUGADOR) == nrow(raw_data)  # TRUE

# la relacion nombre Jugador <--> id jugador es uno a uno:
(nrow(unique(raw_data[ , c('NOMBRE', 'ID_JUGADOR')])) == n_distinct(raw_data$ID_JUGADOR)) ==
  (n_distinct(raw_data$ID_JUGADOR) == n_distinct(raw_data$NOMBRE))  # TRUE

# hay 32 equipos
n_distinct(raw_data$ID_EQUIPO) == 32  # TRUE

# la relacion EQUIPO <--> ID_EQUIPO es uno a uno:
(nrow(unique(raw_data[ , c('EQUIPO', 'ID_EQUIPO')])) == n_distinct(raw_data$ID_EQUIPO)) ==
  (n_distinct(raw_data$ID_EQUIPO) == n_distinct(raw_data$EQUIPO))  # TRUE

# la cant. de filas coincide con las esperadas: 31 equipos con 26 jugadores y 1 con 25
nrow(raw_data) == 26*31+25 # TRUE

# la cantidad de registros por equipos es la correcta: (Es correcto que iran tenga 25)
raw_data %>% group_by(EQUIPO) %>%
  summarise(total_count = n(), .groups = 'drop') %>%
  print(n = 32)

# los tipos de datos son correctos 
# (sin tener en cuenta que falta procesar ALTURA y PESO, y que a fines de visualizacion
# no nos importa el tipo de dato de los IDS.
sapply(raw_data, class) %>% as.data.frame()

# las edades extraidas tienen sentido
summary(raw_data$EDAD)

# hay 4 equipos por grupo
raw_data %>% group_by(GRUPO) %>%
  summarise(total_teams = n_distinct(ID_EQUIPO), .groups = 'drop')


###############################################################################
########## TRANSFORMACION DE DATOS Y GENERACION DE NUEVOS CAMPOS ##############

# to
ls_posiciones <- c("Portero",
                   "Lateral Derecho", "Defensa Central", "Carrillero Derecho", "Carrillero Izquierdo",
                   "Medio Centro Defensivo", "Medio Centro", "MEdio Centro Ofensivo", "Medio Izquierdo", "Medio Derecho",
                   "Extremo Derecho", "Extremo Izquierdo", "Lateral Izquierdo", "Delantero Centro", "Media Punta")
# from
names(ls_posiciones) <- c("GK",
                          "RB", "CB", "RWB", "LWB",
                          "CDM", "CM", "CAM", "LM", "RM",
                          "RW", "LW", "LB", "ST", "CF")
data <- raw_data %>%
  mutate(
    # quito las posibles posiciones del jugador de la cadena de su nombre
    NOMBRE = vapply(strsplit(NOMBRE, "GK|RWB|CB|LWB|CM|RW|ST|RM|LW|CDM|CAM|LB|LM|RB|CF"), `[`, 1, FUN.VALUE=character(1)),
    # paso a espanol los campos de pie favorito
    PIE_FAVORITO = recode(PIE_FAVORITO, "Right" = "Derecho", "Left" = "Izquierdo"),
    # guardo en otro campo las descripciones del las posiciones
    MEJOR_POSICION_DESC = recode(MEJOR_POSICION, !!!ls_posiciones),
    # me quedo con el peso en kg
    "PESO (KG)" = as.integer(vapply(strsplit(PESO, "kg"), `[`, 1, FUN.VALUE=character(1))),
    # me quedo con la altura en cm
    "ALTURA (CM)" = as.integer(vapply(strsplit(ALTURA, "cm"), `[`, 1, FUN.VALUE=character(1))),
    # calculo del IMC
    "IMC (KG/M^2)" = `PESO (KG)`/((`ALTURA (CM)`/100)**2),
    # reemplazo algunas imagenes que no están en la pag de la fifa pero son necesarias
    SRC_JUGADOR = recode(SRC_JUGADOR, "https://cdn.sofifa.net/players/272/483/23_60.png" = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTxYKbGXDxAMuqzU0GS6zIALNogukHLYxA6Qj8HICRl8JMRcnnFsUFDcpXONg3ncK2APSM&usqp=CAU",
    "https://cdn.sofifa.net/players/272/470/23_60.png" = "https://cdn.resfu.com/img_data/players/medium/892591.jpg?size=300x&lossy=1")
  ) %>%
  # separo la informacion de equipo y contrato
  tidyr::separate(EQUIPO_Y_CONTRATO, into = c("EQUIPO_ACTUAL", "CONTRATO"), sep = "\\s\\([0-9]+\\)\\s", extra = "drop") %>%
  # remuevo los campos sin info o que no utilizare
  select(-c("VALOR", "SALARIO", "ALTURA", "PESO"))

################################################################################
#################### GUARDADO DE LA DATA PROCESADA #############################

readr::write_delim(data, file = "step_3_ShinyApp//data/clean_data.csv", delim = '|')

