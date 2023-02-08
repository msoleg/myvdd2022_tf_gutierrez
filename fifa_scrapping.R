

# Es nesario tenes instalado Java JDK (Yo tengo la version 19.0.2)
# debe estar incluido el path de JavaJDK a las variable de sistema 


# chequeamos primero la version de nuestro chrome visitando chrome://version/
# Vemos al incio la version de chrome. En micaso 109.0.5414.120
# Luego consultamos las versiones disponibles de chromedirver escibiendo binman::list_versions("chromedriver") en la consila
# Puede ocurrir que no coincida, lo que haremos será 
# install.packages("RSelenium")
library(tidyverse)
library(RSelenium)
library(netstat)

rd <- rsDriver(browser = "firefox",
               chromever = NULL,
               verbose = FALSE, port = 8081L)


remDr <- rd$client

# # abrimos el navegador
# remDr$open()

# navegamos a la pagina
remDr$navigate('https://sofifa.com/teams?keyword=')

# maximizamos la ventana
remDr$maxWindowSize()

# realizamos la busqueda
search_box <- remDr$findElement(using = 'name', 'keyword')
search_box$sendKeysToElement(list('Argentina', key = 'enter'))

global <- remDr$findElement(using = 'xpath', '/html/body/div[1]/div/div[2]/div/table/tbody/tr/td[3]/span')
  
ataque <- 
  
mediocampo <-
  
defensa <- 
  

# cliqueamos en la primer opcion
logo_object <- remDr$findElement(using = 'xpath', '/html/body/div[1]/div/div[2]/div/table/tbody/tr/td[1]/img')

url_logo <- logo_object$getText()
url_logo

team_object <- remDr$findElement(using = "xpath", "/html/body/div[1]/div/div[2]/div/table/tbody/tr/td[2]/a[1]/div")
team_object$clickElement()

