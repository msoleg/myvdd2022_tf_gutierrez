# TRABAJO FINAL
Febrero 2023<br>
Repositorio para la entrega del trabajo final correspondiente al curso **Manejo y Visualización de Datos**
> Alumna: María Soledad Gutiérrez <br>
> Docentes: Mgs. Marcos Prunello y Mgs. Diego Marfetán Molina <br>
> __Maestría en Estadística Aplicada. UNR__

<br>

<br>


La metodología elegida se resume en los siguientes pasos:

1. **OBTENCIÓN DE LA DATA Y PROCESAMIENTO INICIAL** Se realizó un scraping de la página oficial de la [FIFA](https://sofifa.com/teams?keyword=) (en francés Fédération Internationale de Football Association) con el fin de obtener información sobre los 32 equipos participantes de la __"Copa Mundial de Fútbol Qatar 2022"__. También se unió lo obtenido a la tabla de posiciones finales obtenida desde [SPORTINGNEWS](https://www.sportingnews.com/ar/futbol/news/posiciones-finales-mundial-qatar-2022/crblvcbnrg0yxbkkucxvzb9r). 

Herramientas: 
    - Librería `RSelenium` para la navegación entre las landing pages de los equipos.
    - Librería `rvest` para extraer tablas HTML completas.
    - Librarí `dplyr` para la manipulación y transformación básica de los datos. 
    
2. **VALIDACIÓN Y PROCESAMIENTO DE LOS DATOS** Se realizó una validación de los datos obtenidos en la etapa anterior para asegurar la correcta recolección. Luego, se aplicó la limpieza y procesamiento de los mismos. En esta etapa se incluyen tareas tales como quitar contenidos extras del nombre del jugador, separar la altura y peso en las distintas unidades, cambiar a español las posiciones de los jugadores, quitar campos que no serán utilizados (tales como valor y sueldo que se encuentran en 0), etc. 


3. **VISUALIZACIÓN** Se opta por realizar una aplicación web __Shiny__ para mostrar la exploración del dataset.

<br>

_Tal como se detalla en la consigna del trabajo final, este repositorio es PÚBLICO_

***



