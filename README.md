<!--
*** Thanks for checking out this README Template. If you have a suggestion that would
*** make this better, please fork the repo and create a pull request or simply open
*** an issue with the tag "enhancement".
*** Thanks again! Now go create something AMAZING! :D
-->





<!-- PROJECT SHIELDS -->
<!--
*** I'm using markdown "reference style" links for readability.
*** Reference links are enclosed in brackets [ ] instead of parentheses ( ).
*** See the bottom of this document for the declaration of the reference variables
*** for contributors-url, forks-url, etc. This is an optional, concise syntax you may use.
*** https://www.markdownguide.org/basic-syntax/#reference-style-links
-->


<!-- PROJECT LOGO -->
<br />
<p align="center">
  
  <a>
    <img src="ursus_all.jpg">
  </a>

  <h3 align="center">Herramienta que permite estimar la temperatura(LST) en cualquier punto de cualquier ciudad, analizando las características urbanas (edificios, vegetación, agua, ...) detectadas en el radio de 1 km. a la redonda. El sistema permite simular la temperatura que tendrían diferentes escenarios añadiendo diferentes elementos de infraestructura verde </h3>

  <p align="center">
    Conoce de forma simple e intuitiva como mejoraría la temperatura en las zonas más desfavorables de cualquier ciudad añadiendo infraestructuras verdes  con tan sólo unos "clicks". 
  </p>



<!-- TABLE OF CONTENTS -->
## Contenidos

* [Sobre el proyecto](#sobre-el-proyecto)
  * [Tecnologías](#Tecnologías)
* [Cómo empezar?](#Cómo_empezar?)
  * [Prerrequisitos](#Prerrequisitos)
  * [Instalación](#Instalación)
* [Uso](#Uso)
* [Contribuciones](#Contribuciones)
* [Contacto](#Contacto)
* [Agradecimientos](#Agradecimientos)



<!-- ABOUT THE PROJECT -->
## Sobre el proyecto

URSUS-LST_PREDICTION  es una herramienta de ayuda para la toma de decisiones en cuanto a la determinación de la ubicación óptima para la instalación de infraestructuras verdes en las ciudades, y en cuanto a la selección del tipo adecuado de infraestructura.

...

### Tecnologías

La tecnología empleada actualmente para el desarrollo de la aplicación es la siguiente:

* [R](https://cran.r-project.org/)
* [R-Studio](https://rstudio.com/)
* [R-Shiny](https://shiny.rstudio.com/)
* Librerías de R para trabajar con imágenes Lidar (Lidr) y satelitales (raster)
* [Lidar](http://centrodedescargas.cnig.es/CentroDescargas/index.jsp)
* Landsat-8 satellite images of the city to be analized+
* Sentinel-2 satellite images of the city to be analized



<!-- Cómo empezar? -->
## Cómo empezar?


Para obtener una copia de la aplicación, conocer los requisitos previos para su correcto funcionamiento y los pasos para su instalación, y como hacer uso de la misma, puede seguir los siguientes pasos.

### Prerrequisitos

Para el correcto funcionamiento del DashBoard o aplicación web, será necesario tener conexión a internet e instalar previamente R, R-Shiny, y el resto de librerías necesarias que se describen en el código del servidor (server.r) y en el código de la interfaz gráfica del dashboard (ui.r)

* R
* R-Shiny
* R-libraries
* Conección a internet



### Instalación


1. Clonar el repositorio
```sh
git clone https://github.com/ursusdm/URSUS_LST_PREDICTION.git
```
2. Lanzar la aplicación en un servidor shiny



<!-- USAGE EXAMPLES -->
## Uso

* Navegue por el mapa y seleccione un punto 
* Realice la predicción de la temperatura (Predecir LST)
* El sistema analizará los elementos urbanos a 1km. a la redonda
* El sistema mostrará los % de cada elemento urbano detectado (veg. escasa, densa, agua, edificios>12m, ...) en intervalos de 250m
* El sistema mostrará la temperatura predicha (LST predicha por el modelo) y la temperatura real en el punto
* El sistema permite la simulación de nuevos escenarios modificando los % de vegetación densa, moderada y escasa entre 0-250m.


[product-screenshot-1]
[product-screenshot-2]
[product-screenshot-3]
[product-screenshot-4]
[product-screenshot-5]
[product-screenshot-6]
[product-screenshot-7]



<!-- CONTRIBUTING -->
## Contribuciones

1. Clonar el proyecto
```sh
git clone https://github.com/ursusdm/ursusdm_pv.git
```
3. Commit. Preparar los cambios que se subirán a GitHub con un nombre representativo
```sh
git commit -m 'Add some Amazing Feature'
```
4. Push. Subir los cambios locales a GitHub
```sh
git push
```

5. Para descargar en local el código actual de la rama, realize un Pull 



<!-- CONTACT -->
## Contacto

Luis Pérez Urrestarazu  - lperez@us.es
</br>
José del Campo Ávila  - jcampo@uma.es
</br>
Domingo López Rodríguez  - dominlopez@uma.es
</br>
Francisco Rodríguez Gómez  - francisco.rdg.gmz@uma.es


<!-- CONTACT -->
## Aplicación

La aplicación está disponible en el siguiente enlace

#Project Link: [https://ursusdm.shinyapps.io/ursusdm_pv/](https://ursusdm.shinyapps.io/ursusdm_pv/)


<!-- ACKNOWLEDGEMENTS -->
## Agradecimientos

* Este trabajo ha sido apoyado por el proyecto RTI2018-095097-BI00 en la convocatoria de Proyectos I+D+i 2018 del Ministerio de Ciencia, Innovación 𝑦 Universidades, España.

[product-screenshot-1]: screenshot_1.png
[product-screenshot-2]: screenshot_2.png
[product-screenshot-3]: screenshot_3.png
[product-screenshot-4]: screenshot_4.png
[product-screenshot-5]: screenshot_5.png
[product-screenshot-6]: screenshot_6.png
[product-screenshot-7]: screenshot_7.png
