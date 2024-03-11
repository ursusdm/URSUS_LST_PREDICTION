#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

###########################Libraries####################################################################### 

library(igraph)
library(raster)
#library (rgdal)
library(RStoolbox)
#library(RGISTools)
library(zoom)
library(excelR)
library(leaflet)
library(tidyverse)
library(ggplot2)
library(widgetframe)
library (lidR)
library(rgl)
library(rasterVis)
library(shiny)
library(shinythemes)
library(ggmap)
library(leaflet.extras)
library(sp)
library(RColorBrewer)
library(viridis) 
library(gstat)
library(randomForest)
#library(shinyalert)

############################getExtent()#####################################################################

## Para cada imagen Lidar del catálogo, obtenemos su extensión, y devolvemos un df con las extensiones ##

############################################################################################################

getExtent <- function(catalogo) {
  extentList <- c ()
  for (i in 1:nrow(catalogo) ) {
    im <- catalogo [i,]
    # cada imagen lidar se reprojecta a latitud y longitud y cogemos la extensión
    e <- sf::st_bbox(im)
    #x lng y lat sW (abajo izq)   NE (arriba derecha)
    lng1 <- xmin (extent (e))
    lat1 <- ymin (extent (e))
    lng2 <- xmax (extent (e))
    lat2 <- ymax (extent (e))
    minDF <- cbind (lng1,lat1)
    maxDF <- cbind (lng2,lat2)
    fDF <- as.data.frame(rbind(minDF,maxDF))
    colnames (fDF) <- c("lng","lat")
    cord.UTM = SpatialPoints(cbind(fDF$lng, fDF$lat),
                             proj4string=crs(im))
    
    cord.DEC<- spTransform(cord.UTM, CRS("+proj=longlat"))
    lng1 <- xmin(extent(cord.DEC))
    lat1 <- ymin(extent(cord.DEC))
    lng2 <- xmax(extent(cord.DEC))
    lat2 <- ymax(extent(cord.DEC))
    extentList[[i]] <- c (lng1,lat1,lng2,lat2)
  }
  
  
  #dataFrame with img extents
  dfExtents = t(as.data.frame(extentList, optional = TRUE))
  colnames(dfExtents) = c("lng1", "lat1", "lng2", "lat2" )
  rownames(dfExtents) = 1:length (extentList)
  dfExtents <- as.data.frame (dfExtents)
  
  print (dfExtents)
  
  #closeAlert(num = 0, id = NULL)
  
  return(as.data.frame(dfExtents))
  
}



###################################### segmentRoofs () ##########################################################

## Función que segmenta los tejados del área de interés 

segmentRoofs<- function (aoi) {
  segmentedRoof <- filter_poi(aoi,Classification == LASBUILDING | Classification == LASGROUND)
  return (segmentedRoof)
}


#################################################################################################################

## Función que segmenta las zonas de escasa vegetación del área de interés 


segmentLowVeg<- function (aoi) {
  segmented <- filter_poi(aoi,Classification == LASLOWVEGETATION)
  return (segmented)
}

#################################################################################################################

## Función que segmenta las zonas de moderada vegetación del área de interés 


segmentModVeg<- function (aoi) {
  segmented <- filter_poi(aoi,Classification == LASMEDIUMVEGETATION)
  return (segmented)
}

#################################################################################################################

## Función que segmenta las zonas de  vegetación densa del área de interés 


segmentDenseVeg<- function (aoi) {
  segmented <- filter_poi(aoi,Classification == LASHIGHVEGETATION )
                          #Classification == LASHIGHVEGETATION & Classification == LASGROUND)
  return (segmented)
}


#################################################################################################################

#################################################################################################################


###################################### chmRoofs () ##########################################################

## Función que rasteriza los tejados segmentados del área de interés 

chmRoofs<- function (aoiSegmented) {
  
  # Eliminamos el suelo de la nube de puntos. Normalizamos cada imagen o nube de puntos LIDAR

  lasNormalized <- normHeight(aoiSegmented)
  
  chm <- rasterize_canopy(lasNormalized, res = 1, algorithm = pitfree())
  
  segmentRoofDSM1 <- chm
  segmentRoofDSM1[segmentRoofDSM1<1] <- NA
  
  return (segmentRoofDSM1)

}

#rasterize vegetation
rasterize_ <-  function(lidrImage) {
  #lasNormalized <- normHeightVeg(lidrImage)
  chm <- rasterize_canopy(lidrImage, res = 1, algorithm = p2r())
  return (chm)
}




paintCircle <- function(coordsReaded){
leafletProxy("mymap")  %>%
  addCircles(lat = coordsReaded[2] , lng = coordsReaded[1] ,radius = 1000, color = "orange")
}

loadImages <-  function(){
  #load Lidar catalog from data folder
  dataFiles <- "data/MALAGA/"
  files<-list.files(path=dataFiles, full.names = TRUE)
  print (files)
  ctg <- readLAScatalog(files)
  return (ctg)
}

readCoordinates <- function  (feature) {
  # leemos las coordenadas del polígono trazado
  coords_x <- feature()$geometry$coordinates[[1]]
  coords_y <- feature()$geometry$coordinates[[2]]
  coordsArray <- c(c(coords_x,coords_y))
  return (coordsArray)
}

#Creamos un df con las coordenadas (coords) del punto en UTM
createCoordsDF <- function(coordsReaded) {
  
  df <- data.frame(coordsReaded[2],coordsReaded[1])
  colnames(df) <- c("long_coord","lat_coord")
  
  # Creamos un SpatialPoints object con las coordenadas del polígono en (LAt,LNG)
  cord.dec = SpatialPoints(cbind(df$lat_coord, df$long_coord), proj4string=CRS("+proj=longlat"))
  
  # Convertimos las coordenadas del punto (lat,lng) a UTM que es como tenemos el catalogo y las imagenes LIDAR 
  cord.UTM <- spTransform(cord.dec, crs(ctg))
  
  return (cord.UTM)
}

clippingAOI <- function(cord.UTM, radius){
  # Cortamos sobre el catalogo LIDR el AOI a 1 km del punto seleccionado
  print (paste("Clipeando terreno a ...",radius))
  clipped <- clip_circle(ctg, xmin(extent(cord.UTM)), ymin(extent(cord.UTM)), radius)
  print ("Clipeado ...")
  return (clipped)
}

# Lidr image to raster DSM
dsmFromLidar <- function (las) {
  nH <- normHeight (las)
  chm <- rasterize_canopy(nH, res = 1, p2r())
  return (chm)
}

normHeight <- function(clipped){
  # Normalizamos las alturas
  print ("Normalizando alturas ...")
  clipped <- normalize_height(clipped,knnidw())
  print ("Normalizadas las alturas ...")
  #plot (clipped)
  return (clipped)
}

######### total area of raster image*************
calculateLidrArea <-  function(clipped,segmented, type) {
  PercentArea <- npoints(segmented) / npoints(clipped) * 100
  print(paste("Points de la imagen Lidar de", type,  "a 1km"))
  print (npoints(segmented))
  print ("Points de la imagen total")
  print (npoints(clipped))
  print(paste("Área (%) de la imagen Lidar de", type,  "a 1km"))
  print (PercentArea)
  print("Área (%) de la imagen total a 1km")
  print (area(clipped))
  return (PercentArea)
}

########### calculate Area ################
calculateArea <- function(geo_raster, clipped, type) {
  totalPixeles <- geo_raster[!is.na(geo_raster)]
  areaRasterM2 <- ncell(totalPixeles) #m2
  areaRasterKM2 <- areaRasterM2/1000000   #km2
  areaTotalLidarKm2 <- area (clipped)/1000000 
  areaTypePercent <- ( areaRasterKM2 / areaTotalLidarKm2) * 100
  print(paste("Área km2 de la imagen raster de", type,  "a 1km del punto"))
  print (areaRasterKM2)
  print("Área  de la imagen total a 1km")
  print (areaTotalLidarKm2)
  print(paste("%",type))
  print (areaTypePercent)
  return (areaTypePercent)
}
###########Start code #################

pinta_capa <- function(capa_a_pintar,name="") {
  
  capa_pts <- rasterToPoints(capa_a_pintar, spatial = TRUE)
  # Then to a 'conventional' dataframe
  capa_df <- data.frame(capa_pts)
  capa_df <- cbind(capa_df, alpha = 1)
  ggplot() +
    geom_raster(data = capa_df ,
                aes(x = x,
                    y = y,
                    fill = layer)) +
    guides(fill = guide_colorbar(title = name)) +
    #scico::scale_fill_scico(palette = "vikO") +
    scale_fill_gradientn(colours = wes_palette("Zissou1", 10, type = "continuous"))+
    theme_void()
}
##########################################################

clipeaNDVI <- function(cord.UTM,NDVICLASSIFY,segmentRoofDSM) {
   print ("cord.UTM")
   print (cord.UTM)
   p <- c(xmin(cord.UTM),ymin(cord.UTM))
   d <- distanceFromPoints(NDVICLASSIFY, p)
   d [d>1000] <- NA
   #plot (d)
   filter_raster <- mask(x = NDVICLASSIFY, mask = d)
   cropeada <- crop(x=filter_raster,y=raster(segmentRoofDSM))
   print ("casca")
   return (cropeada)
}



################################################LOAD LIDAR IMAGES CATALOG####################################################

createRasterDistance <- function (cord.UTM,rasterObj) {
  p <- c(xmin(cord.UTM[1]),ymin(cord.UTM[1]))
  d <- distanceFromPoints(NDVICLASSIFY, p)
 
}


####################################################################################

ctg <- loadImages()

# RF model
rf <- readRDS("rf50Cut.rds")

lst <- readRDS("lst_list.rds")

#dai <- readRDS("dai_list.rds")

lst <-  lst[[1]]
#dai <- dai[[1]]

# getExtents for every lidar image in catalog (para pintar la cuadricula de zonas cubiertas con imágenes LIDAR sobre el mapa Leaflet)
extents <- getExtent (ctg)

#read ndvi classify image of Malaga
NDVICLASSIFY <<- readRDS("NDVI_CLASS.rds")

# Define server logic
shinyServer(function(input, output, session) {
  
  ############ALERTS#############
  # A queue of notification IDs
  ids <- character(0)
  # A counter
  n <- 0
  ###############################

    # Mapa Leaflet
    output$mymap <- renderLeaflet({
      
        leaflet() %>% 
        
          addProviderTiles('Esri.WorldImagery') %>%
            
          # Mostramos una cuadrícula por imagen LIDAR del catálogo sobre el mapa Leaflet
          addRectangles(
            lng1=extents$lng1, lat1=extents$lat1,
            lng2=extents$lng2, lat2=extents$lat2,
            fillColor = "transparent"
          ) %>%
      
          # añadimos la barra con herramientas gráficas (sólo rectángulo por ahora)
          addDrawToolbar(
            targetGroup='draw',
            polylineOptions = FALSE,
            polygonOptions = FALSE,
            circleOptions = FALSE,
            rectangleOptions = FALSE,
            markerOptions = drawMarkerOptions(makeAwesomeIcon(
              icon = "home",
              library = "glyphicon",
              markerColor = "blue",
              iconColor = "white",
              spin = FALSE,
              extraClasses = NULL,
              squareMarker = FALSE,
              iconRotate = 0,
              fontFamily = "monospace",
              text = NULL
            ), zIndexOffset = 2000,repeatMode = FALSE),
            circleMarkerOptions = FALSE, 
            singleFeature = FALSE,
            editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
          ) 
      
    })
  
  
  ################CALCULA % ELEMENTO URBANO##################################
  
  calculatePercentOfUrbanElement <- function (elementRaster,totalRaster){
    areaM2 <- ncell(elementRaster[!is.na(elementRaster)]) / ncell (totalRaster)
    return (areaM2*100)
  }
  
  
  
  #################################
  #numInt 1: 0-250,2_250,750, ...
  #p point
  #r raster layer
  
  segmentaEnIntervalos <- function(r,p,numInt) {
    
    p <- c(xmin(p),ymin(p))
    
    px <- p[1]
    py <- p[2]
    p <- c(px,py)
    
    d <- distanceFromPoints(raster(r),p)
    
    if (numInt==1)
      d [d>250] <- NA
    else if (numInt==2)
      d [d<250 | d>=500] <- NA
    else if (numInt==3)
      d [d<500 | d>=750] <- NA
    else
      d [d<750 |d>=1000]
    #Recortamos el trozo de imagen raster en el intervalo
    dist_raster_dv <- mask(x = raster(r), mask = d)

    m2EnIntervalo <- ncell (dist_raster_dv[!is.na(dist_raster_dv)])
    return (m2EnIntervalo)
  }
  
  
  segmentaEnIntervalosW <- function(r,p,numInt) {
    
    p <- c(xmin(p),ymin(p))
    
    px <- p[1]
    py <- p[2]
    p <- c(px,py)
    
    d <- distanceFromPoints(raster(r),p)
    
    if (numInt==1)
      d [d>250] <- NA
    else if (numInt==2)
      d [d<250 | d>=500] <- NA
    else if (numInt==3)
      d [d<500 | d>=750] <- NA
    else
      d [d<750 |d>=1000]
    #Recortamos el trozo de imagen raster en el intervalo
    dist_raster_dv <- mask(x = r, mask = d)
    
    m2EnIntervalo <- ncell (dist_raster_dv[!is.na(dist_raster_dv)])*30*30
    
    return (m2EnIntervalo)
  }
  
  ###################################
  calculatePercentOfUrbanElement2 <- function (elementRaster,totalAreaKm2){
    areaKM2 <- ncell(elementRaster[!is.na(elementRaster)]) / 1000000 
    return ( (areaKM2/totalAreaKm2) * 100 )
  }
  
  '%!in%' <- Negate('%in%')
  
  areaRasterImg <- function(elementRaster){
    
  }
  
 
  # Simulete new LST after change vegetation % on the new scenario
  observeEvent(input$simulateNewScenario, {
    # updated %veg 250 values
    print ("Nuevo valor de vegetación")
    print (input$rngDV)
    dfForPrediction$percent_dense_veg_less_250 <<-  input$rngDV
    dfForPrediction$percent_mod_veg_less_250 <<-  input$rngMV
    dfForPrediction$percent_dense_veg_less_250 <<- input$rngLV
    predictedLST2 <- predict (rf,dfForPrediction)
    print ("predicterLST2")
    print (predictedLST2)
    print ("dfForPrediction DV")
    print (dfForPrediction$percent_dense_veg_less_250)
    output$predictedLST2 <- renderUI({
      paste0(predictedLST2, " ºC")
    })
    output$benefits <- renderUI({
      paste0(predictedLST-predictedLST2, " ºC")
    })
  } )
  
  ########################################
    
    # Cuando pulsemos estimar LST (boton)
    observeEvent(input$estimateLST, {
      
      #Leemos las coordenadas del punto
      coordsReaded <- readCoordinates(feature)
      
      print ("coord readed")
      print (coordsReaded[1])
      print (coordsReaded[2])
      
      #Pintamos el circulo al radio de 1 km
      paintCircle(coordsReaded)
      print ("circle painted")
      
      #Obtenemos las coordenadas UTM del punto
      cord.UTM <- createCoordsDF(coordsReaded)
      

      #Cortamos el catalogo Lidar a 1 km a la redonda del punto
      print ("Getting Lidar images from catalog (which cover 1km. radius)")
      id <- showNotification(paste("Processing", "Cut 1km. circle over Lidar catalog"), duration = NULL)
      ids <<- c(ids, id)
      n <<- n + 1
      clipped <- clippingAOI(cord.UTM,1000)
      removeNotification(ids[1])
      ids <<- ids[-1]
      #clipped <- normalize_height(clipped, tin())
      print("Área de la imagen Lidar a 1km")
      print (st_area(clipped))
      
      # Area total procesada (1km del punto)
      areaKM2 <- area (clipped) / 1000000
      
      
      #Clipeamos la imagen Raster del NDVI
      
      NDVICLASSIFY <- readRDS ('clases.rds')
      
      
   
      #normLidar <- normHeight(clipped)
      
      ##############ROOF PROCESSING############################
      print ("Building processing")
      id <- showNotification(paste("Processing", "Building"), duration = NULL)
      ids <<- c(ids, id)
      n <<- n + 1
      #Filtramos los edificios 3D
      segmentRoof <- segmentRoofs (clipped)
      #Segmentamos los tejados
      segmentRoofDSM <- chmRoofs(segmentRoof)
      #percentBuildingOfArea <- calculateArea(segmentRoofDSM,clipped,"Building")
      output$graficPlot <- renderPlot({
        plot (segmentRoofDSM, main = "Buildings and their heights (normalized)", col = rev(rainbow(8)))
        
      })
      
      
      
      # Filtering building by height
      aux1 <- segmentRoofDSM
      aux2 <- segmentRoofDSM
      aux3 <- segmentRoofDSM
      aux1[aux1>=12] <- NA 
      aux2[aux2<12 | aux2>=24] <- NA 
      aux3[aux3<=24] <- NA 
      build_more_24 <- aux3
      build_12_24 <- aux2
      build_less_12 <- aux1 
      
      areaKM2 <- area (clipped) /1000000
      
      #% of building
      perBuild_less12 <- calculatePercentOfUrbanElement2 (build_less_12,areaKM2)
      
      perBuild_12_24 <- calculatePercentOfUrbanElement2 (build_12_24,areaKM2)
      
      perBuild_more24 <- calculatePercentOfUrbanElement2 (build_more_24,areaKM2)
      
      #plot raster of buildings
      
      output$graficPlot666 <- renderPlot({
        plot (build_less_12, main = "Buildings <=12", col = rev(rainbow(8)))
        
      })
      
      output$graficPlot667 <- renderPlot({
        plot (build_12_24, main = "Buildings >12 & <=24 ", col = rev(rainbow(8)))
        
      })
      
      output$graficPlot668 <- renderPlot({
        plot (build_more_24, main = "Buildings >24", col = rev(rainbow(8)))
        
      })
      
    
      
      
      #Remove pop-ups building processing
      removeNotification(ids[1])
      ids <<- ids[-1]
      print ("Tejados procesados")
      ###################################################
   
      #################WATER###########################
      print ("Water processing")
      id <- showNotification(paste("Processing", "Water"), duration = NULL)
      ids <<- c(ids, id)
      n <<- n + 1
      
      # Hacer cropping y mostrar
      waterSegmented <- clipeaNDVI (cord.UTM,NDVICLASSIFY[[1]], raster (segmentRoofDSM))
      
      auxRaster <- waterSegmented
      
      auxRaster [auxRaster!=1] <- NA
      
      waterSegmentedFinal <- auxRaster
      
      # output$graficPlot14 <- renderPlot({
      #   plot (NDVICLASSIFY, main = "NDVI from Sentinel image of city")
      #   
      # })
      
      output$graficPlot15 <- renderPlot({
        plot (waterSegmentedFinal, main = "Water", col = "lightblue", legend = FALSE)
        
      })
      
      waterAreaPercent <- calculatePercentOfUrbanElement(waterSegmentedFinal,waterSegmented)
      
      removeNotification(ids[1])
      ids <<- ids[-1]
      print ("Water procesada")
      ################################################
      
      #Raster image full Lidar model
      
     
      #fullImage <- rasterize_canopy()
      
      ################DENSE VEG. PROCESSING##########
      id <- showNotification(paste("Processing", "Dense veg."), duration = NULL)
      ids <<- c(ids, id)
      n <<- n + 1
      segmentDV <- segmentDenseVeg (clipped)
      segmentDVDSM <- rasterize_(segmentDV)
      #percentDVOfArea  <- calculateArea(segmentDVDSM,clipped,"Dense vegetation")
      output$graficPlot13 <- renderPlot({
        plot (segmentDVDSM, main = "Dense veg.",
              col=c("darkgreen"), legend=FALSE )
      })
      
      DVAreaPercent <- calculatePercentOfUrbanElement2(segmentDVDSM,areaKM2)
      
      removeNotification(ids[1])
      ids <<- ids[-1]
      print ("Veg.densa procesada")
      ####################LOW V##########################

      print ("LV processing")
      id <- showNotification(paste("Processing", "Low veg."), duration = NULL)
      ids <<- c(ids, id)
      n <<- n + 1
      segmentLV <- segmentLowVeg (clipped)
      #percentLVOfArea <- calculateLidrArea (clipped, segmentLV, "Low veg." )
      segmentDSMLV <- rasterize_(segmentLV)
      #percentLVOfArea  <- calculateArea(segmentDSMLV,clipped,"Low vegetation")
      output$graficPlot235 <- renderPlot({
        plot (segmentDSMLV, main = "Low vegetation",
              col=c("lightgreen"), legend=FALSE )
      })
      
      LVAreaPercent <- calculatePercentOfUrbanElement2(segmentDSMLV,areaKM2)
      
      removeNotification(ids[1])
      ids <<- ids[-1]
      print ("LW procesada")

      ####################Moderata VEG##########################
      print ("Mod veg. processing")
      id <- showNotification(paste("Processing", "Moderate veg."), duration = NULL)
      ids <<- c(ids, id)
      n <<- n + 1
      segmentMV <- segmentModVeg (clipped)
      #percentMVOfArea <- calculateLidrArea (clipped, segmentMV, "Mod veg." )
      segmentDSMMV <- rasterize_(segmentMV)
      #percentMVOfArea  <- calculateArea(segmentDSMMV,clipped,"Moderate vegetation")
      output$graficPlot234 <- renderPlot({
        plot (segmentDSMMV, main = "Moderate vegetation",
              col=c("green"), legend=FALSE )
      })
      
      MVAreaPercent <- calculatePercentOfUrbanElement2(segmentDSMMV,areaKM2)
      
      
      removeNotification(ids[1])
      ids <<- ids[-1]
      print ("MV procesada")
      ################################################
      
      
      
      other <- 100 - (perBuild_less12+perBuild_12_24+perBuild_more24+DVAreaPercent+MVAreaPercent+LVAreaPercent+
                        waterAreaPercent)
      id <- showNotification(paste("Creating table", "features % 1km. radius"), duration = NULL)
      ids <<- c(ids, id)
      n <<- n + 1
      dfPercents <- data.frame (perBuild_less12,perBuild_12_24, perBuild_more24,
                                DVAreaPercent,MVAreaPercent,LVAreaPercent,
                                waterAreaPercent, other)
      colnames (dfPercents) <- c("%Build<12","%Build12_24","%Build>24","%DV","%MV","%LV","%Water", "%Other")
      output$table_total_percents <- renderTable(dfPercents)
      removeNotification(ids[1])
      ids <<- ids[-1]
      
      
      #################################################
      
      #Intervalos
      
      print ("Interval processing [0-250,250-500,500-750,750-1000]")
      id <- showNotification(paste("Processing", "Intervals"), duration = NULL)
      ids <<- c(ids, id)
      n <<- n + 1
      
      #0-250
      
      #Build
      m2BuildLess12_0_250 <- segmentaEnIntervalos (build_less_12,cord.UTM,1)
      m2Build12_24_0_250 <- segmentaEnIntervalos (build_12_24,cord.UTM,1)
      m2BuildMore24_0_250 <- segmentaEnIntervalos (build_more_24,cord.UTM,1)
      
      #Veg
      m2DVLess12_0_250 <- segmentaEnIntervalos (segmentDVDSM,cord.UTM,1)
      m2MV_0_250 <- segmentaEnIntervalos (segmentDSMMV,cord.UTM,1)
      m2LV_12_0_250 <- segmentaEnIntervalos (segmentDSMLV,cord.UTM,1)
      
      #Water
      
      m2_Water_0_250 <- segmentaEnIntervalosW (waterSegmentedFinal,cord.UTM,1)
      
      #Other
      other <- filter_poi (clipped, Classification %!in% c(LASHIGHVEGETATION, LASMEDIUMVEGETATION, LASLOWVEGETATION) )
      lasNormalized <- normHeight(other)
      chmOther <- rasterize_canopy(lasNormalized, res = 1, algorithm = pitfree())
      m2_other_0_250 <- segmentaEnIntervalos (chmOther,cord.UTM,1)
      
      totalAreaM2 = m2_Water_0_250+m2_other_0_250+m2LV_12_0_250+m2MV_0_250+m2DVLess12_0_250+
        m2BuildLess12_0_250+m2Build12_24_0_250+m2BuildMore24_0_250
      
      areaBuildLess12_0_250 <- (m2BuildLess12_0_250/totalAreaM2)*100
      areaBuild_12_24_0_250 <- (m2Build12_24_0_250/totalAreaM2)*100
      areaBuildMore24_0_250 <- (m2BuildMore24_0_250/totalAreaM2)*100
      areaDV_0_250 <- (m2DVLess12_0_250/totalAreaM2)*100
      areaMV_0_250 <- (m2MV_0_250/totalAreaM2)*100
      areaLV_0_250 <- (m2LV_12_0_250/totalAreaM2)*100
      areaW_0_250 <- (m2_Water_0_250/totalAreaM2)*100
      areaOther_0_250 <- (m2_other_0_250/totalAreaM2)*100
      
      dfPercents_0_250 <- data.frame (areaBuildLess12_0_250,areaBuild_12_24_0_250,
                                      areaBuildMore24_0_250,
                                      areaDV_0_250,areaMV_0_250,
                                      areaLV_0_250, areaW_0_250,
                                      areaOther_0_250)
      colnames (dfPercents_0_250) <- c("%Build<12","%Build12_24","%Build>24","%DV","%MV","%LV","%Water", "%Other")
      output$table_total_percents_0_250 <- renderTable(dfPercents_0_250)
      
      ######################
      #250-500
      
      
      #Build
      m2BuildLess12_250_500 <- segmentaEnIntervalos (build_less_12,cord.UTM,2)
      m2Build12_24_250_500 <- segmentaEnIntervalos (build_12_24,cord.UTM,2)
      m2BuildMore24_250_500 <- segmentaEnIntervalos (build_more_24,cord.UTM,2)
      
      #Veg
      m2DVLess12_250_500 <- segmentaEnIntervalos (segmentDVDSM,cord.UTM,2)
      m2MV_250_500 <- segmentaEnIntervalos (segmentDSMMV,cord.UTM,2)
      m2LV_250_500 <- segmentaEnIntervalos (segmentDSMLV,cord.UTM,2)
      
      #Water
      m2_Water_250_500 <- segmentaEnIntervalosW (waterSegmentedFinal,cord.UTM,2)
      
      #Other
      m2_other_250_500 <- segmentaEnIntervalos (chmOther,cord.UTM,2)
      
      totalAreaM2 = m2_Water_250_500+m2_other_250_500+m2LV_250_500+m2MV_250_500+m2DVLess12_250_500+
        m2BuildLess12_250_500+m2Build12_24_250_500+m2BuildMore24_250_500
      
  
      areaBuildLess12_250_500 <- (m2BuildLess12_250_500/totalAreaM2)*100
      areaBuild_12_24_250_500 <- (m2Build12_24_250_500/totalAreaM2)*100
      areaBuildMore24_250_500 <- (m2BuildMore24_250_500/totalAreaM2)*100
      areaDV_250_500 <- (m2DVLess12_250_500/totalAreaM2)*100
      areaMV_250_500 <- (m2MV_250_500/totalAreaM2)*100
      areaLV_250_500 <- (m2LV_250_500/totalAreaM2)*100
      areaW_250_500 <- (m2_Water_250_500/totalAreaM2)*100
      areaOther_250_500 <- (m2_other_250_500/totalAreaM2)*100
      
      dfPercents_250_500<- data.frame (areaBuildLess12_250_500,areaBuild_12_24_250_500,
                                      areaBuildMore24_250_500,
                                      areaDV_250_500,areaMV_250_500,
                                      areaLV_250_500, areaW_250_500,
                                      areaOther_250_500)
      colnames (dfPercents_250_500) <- c("%Build<12","%Build12_24","%Build>24","%DV","%MV","%LV","%Water", "%Other")
      output$table_total_percents_250_500 <- renderTable(dfPercents_250_500)
      
      
      
      ######################
      #500_750
      
      
      #Build
      m2BuildLess12_500_750 <- segmentaEnIntervalos (build_less_12,cord.UTM,3)
      m2Build12_24_500_750 <- segmentaEnIntervalos (build_12_24,cord.UTM,3)
      m2BuildMore24_500_750 <- segmentaEnIntervalos (build_more_24,cord.UTM,3)
      
      #Veg
      m2DVLess12_500_750 <- segmentaEnIntervalos (segmentDVDSM,cord.UTM,3)
      m2MV_500_750 <- segmentaEnIntervalos (segmentDSMMV,cord.UTM,3)
      m2LV_500_750 <- segmentaEnIntervalos (segmentDSMLV,cord.UTM,3)
      
      #Water
      m2_Water_500_750 <- segmentaEnIntervalosW (waterSegmentedFinal,cord.UTM,3)
      
      #Other
      m2_other_500_750 <- segmentaEnIntervalos (chmOther,cord.UTM,3)
      
      totalAreaM2 = m2_Water_500_750+m2_other_500_750+m2LV_500_750+m2MV_500_750+m2DVLess12_500_750+
        m2BuildLess12_500_750+m2Build12_24_500_750+m2BuildMore24_500_750
      
      
      areaBuildLess12_500_750 <- (m2BuildLess12_500_750/totalAreaM2)*100
      areaBuild_12_24_500_750 <- (m2Build12_24_500_750/totalAreaM2)*100
      areaBuildMore24_500_750 <- (m2BuildMore24_500_750/totalAreaM2)*100
      areaDV_500_750 <- (m2DVLess12_500_750/totalAreaM2)*100
      areaMV_500_750 <- (m2MV_500_750/totalAreaM2)*100
      areaLV_500_750 <- (m2LV_500_750/totalAreaM2)*100
      areaW_500_750 <- (m2_Water_500_750/totalAreaM2)*100
      areaOther_500_750 <- (m2_other_500_750/totalAreaM2)*100
      
      dfPercents_500_750<- data.frame (areaBuildLess12_500_750,areaBuild_12_24_500_750,
                                       areaBuildMore24_500_750,
                                       areaDV_500_750,areaMV_500_750,
                                       areaLV_500_750, areaW_500_750,
                                       areaOther_500_750)
      colnames (dfPercents_500_750) <- c("%Build<12","%Build12_24","%Build>24","%DV","%MV","%LV","%Water", "%Other")
      output$table_total_percents_500_750 <- renderTable(dfPercents_500_750)
      
      
      
      ######################
      #750_1000
      
      
      #Build
      m2BuildLess12_750_1000<- segmentaEnIntervalos (build_less_12,cord.UTM,4)
      m2Build12_24_750_1000<- segmentaEnIntervalos (build_12_24,cord.UTM,4)
      m2BuildMore24_750_1000 <- segmentaEnIntervalos (build_more_24,cord.UTM,4)
      
      #Veg
      m2DVLess12_750_1000 <- segmentaEnIntervalos (segmentDVDSM,cord.UTM,3)
      m2MV_750_1000 <- segmentaEnIntervalos (segmentDSMMV,cord.UTM,4)
      m2LV_750_1000<- segmentaEnIntervalos (segmentDSMLV,cord.UTM,4)
      
      #Water
      m2_Water_750_1000 <- segmentaEnIntervalosW (waterSegmentedFinal,cord.UTM,3)
      
      #Other
      m2_other_750_1000 <- segmentaEnIntervalos (chmOther,cord.UTM,4)
      
      totalAreaM2 = m2_Water_750_1000+m2_other_750_1000+m2LV_750_1000+m2MV_750_1000+m2DVLess12_750_1000+
        m2BuildLess12_750_1000+m2Build12_24_750_1000+m2BuildMore24_750_1000
      
      areaBuildLess12_750_1000 <- (m2BuildLess12_750_1000 /totalAreaM2)*100
      areaBuild_12_24_750_1000 <- (m2Build12_24_750_1000 /totalAreaM2)*100
      areaBuildMore24_750_1000 <- (m2BuildMore24_750_1000 /totalAreaM2)*100
      areaDV_750_1000 <- (m2DVLess12_750_1000 /totalAreaM2)*100
      areaMV_750_1000 <- (m2MV_750_1000 /totalAreaM2)*100
      areaLV_750_1000 <- (m2LV_750_1000 /totalAreaM2)*100
      areaW_750_1000 <- (m2_Water_750_1000 /totalAreaM2)*100
      areaOther_750_1000 <- (m2_other_750_1000 /totalAreaM2)*100
      
      dfPercents_750_1000 <- data.frame (areaBuildLess12_750_1000 ,areaBuild_12_24_750_1000 ,
                                        areaBuildMore24_750_1000 ,
                                        areaDV_750_1000 ,areaMV_750_1000 ,
                                        areaLV_750_1000 , areaW_750_1000 ,
                                        areaOther_750_1000 )
      colnames (dfPercents_750_1000 ) <- c("%Build<12","%Build12_24","%Build>24","%DV","%MV","%LV","%Water", "%Other")
      output$table_total_percents_750_1000  <- renderTable(dfPercents_750_1000 )
      
      
      removeNotification(ids[1])
      ids <<- ids[-1]
      
      
      # create DF for prediction
      
      dfForPrediction <<- data.frame (areaBuildMore24_750_1000,
                                    areaOther_0_250,
                                    areaBuild_12_24_0_250,
                                    areaDV_0_250,
                                    areaBuildLess12_0_250,
                                    areaMV_0_250,
                                    areaLV_0_250,
                                    areaLV_750_1000,
                                    areaLV_500_750,
                                    areaDV_250_500)
      
      names (dfForPrediction) <<- names (rf$trainingData) [2:11]
      
      predictedLST <<- predict (rf,dfForPrediction)
      
      realLST <- lst [cord.UTM]
      
      print (predictedLST)
      
      output$predictedLST <- renderUI({
        paste0(predictedLST, " ºC")
      })
      
      lstCliped <- clipeaNDVI(cord.UTM,lst,segmentRoofDSM)
      output$graficPlot369 <- renderPlot({
        plot (lstCliped,col=rev(heat.colors(5)))
        plot(cord.UTM, add = TRUE, pch = 16)
        text(cord.UTM, labels = c(round(realLST,2)), pos = 4, offset = 0.7)
      })
      
      
      
      #simulations
      
      output$text <- renderText('Dense veg. % [0-250)m')
      output$text1 <- renderText('Moderate veg. % [0-250)m')
      output$text2 <- renderText('Low veg. % [0-250)m')
      
      output$value <- renderText(areaDV_0_250)
      output$value1 <- renderText(areaMV_0_250)
      output$value2 <- renderText(areaLV_0_250)
      
      updateSliderInput(session, "rngDV", value = areaDV_0_250)
      updateSliderInput(session, "rngMV", value = areaMV_0_250)
      updateSliderInput(session, "rngLV", value = areaLV_0_250)
      

      # Desactivar para el consurso. Activar como app normal
      #shinyjs::show("showSlopes")
      #shinyjs::show("slopeMulti")
      
 
      #Activar para el concurso. Desactivar para app normal
      #shinyjs::show("slopeMulti2")
      #shinyjs::show("showSlopes2")
  

    })
    

    # Observamos los eventos de selección gráfica (trazar polígono, ..) de forma reactiva en la variable función feature
    feature <- eventReactive( input$mymap_draw_new_feature, {
      input$mymap_draw_new_feature
    })
    
    #Salida en el panel lateral del área disponible
    output$distPlot <- renderPlot({
        
      plot(ctg)
  
    })
    

})
