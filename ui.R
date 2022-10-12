#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(shinyWidgets)
library(shinydashboard)

# Define UI for application

#shinyjs::useShinyjs()

shinyUI(dashboardPage(
    
    # Application title
    #titlePanel("Ubicación óptima de placas fotovoltaicas en tejados"),
    
    dashboardHeader(title = "URSUS LST Prediction"),
    
    dashboardSidebar(
        
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("gauge"), badgeLabel = "Main panel", badgeColor = "blue"),
            menuItem("¿What is URSUS LST Prediction?", tabName = "def", icon = icon("question")),
            menuItem("How it work?", tabName = "info", icon = icon("info"))
        )
        
    ),
    
    dashboardBody(
        
        shinyjs::useShinyjs(),
        
        tabItems(
            
            # First tab content
            tabItem(tabName = "dashboard",
        
                        fluidRow( class = "text-center", 
                            
                            box(
                                title = tagList(shiny::icon("globe"), "World Map"),
                                status = "primary", 
                                solidHeader = TRUE,
                                leafletOutput("mymap"),
                                p(),
                                actionButton("estimateLST", "LST Prediction")
                            ),
                            box(
                              title = tagList(shiny::icon("building"), "Building"),
                              status = "primary",
                              solidHeader = TRUE,
                              plotOutput("graficPlot")
                            )
                            
                            
                        ),
                    
                        fluidRow( class = "text-center", 
                          box(
                            title = tagList(shiny::icon("building"), "Building <=12m"),
                            status = "primary",
                            solidHeader = TRUE,
                            width = 4,
                            plotOutput("graficPlot666")
                          ),
                          
                          box(
                            title = tagList(shiny::icon("building"), "Building >12m & <=24m"),
                            status = "primary",
                            solidHeader = TRUE,
                            width = 4,
                            plotOutput("graficPlot667")
                          ) ,
                          
                          box(
                            title = tagList(shiny::icon("building"), "Building >24m"),
                            status = "primary",
                            width = 4,
                            solidHeader = TRUE,
                            plotOutput("graficPlot668")
                          )  
                                  
                        ),
                        
                        fluidRow( class = "text-center",
                                  
                           
                            box(
                              title = tagList(shiny::icon("tree"), "Dense vegetation"),
                              status = "primary",
                              solidHeader = TRUE,
                              plotOutput("graficPlot13")
                            ),
                            box(
                              title = tagList(shiny::icon("tree"), "Moderate veg."),
                              status = "primary",
                              solidHeader = TRUE,
                              plotOutput("graficPlot234")
                            )
                            
                            
                        ),
                    
                        fluidRow( class = "text-center",
                                  
                          box(
                            title = tagList(shiny::icon("tree"), "Low veg."),
                            status = "primary",
                            solidHeader = TRUE,
                            plotOutput("graficPlot235")
                          ),
                          
                          box(
                            title = tagList(shiny::icon("water"), "Water"),
                            status = "primary",
                            solidHeader = TRUE,
                            plotOutput("graficPlot15")
                          )
                          
                        ),
                    
                    
                       
                    
                        fluidRow(
                            
                              titlePanel("Total % of urban element at 1km"),
                          
                              tableOutput('table_total_percents')
                          
                        ),
                    
                        fluidRow(
                                 titlePanel("% of urban element [0-250m)"),
                                 tableOutput('table_total_percents_0_250')
                          
                        ),
                    
                        fluidRow(
                                 titlePanel("% of urban element [250-500m)"),
                                 tableOutput('table_total_percents_250_500')
                          
                        ),
                    
                        fluidRow(
                                 titlePanel("% of urban element [500-750m)"),
                                 tableOutput('table_total_percents_500_750')
                          
                        ),
                        
                        fluidRow(
                                 titlePanel("% of urban element [750-1000m)"),
                                 tableOutput('table_total_percents_750_1000')
                          
                        ),
                    
                        fluidRow( class = "text-center",
                                  titlePanel("LST"),
                                  box(
                                    title = tagList(shiny::icon("temperature-high"), "LST (Real temperature)"),
                                    status = "primary",
                                    solidHeader = TRUE,
                                    plotOutput("graficPlot369")
                                  ),
                                  infoBox(
                                    "LST predicted", uiOutput("predictedLST"), icon = icon("temperature-high"), color = "purple"
                                  )
                                  
                                  
                                  
                        ),
                    
                        fluidRow( class = "text-center",
                                  titlePanel("Simulations. Change Vegetation %"),
                                  
                                  #textInput("caption", "Caption", "Dense veg. 0-250"),
                                  textOutput("text"),
                                  verbatimTextOutput("value"),
                                  
                                  #textInput("caption", "Caption", "Moderate veg. 0-250"),
                                  textOutput("text1"),
                                  verbatimTextOutput("value1"),
                                  
                                  #textInput("caption", "Caption", "Low veg. 0-250"),
                                  textOutput("text2"),
                                  verbatimTextOutput("value2"),
                                  
                                  # Change pertent
                                  sliderInput("rngDV", "Range DV [0-250)m", value = 0, min = 0, max = 100),
                                  sliderInput("rngMV", "Range MV [0-250)m", value = 0, min = 0, max = 100),
                                  sliderInput("rngLV", "Range LV [0-250)m", value = 0, min = 0, max = 100),
                                  
                                  actionButton("simulateNewScenario", "Simulate new scenario"),
                                  
                                  
                              
                        ),
                    
                    fluidRow( class = "text-center",
                              titlePanel("Simulation result on new Scenario"),
                        #LST predicha sin cambiar nada (con los % reales de analizar el entorno)
                       
                        #Lst predicha tras el cambio en las vegetaciones
                        infoBox(
                          "LST of the new scenario", uiOutput("predictedLST2"), icon = icon("temperature-high"), color = "purple"
                        ),
                        infoBox(
                          "temperature improvement", uiOutput("benefits"), icon = icon("temperature-high"), color = "green"
                        )
                    )
                    
                        
                    
         
            ),
            
            # Second tab content
            tabItem(tabName = "info",
                    
                    h2("How it work?"),
                    
                    p( tags$em("INSERT VIDEO TUTORIAL") ),
                    
                   #Insertar video tutorial
                    
            ),
            
            # Second tab content
            tabItem(tabName = "def",
                    h2("¿What is URSUS LST Prediction?"),
                    p(tags$em("URSUS LST Prediction"),"is a data mining based tool that allows you to estimate the LST of any location in any city, analyzing the urban characteristics (%dense vegetation, %moderate vegetation, %buildings in height ranges, ...) within a 1km radius. In addition, it allows simulations to be carried out that allow estimating how the temperatures of said location would drop, increasing the percentage of surrounding vegetation. ")
            ),
            
            # Second tab content
            tabItem(tabName = "predicciones",
                    h2("Predicciones de energía solar media diaria"),
                    fluidRow( class = "text-center",
                              
                              
                              tabBox(
                                  title = tagList(shiny::icon("home"), "Energía solar media diaria"),
                                  
                                  # The id lets us use input$tabset1 on the server to find the current tab
                                  id = "tabset1", height = "250px",
                                  
                                  #app normal / concurso
                                  #tabPanel("Tejados disponibles", actionButton("showArea", "Estimar energía solar media diaria"), actionButton("showArea2", "Estimar energía solar media diaria"), plotOutput("graficPlot6") ),
                                 
                                  tabPanel("Extracción de características", actionButton("showArea", "Estimar energía solar media diaria"), actionButton("showArea2", "Estimar energía solar media diaria"), plotOutput("graficPlot6") ),
                                  
                                  
                                  
                                  #tabPanel("Plot Areas", plotOutput("graficPlot6") ),
                                  #tabPanel("Características y producción de los tejados disponibles",  DT::dataTableOutput("areatable"))
                                  tabPanel("LST predicha",  DT::dataTableOutput("areatable"))
                                  
                              ),
                              
                              
                              
                              infoBoxOutput("totalRoofAreaBox"),
                              infoBoxOutput ("totalEnergyAreaBox")
                              
                    ),
            ),
            
            # Second tab content
            tabItem(tabName = "simulaciones",
                    h2("Predicciones de energía solar a un día vista"),
                    fluidRow( class = "text-center",
                              
                              
                              tabBox(
                                title = tagList(shiny::icon("home"), "Energía solar a un día vista"),
                                
                                # The id lets us use input$tabset1 on the server to find the current tab
                                id = "tabset1", height = "250px",
                                
                                #app normal / concurso
                                tabPanel("Tejados disponibles", actionButton("showArea4", "Estimar energía solar a un día vista"), plotOutput("graficPlot20") ),
                                
                                #tabPanel("Plot Areas", plotOutput("graficPlot6") ),
                                tabPanel("Características y producción de los tejados disponibles",  DT::dataTableOutput("areatable2"))
                                
                              ),
                              
                              
                              
                              infoBoxOutput("totalRoofAreaBox2"),
                              infoBoxOutput ("totalEnergyAreaBox2")
                              
                    ),
            )
            
            
            
            
        
    )
    
  )
    
))
    

    # Sidebar LAyout
    # sidebarLayout(
    #     
    #     #SideBar Panel
    #     sidebarPanel(
    #         plotOutput("graficPlot")
    #     ),
    # 
    #     # Main Panel
    #     mainPanel(
    #         
    #         leafletOutput("mymap"),
    #         
    #         absolutePanel(top = 80, right = 30,
    #                       actionButton("showRoof", "Mostrar tejados"),
    #                       p(),
    #                       actionButton("showArea", "Mostrar áreas"),
    #                       p()
    #                       #actionButton("coords", "Insertar coordenadas")
    #         ),
    #         
    #         p(),
    #         
    #         #plotOutput("graficPlot"),
    #    
    #         p(),
    #         
    #         fluidRow(
    #             
    #             column(8, align="center",
    #                    
    #                    multiInput(
    #                        inputId = "aspectMulti", label = "Seleccione la orientación de los tejados",
    #                        choices = c("Norte", "Sur", "Este",
    #                                    "Oeste")
    #                    ),
    #                    
    #                    actionButton("showOrientation", "Mostrar orientación"),
    #                    
    #                    p(),
    #             )
    #             
    #         ),
    #         
    #         p(),
    #         
    #         
    #         fluidRow(
    #             
    #             column(8, align="center",
    #                    
    #                    multiInput(
    #                        inputId = "slopeMulti", label = "Seleccione el tipo de inclinación de los tejados",
    #                        choices = c("Planos", "Ligeramente inclinados", "Inclinados",
    #                                    "Muy inclinados")
    #                    ),
    #                    
    #                    actionButton("showSlopes", "Mostrar pendientes"),
    #                    
    #                    p(),
    #             )
    #             
    #         ),
    #        
    #         
    #         #verbatimTextOutput(outputId = "aspect"),
    #         
    #        
    #         
    #         #verbatimTextOutput(outputId = "slope"),
    #         p(),
    #         plotOutput("graficPlot2"),
    #         p(),
    #         plotOutput("graficPlot3"),
    #        
    #         
    #     )
    #     
    # )
    

