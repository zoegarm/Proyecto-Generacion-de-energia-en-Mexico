##   Curso Data Science Santander-BEDU | Módulo 2: R
##  Equipo 2: Zoé Ariel García Martínez, Gerardo Miguel Pérez Solis, Atenea De La Cruz Brito
## Proyecto: Generación de energía en México: alternativas limpias
#install.packages("rworldxtra")
#Shiny app
#Librerías
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(sf)
library(rworldxtra)

setwd("/CURSO DATA SCIENCE/Proyecto") #Cambiar según usuario
pot <- read.csv("mx_inventory_pot_new.csv")

############################ I. UI  ############################

ui <-   
    fluidPage(
        dashboardPage(
            #Título
            dashboardHeader(title = "Proyecto Equipo 2"),
            #Sidebar
            dashboardSidebar(
                #Menu
                sidebarMenu(
                    
                    menuItem("Presentación", tabName = "intro", icon = icon("file-text")),
                    menuItem("Panorama Actual", tabName = "Panorama_Mex", icon = icon("sun-o")),
                    menuItem("Análisis Exploratorio de Datos", tabName = "aed", icon = icon("area-chart")),
                    menuItem("Datos gen", tabName = "table_g", icon = icon("table")),
                    menuItem("Datos pot", tabName = "table_p", icon = icon("table")),
                    menuItem("Mapa Interactivo", tabName = "map", icon = icon("map"))
                )
            ),
            #Cuerpo
            dashboardBody(
                tabItems(
                    # Presentación
                    tabItem(tabName = "intro",
                            fluidRow(
                                titlePanel("Proyecto. Generación de energía en México: alternativas limpias"),
                                #includeText(paste("include.txt"))
                                br(),
                                pre(includeText("include.txt"))
                            )
                            ),
                            #Análisis Exploratorio de Datos
                            tabItem(tabName = "aed", 
                                    fluidRow(
                                        titlePanel(h3("Histograma")),
                                        img(src= "hist_dni.png", height = 516, width = 430),
                                        titlePanel(h3("Boxplot")),
                                        img(src= "boxplot_dni2.png",width =800),
                                        titlePanel(h3("Scatter plot ranking Banco Mundial")),
                                        img(src= "plat_rank.png", height = 516, width = 430),
                                        titlePanel(h3("Scatter plot plantas instaladas en México")),
                                        img(src= "plot_gen2.png", height = 516, width = 430),
                                        titlePanel(h3("Generación por tipo de planta en México")),
                                        img(src= "generacion_promedio.png"),
                                        titlePanel(h3("Generación por Estado de la República")),
                                        img(src= "generacion_promedio_estado.png"),
                                        titlePanel(h3("Scatter plot potencial de generación en México")),
                                        img(src= "plot_pot.png", height = 516, width = 430),
                                        titlePanel(h3("Potencial por tipo de planta en México")),
                                        img(src= "pot_promedio.png"),
                                        titlePanel(h3("Potencial por Estado de la República")),
                                        img(src= "pot_promedio_estado.png"),
                                        titlePanel(h3("Potencial por del estado de Baja California")),
                                        img(src= "pot_promedio_BC.png")
                                    )
                            ),
                            #Datos de generación
                            tabItem(tabName = "table_g",
                                    fluidRow(        
                                        titlePanel(h3("Tabla de datos de generación de energía")),
                                        dataTableOutput ("data_table_g")
                                    )
                            ), 
                            #datos de potencial
                            tabItem(tabName = "table_p",
                                    fluidRow(        
                                        titlePanel(h3("Tabla de datos de potencial de energía")),
                                        dataTableOutput ("data_table_p")
                                    )
                            ),
                            #Mapa
                            tabItem(tabName = "map",
                                    fluidRow(
                                        titlePanel("Mapa interactivo de potencial de generación de energía en México"),
                                        leafletOutput("data_map")
                                    )
                            ),
                            tabItem( tabName = "Panorama_Mex", 
                                 fluidRow(
                                     titlePanel("Panoráma Nacional de Consumo Eléctrico en México"),
                                     br(),
                                     pre(includeText("Panorama.txt")),
                                     titlePanel(h3("Serie de Tiempo")),
                                     #agrego un select_input:
                                     selectInput("ts", "Seleccione la opción que desee desplegar: ", 
                                                 choices = c(as.character(1:12))
                                    ),
                                    imageOutput("image1")
                                    #img(src = "ConsumoE_1.png")
                                 )
                            )
                            
                    )
                )
            )
        )

########################## II. Server ##########################
#Server
server <- function(input, output) {
    
    #Presentación     
    #Análisis Exploratorio de Datos
    
    #Datos
    #Data Table genreacion
    output$data_table_g <- renderDataTable( {read.csv("mx_inventory_gen_new.csv")}, 
                                          options = list(aLengthMenu = c(5,25,50),
                                                         iDisplayLength = 5)
                                          
    )
    
    #Data Table potencial
    output$data_table_p <- renderDataTable( {pot}, 
                                            options = list(aLengthMenu = c(5,25,50),
                                                           iDisplayLength = 5)
    )
    
    #Mapa Interactivo
    #Datos
    
    #Generar mapa
    output$data_map <- renderLeaflet({
        leaflet() %>% 
            addTiles() %>% 
            addCircles(data = pot, lat = ~lat, lng = ~lon) %>% 
            addMarkers(data = pot, lat = ~lat, lng = ~lon, clusterOptions = markerClusterOptions())

        #Cambiar colores
        Number_pt <- pot$plant_type %>% unique() %>% length()
        Names_pt <- pot$plant_type %>% unique()
        Colores <- c('#1f78b4', '#e41a1c', '#ff7f00', '#4daf4a', '#a6cee3')
        pal <- colorFactor(Colores, domain = Names_pt)
        #Mapa con colores
        m <- leaflet() %>% addTiles() %>% addCircles(data = pot, lat = ~lat, lng = ~lon, 
                                                     color = pal(pot$plant_type), fillOpacity = 1, popup = ~potencial, label = pot$potencial,
                                                     group = "Plantas") %>%
            addMarkers(data = pot, lat = ~lat, lng = ~lon, clusterOptions = markerClusterOptions())
        
        #Leyenda
        m <- m %>% addLegend(data = pot, "topright", pal = pal, 
                             values = ~plant_type, title = "Tipo de planta", opacity = 0.8, group = "Leyenda")
        #Capas
        (m <- m %>% addLayersControl(overlayGroups = c("Plantas", "Leyenda"),
                                     options = layersControlOptions(collapsed = F)))
        })
    
    
    
    outfile <- tempfile(fileext = ".png")
    #Consumo 
    output$image1 <- renderImage({
        if (is.null(input$ts))
            return(NULL)
        
        if (input$ts == "1") {
            return(list(
                src = "www/ConsumoE_1.png",
                contentType = "image/png",
                alt ="1"
            ))
        } 
        else if (input$ts == "2") {
            return(list(
                src = "www/ConsumoE_2.png",
                filetype = "image/png",
                alt = "2"
            ))
            
        }
        else if (input$ts == "3") {
            return(list(
                src = "www/ConsumoE_3.png",
                filetype = "image/png",
                alt = "algo"
            ))
        }
        
        else if (input$ts == "4") {
            return(list(
                src = "www/ConsumoE_4.png",
                filetype = "image/png",
                alt = "algo"
            ))
        }
        
        else if (input$ts == "5") {
            return(list(
                src = "www/ConsumoE_5.png",
                filetype = "image/png",
                alt = "algo"
            ))
        }
        
        else if (input$ts == "6") {
            return(list(
                src = "www/ConsumoE_6.png",
                filetype = "image/png",
                alt = "algo"
            ))
        }
        
        else if (input$ts == "7") {
            return(list(
                src = "www/ConsumoE_7.png",
                filetype = "image/png",
                alt = "algo"
            ))
        }
        
        else if (input$ts == "8") {
            return(list(
                src = "www/ConsumoE_8.png",
                filetype = "image/png",
                alt = "algo"
            ))
        }
        
        else if (input$ts == "9") {
            return(list(
                src = "www/ConsumoE_9.png",
                filetype = "image/png",
                alt = "algo"
            ))
        }
        
        else if (input$ts == "10") {
            return(list(
                src = "www/ConsumoE_10.png",
                filetype = "image/png",
                alt = "algo"
            ))
        }
        
        else if (input$ts == "11") {
            return(list(
                src = "www/ConsumoE_11.png",
                filetype = "image/png",
                alt = "algo"
            ))
        }
        
        else if (input$ts == "12") {
            return(list(
                src = "www/ConsumoTotMex.png",
                filetype = "image/png",
                alt = "algo"
            ))
        }
        
    }, deleteFile = FALSE)
    
}#fin server



###################### III. Run the app ########################
shinyApp(ui = ui, server = server)
