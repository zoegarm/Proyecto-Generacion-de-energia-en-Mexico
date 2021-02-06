library(shiny)
library(fbRanks)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinythemes)

#fijo el directorio:
setwd("C:/Users/Ariel/Desktop/BEDU/R/archivos de equipo/proyecto")

ui <- fluidPage(
    dashboardPage(
    #Header
        dashboardHeader(title = "Proyecto Equipo 2"),
    
    #Sidebar content
        dashboardSidebar(
            #menu
            sidebarMenu(
                menuItem("Presentación", tabName = "intro", icon = icon("dashboard")),
                menuItem("Panorama Actual", tabName = "Panorama_Mex", icon = icon("sun-o")),
                menuItem("Análisis Exploratorio de Datos", tabName = "aed", icon = icon("area-chart")),
                menuItem("Datos", tabName = "table", icon = icon("table")),
                menuItem("Mapas", tabName = "img", icon = icon("file-picture-o")),
                menuItem("Mapa Interactivo", tabName = "map", icon = icon("file-picture-o"))
                
                        )
                    ),
    #Body content      
        dashboardBody(
            tabItems(
                #Presentación
                tabItem( tabName = "intro", 
                         fluidRow(
                             titlePanel("Proyecto: Generación de Energía en México: alternativas limpias"),
                             br(),
                             pre(includeText("include.txt"))
                             )),
                
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
                             ))
                    )
            
            )
        )
    )    

server <- function(input, output) {

############################### Cargo las imágenes de panorama:        
    outfile <- tempfile(fileext = ".png")
    #hago la mañaaaaaa: 
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
                alt = "3"
            ))
        }
        
        else if (input$ts == "4") {
            return(list(
                src = "www/ConsumoE_4.png",
                filetype = "image/png",
                alt = "4"
            ))
        }
        
        else if (input$ts == "5") {
            return(list(
                src = "www/ConsumoE_5.png",
                filetype = "image/png",
                alt = "5"
            ))
        }
        
        else if (input$ts == "6") {
            return(list(
                src = "www/ConsumoE_6.png",
                filetype = "image/png",
                alt = "6"
            ))
        }
        
        else if (input$ts == "7") {
            return(list(
                src = "www/ConsumoE_7.png",
                filetype = "image/png",
                alt = "7"
            ))
        }
        
        else if (input$ts == "8") {
            return(list(
                src = "www/ConsumoE_8.png",
                filetype = "image/png",
                alt = "8"
            ))
        }
        
        else if (input$ts == "9") {
            return(list(
                src = "www/ConsumoE_9.png",
                filetype = "image/png",
                alt = "9"
            ))
        }
        
        else if (input$ts == "10") {
            return(list(
                src = "www/ConsumoE_10.png",
                filetype = "image/png",
                alt = "10"
            ))
        }
        
        else if (input$ts == "11") {
            return(list(
                src = "www/ConsumoE_11.png",
                filetype = "image/png",
                alt = "11"
            ))
        }
        
        else if (input$ts == "12") {
            return(list(
                src = "www/ConsumoTotMex.png",
                filetype = "image/png",
                alt = "12"
            ))
        }
        
    }, deleteFile = FALSE)
#######################################arriba panorama


    
}


###################################Parte final:
shinyApp(ui = ui, server = server)
