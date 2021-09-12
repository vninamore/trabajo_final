library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(sp)

getwd()



ui <- fluidPage(
  shinythemes::themeSelector(),
  sidebarPanel(

  ),
  mainPanel(
    tabsetPanel(
      tabPanel("PROYECTO FINAL")
    )
  ),
  titlePanel("ANÁLISIS MULTIVARIADO DEL COVID EN AREQUIPA)"), style="color:green",
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Ingresar CSV Aqui",
                multiple = FALSE,
                accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
      ),
      tags$hr(),
      h5(helpText("Seleccione los parametros:")),
      checkboxInput(inputId = 'header', 'Header', T),
      checkboxInput(inputId = "stringAsFactors", "stringAsFactors", T),
      br(),
      radioButtons("sep", "Separador",
                   choices = c(Coma = ",",
                               puntoyComa = ";",
                               Tab = "\t",
                               espacio=''),
                   selected = ","),
      radioButtons("quote", "Cita",
                   choices = c(Ninguna = "",
                               "cita doble" = '"',
                               "cita simple" = "'"),
                   selected = '"'),
      
      
    
        
      
      #radioButtons("disp", "Display",choices = c(Head = "head",All = "all"),selected = "head")
    ),
    
    
    mainPanel(uiOutput("todo"))
  )
)

server <- function(input,output){
  data <- reactive({
    file <- input$file1
    if(is.null(file)){return()} 
    read.table(file=file$datapath,
               sep=input$sep,
               header = input$header,
               stringsAsFactors = input$stringAsFactors)
  })
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    input$file1
  })
  output$sum <- renderTable({
    if(is.null(data())){return ()}
    summary(data())
  })
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  
  
  
 
   #if(is.null(data())){return ()}
  
  #
  
  output$map<- renderLeaflet({
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    mapa<-leaflet(zona_estudio1) %>%addProviderTiles("Stamen.TonerLite",group = "Toner") %>%
      addProviderTiles("HikeBike", group = "Bike") %>% 
      addProviderTiles("Esri", group = "Esri") %>%
      addProviderTiles("Stamen.Watercolor", group="Acuarela") %>%
      addLayersControl(overlayGroups = c("capapuntos"),baseGroups = c("Toner", "Bike", "Esri","Acuarela")) %>%addPolygons()%>% 
      addTiles()
  })
  
  
  output$provincia <- renderPrint({ input$data })
  output$provincia<- renderPlot({
    df<- read.csv(input$file1$datapath,
                    header = input$header,
                    sep = input$sep,
                    quote = input$quote)
    ggplot(data=Data_edit)+
      aes(x=PROVINCIA)+
      geom_density(color=4, fill=4, alpha= 0.25)
  })
  
  output$fabricante <- renderPrint({ input$data })
  output$fabricante<- renderPlot({
    df<- read.csv(input$file1$datapath,
                  header = input$header,
                  sep = input$sep,
                  quote = input$quote)
    ggplot(data=Data_edit)+
      aes(x=FABRICANTE)+
      geom_density(color=2, fill=3, alpha= 0.25)
  })
    
  output$distrito <- renderPrint({ input$data })
  output$distrito<- renderPlot({
    df<- read.csv(input$file1$datapath,
                  header = input$header,
                  sep = input$sep,
                  quote = input$quote)
    ggplot(data=Data_edit)+
      aes(x=DISTRITO)+
      geom_density(color=3, fill=2, alpha= 0.25)
    
  })
  
  output$sexo <- renderPrint({ input$data })
  output$sexo<- renderPlot({
    df<- read.csv(input$file1$datapath,
                  header = input$header,
                  sep = input$sep,
                  quote = input$quote)
    ggplot(data=Data_edit)+
      aes(x=SEXO)+
      geom_density(color=3, fill=4, alpha= 0.25)
    
  })
  output$todo <- renderUI({
    if(is.null(data()))
      h5("desarrollado con", tags$img(src='RStudio-Ball.png', heigth=200, width=200))
    else
      tabsetPanel(tabPanel("inicio", tableOutput("filedf")),
                  tabPanel("Datos", tableOutput("table")),
                  tabPanel("resumen", tableOutput("sum")),
                  tabPanel("mapita con leaflet", leafletOutput("map")),
                  tabPanel("dens provincias", plotOutput("provincia")),
                  tabPanel("dens fabricante", plotOutput("fabricante")),
                  tabPanel("dens distrito", plotOutput("distrito")),
                  tabPanel("dens sexo", plotOutput("sexo"))
      )
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)