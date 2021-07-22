library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(plotly)
library(dplyr)

bn <- read.csv('mappp.csv',header = T)
shp <- readOGR(dsn = getwd(),layer = 'TM_WORLD_BORDERS_SIMPL-0.3')


ui <- dashboardPage(skin = 'green',
  dashboardHeader(title = p(icon('baby'),'FERTILITY RATE 2000-2010'),titleWidth = 350),
  dashboardSidebar(selectInput('sele','Country (bar,line,vb)',choices = unique(bn$Country.Name)),
                   selectInput('sele2','Year (hist,map)',choices = 2000:2010),
                   sliderInput('sli','Bins (hist)',min = 10,max = 70,value = 30)),
  dashboardBody(
    fluidRow(
      column(4,valueBoxOutput('vb',width = 12)),
      column(4,valueBoxOutput('vb2',width = 12)),
      column(4,valueBoxOutput('vb3',width = 12))
    ),
    fluidRow(
      column(7,
             fluidRow(
               column(6,box(width = 300,height = 270,status = 'success',plotlyOutput('plot2',height = 220))),
               column(6,box(width = 300,height = 270,status = 'success',plotlyOutput('plot3',height = 220)))
             ),
             fluidRow(column(12,
                             box(width = 700,height = 210,status = 'success',plotlyOutput('plot1',height = 190))))),
      column(5,
             box(width = 400,height = 500,title = tags$p(icon('globe',class = 'fa-spin'),'Map Showing Rates'),solidHeader = T,status = 'success',leafletOutput('map',height = 415)))
 
    )
  )
  
)

server <- shinyServer(function(input,output){
  
  output$plot1 <- renderPlotly({
    
    df <- filter(bn,Year == input$sele2)
    ggplotly(ggplot(df,aes(Fertility_Rate)) + geom_histogram(bins = input$sli,color = 'white') + geom_freqpoly(bins = input$sli,color = 'blue') + labs(title = paste('Fertility Rates For ',input$sele2))+ theme(panel.background = element_rect(fill = 'peachpuff')) + theme(plot.title = element_text(hjust = 0.5,face = 'bold',color = 'blue',family = 'forte')))
    
    
  })
  
  output$plot2 <- renderPlotly({
    
    df <- filter(bn,Country.Name == input$sele)
    ggplotly(ggplot(df,aes(x = as.character(Year),y = Fertility_Rate)) + geom_col(fill = 'purple',color = 'black') + labs(title = input$sele,x = 'Year') + theme(plot.title = element_text(hjust = 0.5,face = 'bold',color = 'blue',family = 'forte')) + theme(panel.background = element_rect(fill = 'peachpuff')) + theme(axis.text.x = element_text(angle = 90)))
    
  })
  
  output$plot3 <- renderPlotly({
    
    df <- filter(bn,Country.Name == input$sele)
    ggplotly(ggplot(df,aes(x = Year,y = Fertility_Rate)) + geom_line(color = 'blue') + labs(title = paste(input$sele))+ theme(panel.background = element_rect(fill = 'peachpuff')) + theme(plot.title = element_text(hjust = 0.5,face = 'bold',color = 'blue',family = 'forte')))
    
  })
  
  output$vb <- renderValueBox({
    
    df <- filter(bn,Country.Name == input$sele)
    mm <- max(df$Fertility_Rate)
    valueBox(value = mm,subtitle = 'Maximum Rate',color = 'aqua',icon = icon('line-chart'))
    
  })
  
  output$vb2 <- renderValueBox({
    
    df <- filter(bn,Country.Name == input$sele)
    mn <- min(df$Fertility_Rate)
    valueBox(value = mn,subtitle = 'Minimum Rate',color = 'yellow',icon = icon('bar-chart'))
    
  })
  
  output$vb3 <- renderValueBox({
    
    df <- filter(bn,Country.Name == input$sele)
    m <- mean(df$Fertility_Rate)
    valueBox(value = round(m,5),subtitle = 'Average Rate',color = 'teal',icon = icon('line-chart'))
    
  })
  
  
  output$map <- renderLeaflet({
    
    
    dy <- as.data.frame(shp)
    bn <- filter(bn,Country.Name %in% dy$NAME)
    bn <- filter(bn,Year == input$sele2)
    shp2 <- subset(shp,is.element(shp$NAME,bn$Country.Name))
    
    
    bn <- bn[order(match(bn$Country.Name,shp2$NAME)),]
    
    bins = c(0,2,4,6,8)
    
    pal <- colorBin(palette = c('yellow','green','blue','red'),domain = bn$Fertility_Rate,bins = bins)
    
    nn <- leaflet(data = shp2) %>% addTiles(group = 'basic') %>%
      addProviderTiles(providers$Stamen.TonerLite,group = 'tonerl') %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,group = 'natgeo') %>%
      setView(lat= 17.86,lng = 34.493,zoom = 4) %>% addPolygons(color = 'black',fillOpacity = 1,weight = 2,opacity = 0.2,fillColor = pal(bn$Fertility_Rate),highlightOptions = highlightOptions(fillColor = 'black'),label = paste(bn$Country.Name,' : ',bn$Fertility_Rate)) %>%
        addLayersControl(baseGroups = c('basic','tonerl','natgeo')) %>%
      addLegend(position = 'bottomleft',pal = pal,values = bn$Fertility_Rate)
    
    nn
    
  })
  
  
})

shinyApp(ui,server)