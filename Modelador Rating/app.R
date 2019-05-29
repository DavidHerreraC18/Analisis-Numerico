library(shiny)
library(lubridate)
library(ggplot2)
library(chron)
library(rvest)
library(dplyr)


fx <- function(x) {x^2}
fx2 <- function(x) {sin(x)}
fx3 <- function(x) {sqrt(x)}


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Grafica de Rating"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      helpText("Modelar el rating para un programa de la television Colombiana"),
      #radioButtons(inputId="choice", label="Cadenas televisivas disponibles", 
      #            choices=c("Caracol","RCN","Canal UNO"))
      checkboxGroupInput("programas", "Escoja las programas que desee analizar", c("Noticias Caracol 19:00"="Caracol","La Gloria de Lucho","El Bronx",
                                                                                   "Noticias RCN 19:00"="Noticias RCN","Yo Soy Betty, La Fea",
                                                                                   "CM& Emision Central"= "CM","El Sillon Negro de Guerreros" = "Negro de Guerreros"))
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    if(!is.null(input$programas))
    {
      hacerTodo(input$programas)
    }
  })
}


encontrarRatingPrograma <- function(nombreprograma,fecha)
{
  url<-"https://www.ratingcolombia.com/FECHA.html"
  url2<-gsub("FECHA", fecha, url)
  cat(fecha)
  accesopagina <-html(url2)
  tabla<-accesopagina %>%  html_nodes("i") 
  programa<-nombreprograma
  numlinea<-grep(programa, tabla)
  if(identical(numlinea,integer(0))||numlinea==1){
    return(0);
  }
  linea<-tabla[numlinea[1]]
  boolopcion<-grepl("<br>",linea)
  boolopcion
  if(boolopcion){
    lineamodificada<-gsub("</i>","",linea)
    lineamodificada
    particion<-strsplit(lineamodificada,"<br>")[[1]]
    particion
    particion[[2]]
    bool<-grepl(programa,particion[[1]])
    bool
    if(bool){
      lineafinal<-particion[[1]]
    }else{
      lineafinal<-particion[[2]]
    }
    ptn <- "(.*? )"
    ratingstring<-gsub(ptn, "", lineafinal)
    rating= as.numeric(ratingstring)
    rating
  }else{
    lineamodificada<-gsub("</i>","",linea)
    ptn <- "(.*? )"
    ratingstring<-gsub(ptn, "", lineamodificada)
    rating= as.numeric(ratingstring)
    return(rating)
  }
  
}






#equis= c(10,20,30,40,50,60,70)
#lle= c(2,4,6,8,20,50,80)

hacerTodo <- function(ListaNombre){
  
  fecha = seq(as.Date("2019/04/23"), as.Date("2019/05/24"), "day")
  fecho = fecha
  fecho = format(fecho,"%Y/%m/%d")
  for (fechite in fecho) {
    if (identical(weekdays(as.Date(fechite)),"sábado") || identical(weekdays(as.Date(fechite)),"domingo") || identical(weekdays(as.Date(fechite)),"Sunday") || identical(weekdays(as.Date(fechite)),"Saturday") ) {
      fecho = setdiff(fecho,fechite)
    }
    
  }
  fecha= as.Date(fecho,"%Y/%m/%d")
  fechotas = as.Date(fecho,"%Y/%m/%d")
  fechas=format(fecha,"%Y/%m/%d")
  
  
  reitin=NULL
  names=NULL
  indice=0
  cont = 1
  cat("calculando")
  for (namae in ListaNombre) {
    cont = 1
    for (fechita in fechas) {
      ri=encontrarRatingPrograma(nombreprograma = namae,fechita)
      cat(".")
      if(ri == 0)
      {
        ri = reitin[cont-1]
      }
      reitin <- c(reitin,ri)
      names <- c(names,namae)
      cont = cont + 1 
    }  
  }
  
  
  
  
  
  
  datillos= data.frame(fechotas,reitin,names)
  
  
  plot= ggplot(datillos,aes(x= fechotas, y= reitin, colour = names))+geom_point()
  plot= plot + xlab("Fecha")+ ylab("Millón de personas") +ggtitle("Grafica Rating")
  plot = plot + xlim(as.Date("2019/04/23"), as.Date("2019/06/03"))+ stat_smooth(method="lm",formula = y~poly(x,3),fullrange = TRUE,)
  plot=plot + facet_grid(names ~ .)
  plot
}


# Run the application 
shinyApp(ui = ui, server = server)
