
library(shiny)
library(magick)
load(file = "predictionFunction.rdata")

shinyServer(function(input, output) {

    myval <- eventReactive(input$go,
                           {
                               p <- predictor(input$analisis, input$algebra1, input$algebra2, input$prog, input$model)
                               return(ifelse(p==1, "El alumno dejara la carrera.", "El alumno NO dejara la carrera!"))
                           })
    
    output$textR<-renderText({
        print(myval())
    })
    
    output$my_ui<-renderUI({
        if(myval()=='El alumno dejara la carrera.')
            tags$img(src = 'https://media3.giphy.com/media/fkKORiZTUQvhC/giphy.gif?cid=790b76115d0bc2ad78314c4841731562&rid=giphy.gif')
        else
            tags$img(src = 'http://dl.glitter-graphics.com/pub/1846/1846106q3k4tgfkdq.gif')
    })

})
