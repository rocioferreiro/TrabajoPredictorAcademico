
library(shiny)
library(giphyr)

ui <- fluidPage(
    titlePanel("Predicción"),
    sidebarLayout(
        sidebarPanel(
            analisis <- numericInput("analisis", "Ingresar nota de analisis", value = 0, min = 0, max = 10),
            algebra1 <- numericInput("algebra1", "Ingresar nota del primer examen de algebra", value = 0, min = 0, max = 10),  
            algebra2 <- numericInput("algebra2", "Ingresar nota del segundo examen de algebra", value = 0, min = 0, max = 10),  
            prog <- numericInput("prog", "Ingresar nota de Intro a Prog", value = 0, min = 0, max = 10),
            selectInput("model", "Modelo de Regresion:", choices = c("gbm", "randomForest", "nnet", "glm")),
            actionButton("go", "Done")),
            mainPanel(
                h1(textOutput(outputId = "textR")),
                uiOutput(outputId = "my_ui")
        )
    )
)

server <- function(input, output) {
    
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
            img(src="~/Desktop/TpPredictorAcademico/TrabajoPredictorAcademico/Preduc/sad.gif", height = 300)
        else
            img(src="~/Desktop/TpPredictorAcademico/TrabajoPredictorAcademico/Preduc/happy.gif", height = 300)
    })
}


predictor <- function(analisis, algebra1, algebra2, prog, model, output){
    if(model == "gmb"){
        load(file = "~/Desktop/TpPredictorAcademico/TrabajoPredictorAcademico/model_gbm.rdata")
        finalModel <- model_gbm
    } else if(model == "randomForest"){
        load(file = "~/Desktop/TpPredictorAcademico/TrabajoPredictorAcademico/model_rf.rdata")
        finalModel <- model_rf
    } else if(model == "nnet"){
        load(file = "~/Desktop/TpPredictorAcademico/TrabajoPredictorAcademico/model_nnet.rdata")
        finalModel <- model_nnet
    } else {
        load(file = "~/Desktop/TpPredictorAcademico/TrabajoPredictorAcademico/model_glm.rdata")
        finalModel <- model_glm
    }
    student <- data.frame(analisis,algebra1,algebra2,prog)
    names(student) <- c("Analisis exam 1", "IntroProg exam 1", "Algebra exam 1", "Algebra exam 2")
    predictors <- c("Analisis exam 1","IntroProg exam 1","Algebra exam 1","Algebra exam 2")
    output <- predict(finalModel,student)
}

shinyApp(ui = ui, server = server)
