
library(shiny)

# Define UI for application
shinyUI(fluidPage(

    # Application title
    titlePanel("Predicción para estudiantes de Ingenieria"),

    # Sidebar with a slider input
    sidebarLayout(
        sidebarPanel(
            analisis <- numericInput("analisis", "Ingresar nota de analisis", value = 0, min = 0, max = 10),
            algebra1 <- numericInput("algebra1", "Ingresar nota del primer examen de algebra", value = 0, min = 0, max = 10),  
            algebra2 <- numericInput("algebra2", "Ingresar nota del segundo examen de algebra", value = 0, min = 0, max = 10),  
            prog <- numericInput("prog", "Ingresar nota de Intro a Prog", value = 0, min = 0, max = 10),
            selectInput("model", "Modelo de Regresion:", choices = c("gbm", "randomForest", "nnet", "glm")),
            actionButton("go", "Done", icon("fas fa-magic"))
        ),

        # Show outout
        mainPanel(
            h1(textOutput(outputId = "textR")),
            uiOutput(outputId = "my_ui")
        )
    )
))
