library(shiny)

shinyUI(
  navbarPage("Word Prediction",
        tabPanel(
          "Predictor",
          fluidRow(
            column(4, offset = 2,
                   h4("Enter Your Words Here"),
                   textInput(inputId = "userSentence", label = "", value = "")
            ),
            column(3,
                   h4("Limit Word Prediction To"),
                   sliderInput("numPredicted", "", min=1, max=5, value=3) 
            )
          ),
          fluidRow(
            column(4, offset = 2,
                   hr()
            ),
            column(3,
                   hr()
            ) 
          ),
          fluidRow(
            column(4, offset = 2,
               h4("Entered text"),
               verbatimTextOutput("userWords")
            ),
            column(3,
               h4("Next Word Prediction"),
               div(dataTableOutput("predictionTable"))
            )
          )
        ),
      tabPanel(
        "About",
        fluidRow(
          column(4, offset = 2,
            includeMarkdown("about.md")       
          )
        )
      )
  )
)