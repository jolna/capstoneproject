
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$head(
    tags$style(
      HTML(".shiny-notification {
           height: 100px;
           width: 800px;
           position:fixed;
           top: 300px;
           left: 300px;
           font-size: 25px;
           }
           .shiny-text-output {
           font-size: 20px;
           color: green;
           }
           "
      )
      )
      ),
  # Application title
  titlePanel("Next Word Predictor"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      
      h3("App Settings:"),
      sliderInput("sliderNoOfSugg",
                  "No Of Suggestions:",
                  min = 1,
                  max = 10,
                  value = 5),
      radioButtons("useEst", "Suggest based on:",
                   choiceNames = list(
                     "Backoff Estimation",
                     "Maximum Likelyhood Estimation"
                   ),
                   choiceValues = list(
                     "sbo", "mle"
                   )),
      h5(),
      h3("Diagnostic Settings:"),
      checkboxInput("showDetails", label="Show N-Grams summary", value = FALSE),
      checkboxInput("showTyped", label="Show words entered", value = FALSE),
      checkboxInput("showProgress", label="Show computation progress", value = FALSE),
      h5()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textInput("expression",label="Please type here:",value="Good",width = "500px"),
      textOutput("processingMsg"),
      h3("Suggestions: "),
      textOutput("suggestions"),
      h5(""),
      h5(""),
      h5(""),
      textOutput("actualExpTitle"),
      textOutput("actualExp"),
      h5(""),
      textOutput("detailTitle"),
      tableOutput("detailTable")
      
    )
  )
      ))
