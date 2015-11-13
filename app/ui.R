

library(shiny)

shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "myStyles.css")
  ),    
  # Application title
  titlePanel("Fuzzy Smoothed Text Prediction"),

  # 
  fluidRow(
      column(6,
             div(textInput("inputText", ""), class =  "widen"), 
             div(
               div(actionButton("go", "Predict next word", class = "btn-primary")), 
               wellPanel(textOutput("prediction")), 
               div(actionLink("showDetails", "Show Details")),
               class="center-children"),
             offset = 3)
      
  ),
  fluidRow(
    column(6, 
           conditionalPanel(
             condition = "input.showDetails % 2 == 1",
             p("This text model makes multiple predictions using one to four ",
               "of the last words in the input and compares normalized",
               " scores to find the most likely next word. If a phrase is ",
               "not recognized, the model can skip over (fuzz) words ",
               " to find similar phrases instead.",
               a("View a presentation on RPubs to Learn more", 
                       href="http://rpubs.com/wmurphyrd/fuzzytext")),
             tableOutput("details"),
             div(
              fluidRow(span("Prediction Time:"), textOutput("time", inline=T)),
              fluidRow(span("Model Memory Usage:"), textOutput("size", inline = T)),
              class = "center-children"),
             br(),
           fluidRow(div(
             column(6,
                    strong("Words\u00A0entered into\u00A0this\u00A0app"),
                    plotOutput("inputsCloud", height = "300px")),
             column(6,
                    strong("Words\u00A0predicted by\u00A0this\u00A0app"),
                    plotOutput("outputsCloud", height = "300px"))
           ,class="center-children")),
           fluidRow(
             column(12,
                    div(div(actionButton("refreshClouds", "Update Word Clouds")),
                       a("View source on GitHub", 
                               href="https://github.com/wmurphyrd/FuzzyTextPrediction"),
                        class = "center-children")))
           ),
           offset = 3)
  ),
  
  theme = "bootstrap.min.css"))
