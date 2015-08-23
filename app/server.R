
library(shiny); library(slam); library(pryr); library(wordcloud)
library(tm); library(RWeka); library(fastmatch); library(dplyr); library(magrittr)
modelFactory <-readRDS("www/multiModelConstructor.RDS")
model <- modelFactory$constructModel()
rm(modelFactory)
modelSize <- object_size(model)

saveWords <- function(inpt, pred) {
  #inpt %<>% strsplit(split = " ") %>% unlist %>% extract(., . != "NA") %>% unique
  inpt %<>% PlainTextDocument %>% removeWords(model$profanity) %>% 
    gsub("\\B[[:punct:]]+\\b|\\b[[:punct:]]+\\B|^[[:punct:]]+|[[:punct:]]+$|\\B[[:punct:]]+\\B",
         "", .) %>% stripWhitespace %>% strsplit(split = " ") %>% unlist
  con <- file("www/inputs.txt", open = "a")
  writeLines(inpt, con)
  close(con)
  con <- file("www/predictions.txt", open = "a")
  writeLines(pred, con)
  close(con)
  
}

file2Cloud <- function(fname) {
  words <- read.table(fname) %>% set_names("w") %>%
    group_by(w) %>% summarise(n = n())
  wordcloud(words$w, words$n, min.freq = 1,
            colors = brewer.pal(8, "Dark2"), random.order = F)
}

shinyServer(function(input, output) {
  result <- reactive({
    input$go
    time <- system.time(pred <- model$predict(isolate(input$inputText)))
    word <- pred$pred[1]
    #store words for wordclouds
    
    if(nrow(pred)) {
      saveWords(isolate(input$inputText), word)
      pred$root <- gsub("NA","____", pred$root)
      pred %<>% rename(`String used for prediction` = root, 
                       `Prediction` = pred,
                        Score = prob)
    }
    list(word = word, details = pred, time = summary(time)[3])
  })
  output$prediction <- renderText({
    validate(need(is.list(result()), ""))
    result()$word})
  output$details <- renderTable({
    validate(need(is.list(result()), ""))
    result()$details})
  output$time <- renderText({
    validate(need(is.list(result()), ""))
    c(round(result()$time, digits = 2), "seconds")})
  output$size <- renderPrint({
    validate(need(is.list(result()), ""))
    modelSize})
  output$inputsCloud <- renderPlot({
    input$refreshClouds
    file2Cloud("www/inputs.txt")
  })
  output$outputsCloud <- renderPlot({
    input$refreshClouds
    file2Cloud("www/predictions.txt")
  })

})
