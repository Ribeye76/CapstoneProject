library(shiny)
library(tm)
library(stringr)
library(markdown)
library(stylo)

bigram <- readRDS(file = "bigram.RData")
trigram <- readRDS(file = "trigram.RData")  
quadgram <- readRDS(file = "quadgram.RData")

dataCleaner<-function(text){
  cleanText <- tolower(text)
  cleanText <- removePunctuation(cleanText)
  cleanText <- removeNumbers(cleanText)
  cleanText <- str_replace_all(cleanText, "[^[:alnum:]]", " ")
  cleanText <- stripWhitespace(cleanText)
  return(cleanText)
}

cleanInput <- function(text){
  textInput <- dataCleaner(text)
  textInput <- txt.to.words.ext(textInput, language="English.all", preserve.case = TRUE)
  return(textInput)
}

nextWordPrediction <- function(wordCount,textInput){
  if (wordCount>=3) {textInput <- textInput[(wordCount-2):wordCount]}
  else if(wordCount==2) {textInput <- c(NA,textInput)}
  else {textInput <- c(NA,NA,textInput)
}

wordPrediction <- as.character(quadgram[quadgram$word1 == textInput[1] & 
                                              quadgram$word2 == textInput[2] & 
                                              quadgram$word3 == textInput[3],][1,]$word4)
  if(is.na(wordPrediction)){
    wordPrediction <- as.character(trigram[trigram$word1 == textInput[2] & 
                                                 trigram$word2 == textInput[3],][1,]$word3)
    if(is.na(wordPrediction)){
      wordPrediction <- as.character(bigram[bigram$word1 == textInput[3],][1,]$word2)
    }
  }
print(wordPrediction)
}

shinyServer(function(input, output) {
  wordPrediction <- reactive({
    text <- input$text
    textInput <- cleanInput(text)
    wordCount <- length(textInput)
    wordPrediction <- nextWordPrediction(wordCount,textInput)})
  output$predictedWord <- renderPrint(wordPrediction())
  output$enteredWords <- renderText({input$text}, quoted = FALSE)
})
