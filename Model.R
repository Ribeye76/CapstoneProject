library(shiny)
library(tm)
library(stringr)
library(markdown)
library(stylo)

#Lectura de datos
bigram <- readRDS(file = "bigram.RData")
trigram <- readRDS(file = "trigram.RData")
quadgram <- readRDS(file = "quadgram.RData")

#Declaro las funciones a usar
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
    else {textInput <- c(NA,NA,textInput)}
}

text <- "trump is a"
textInput <- cleanInput(text)
wordCount <- length(textInput)
wordToPredict <- nextWordPrediction(wordCount,textInput)

wordPrediction <- as.character(quadgram[quadgram$word1==wordToPredict[1] & 
                                quadgram$word2==wordToPredict[2] & 
                                quadgram$word3==wordToPredict[3],][1,]$word4)
      if(is.na(wordPrediction)) {
        wordPrediction <- as.character(trigram[trigram$word1==wordToPredict[2] & 
                                       trigram$word2==wordToPredict[3],][1,]$word3)
          if(is.na(wordPrediction)) {
            wordPrediction <- as.character(bigram[bigram$word1==wordToPredict[3],][1,]$word2)
          }
    }
  
print(wordPrediction)
