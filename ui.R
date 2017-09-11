library(shiny)
library(tm)
library(stringr)
library(markdown)
library(stylo)

shinyUI(fluidPage(
  titlePanel("Coursera Data Science Specialization"),
  h4("Capstone Project"),
  sidebarLayout(
    sidebarPanel(
      h2("Next Word Prediction App"),
      em("This app makes a prediction of the next word
         based on the entered words using a n-grams model"),
      textInput("text", label = h3("Enter your text here:"), value = ),
      submitButton("Submit"),
      h6("Note: The word prediction model only supports phrases / words in english")
    ),
    mainPanel(
      h4("Phrase you entered:"),
      textOutput("enteredWords", h5, strong("bold")),
      h4("Predicted next word:"),
      textOutput("predictedWord", h5, strong("bold"))
    )
  )
)
)
