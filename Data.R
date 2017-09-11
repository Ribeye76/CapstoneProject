#Programa para preparación de data frames

options(java.parameters = "-Xmx16000m")
library(tm)
library(NLP)
library(RWeka)

#extracción de los datos
blogsfile <- file("en_US.blogs.txt", open="rb") # open for reading in binary mode
blogs <- readLines(blogsfile, encoding = "UTF-8", skipNul=TRUE)
close(blogsfile)

newsfile <- file("en_US.news.txt", open = "rb") # open for reading in binary mode
news <- readLines(newsfile, encoding = "UTF-8", skipNul=TRUE)
close(newsfile)

twitterfile <- file("en_US.twitter.txt", open = "rb") # open for reading in binary mode
twitter <- readLines(twitterfile, encoding = "UTF-8", skipNul=TRUE)
close(twitterfile)

#creación de training sets de tamaño 50% de cada archivo
set.seed(2017)
sTwitter <- sample(twitter, size = length(twitter)*0.5, replace = FALSE)
sBlogs <- sample(blogs, size = length(blogs)*0.5, replace = FALSE)
sNews <- sample(news, size = length(news)*0.5, replace = FALSE)

rm(twitter)
rm(blogs)
rm(news)

for (i in 1:100) {
  lowBlogs <- 1+4400*(i-1)
  uppBlogs <- 4400*i
  spBlogs <- sBlogs[lowBlogs:uppBlogs]
  
  lowTwitter <- 1+11800*(i-1)
  uppTwitter <- 11800*i
  spTwitter <- sTwitter[lowTwitter:uppTwitter]
  
  lowNews <- 1+5000*(i-1)
  uppNews <- 5000*i
  spNews <- sNews[lowNews:uppNews]
  
  sampleTotal <- c(spTwitter, spBlogs, spNews)
  
  rm(spTwitter, spBlogs, spNews)
  
  filename <- eval(sprintf("SampleTotal %d.txt", i)[1])
  writeLines(sampleTotal, filename)
  textCon <- file(filename)
  rm(sampleTotal)

  #creación de corpus
  textCorpus <- readLines(textCon)
  close(textCon)
  textCorpus <- VCorpus(VectorSource(textCorpus))
  file.remove(filename)

  # Using the TM Package to clean the text
  textCorpus <- tm_map(textCorpus, content_transformer(function(x) iconv(x, to="UTF-8", sub="byte")))
  textCorpus <- tm_map(textCorpus, content_transformer(tolower)) # converting to lowercase
  textCorpus <- tm_map(textCorpus, content_transformer(removePunctuation)) # removing ponctuation
  profanityWords <- readLines("profanity_words.txt") # Removing Profanity Words
  textCorpus <- tm_map(textCorpus,removeWords, profanityWords)
  textCorpus <- tm_map(textCorpus, content_transformer(removeNumbers)) # removing numbers
  removeURL <- function(x) gsub("http[[:alnum:]]*", "", x) ## removing URLs
  textCorpus <- tm_map(textCorpus, content_transformer(removeURL))
  textCorpus <- tm_map(textCorpus, stripWhitespace) ## Stripping unnecessary whitespace from document
  textCorpus <- tm_map(textCorpus, PlainTextDocument) ## Convert Corpus to plain text document

  # Saving the final corpus
  saveRDS(textCorpus, file = "finalCorpus.RData")
  rm(textCorpus)
  finalCorpusMem <- readRDS("finalCorpus.RData")
  # data framing finalcorpus
  finalCorpus <-data.frame(text=unlist(sapply(finalCorpusMem,`[`, "content")),stringsAsFactors = FALSE)
  rm(finalCorpusMem)

  # Tokenizer function to get unigrams
  bigram <- NGramTokenizer(finalCorpus, Weka_control(min = 2, max = 2))
  trigram <- NGramTokenizer(finalCorpus, Weka_control(min = 3, max = 3))
  quadgram <- NGramTokenizer(finalCorpus, Weka_control(min = 4, max = 4))
  rm(finalCorpus)

  # frequency calculations
  bigram <- data.frame(table(bigram))
  trigram <- data.frame(table(trigram))
  quadgram <- data.frame(table(quadgram))

  #order table of frequencies
  bigram <- bigram[order(bigram$Freq,decreasing = TRUE),]
  trigram <- trigram[order(trigram$Freq,decreasing = TRUE),]
  quadgram <- quadgram[order(quadgram$Freq,decreasing = TRUE),]

  #pre-process and splitting of tables
  names(bigram) <- c("words","freq")
  names(trigram) <- c("words","freq")
  names(quadgram) <- c("words","freq")

  bigram$words <- as.character(bigram$words)
  trigram$words <- as.character(trigram$words)
  quadgram$words <- as.character(quadgram$words)

  fbigram <- eval(sprintf("bigram %d.csv", i)[1])
  ftrigram <- eval(sprintf("trigram %d.csv", i)[1])
  fquadgram <- eval(sprintf("quadgram %d.csv", i)[1])

  write.csv(bigram, fbigram ,row.names=F)
  write.csv(trigram, ftrigram ,row.names=F)
  write.csv(quadgram, fquadgram ,row.names=F)

  rm(bigram, trigram, quadgram)
}

#final del loop "for"
rm(sTwitter, sBlogs, sNews)
file.remove("finalCorpus.RData")

bigram1 <- read.csv("bigram 1.csv")
trigram1 <- read.csv("trigram 1.csv")
quadgram1 <- read.csv("quadgram 1.csv")

cumbigram <- bigram1
cumtrigram <- trigram1
cumquadgram <- quadgram1

rm(bigram1, trigram1, quadgram1)
file.remove("bigram 1.csv", "trigram 1.csv", "quadgram 1.csv")

for (j in 2:100) {
  filename <- eval(sprintf("bigram %d.csv", j)[1])
  bigram <- read.csv(filename)
  cumbigram <- merge(cumbigram, bigram, by="words", all.x=TRUE)
  cumbigram$freq <- cumbigram$freq.x + cumbigram$freq.y
  keep <- c("words", "freq")
  cumbigram <- cumbigram[keep]
  bigram <- bigram[!(bigram$words %in% cumbigram$words),]
  cumbigram <- rbind(cumbigram, bigram)
  cumbigram <- cumbigram[order(cumbigram$freq, decreasing = TRUE),]
  file.remove(filename)
  
  filename <- eval(sprintf("trigram %d.csv", j)[1])
  trigram <- read.csv(filename)
  cumtrigram <- merge(cumtrigram, trigram, by="words", all.x=TRUE)
  cumtrigram$freq <- cumtrigram$freq.x + cumtrigram$freq.y
  keep <- c("words", "freq")
  cumtrigram <- cumtrigram[keep]
  trigram <- trigram[!(trigram$words %in% cumtrigram$words),]
  cumtrigram <- rbind(cumtrigram, trigram)
  cumtrigram <- cumtrigram[order(cumtrigram$freq, decreasing = TRUE),]
  file.remove(filename)
  
  filename <- eval(sprintf("quadgram %d.csv", j)[1])
  quadgram <- read.csv(filename)
  cumquadgram <- merge(cumquadgram, quadgram, by="words", all.x=TRUE)
  cumquadgram$freq <- cumquadgram$freq.x + cumquadgram$freq.y
  keep <- c("words", "freq")
  cumquadgram <- cumquadgram[keep]
  quadgram <- quadgram[!(quadgram$words %in% cumquadgram$words),]
  cumquadgram <- rbind(cumquadgram, quadgram)
  cumquadgram <- cumquadgram[order(cumquadgram$freq, decreasing = TRUE),]
  file.remove(filename)
}

cumbigram <- cumbigram[complete.cases(cumbigram),]
cumtrigram <- cumtrigram[complete.cases(cumtrigram),]
cumquadgram <- cumquadgram[complete.cases(cumquadgram),]

write.csv(cumbigram, "bigram.csv", row.names=FALSE)
write.csv(cumtrigram, "trigram.csv", row.names=FALSE)
write.csv(cumquadgram, "quadgram.csv", row.names=FALSE)

rm(cumbigram, cumtrigram, cumquadgram)
rm(bigram, trigram, quadgram)

bigram <- read.csv("bigram.csv")
trigram <- read.csv("trigram.csv")
quadgram <- read.csv("quadgram.csv")

str2 <- strsplit(as.character(bigram$words), split=" ")
str3 <- strsplit(as.character(trigram$words), split=" ")
str4 <- strsplit(as.character(quadgram$words), split=" ")

bigram <- transform(bigram,
                     one = sapply(str2,"[[",1),
                     two = sapply(str2,"[[",2))
trigram <- transform(trigram,
                     one = sapply(str3,"[[",1),
                     two = sapply(str3,"[[",2),
                     three = sapply(str3,"[[",3))
quadgram <- transform(quadgram,
                     one = sapply(str4,"[[",1),
                     two = sapply(str4,"[[",2),
                     three = sapply(str4,"[[",3),
                     four = sapply(str4,"[[",4))

bigram <- data.frame(word1 = bigram$one,
                     word2 = bigram$two,
                     freq = bigram$freq,
                     stringsAsFactors=FALSE)

trigram <- data.frame(word1 = trigram$one,
                      word2 = trigram$two,
                      word3 = trigram$three,
                      freq = trigram$freq,
                      stringsAsFactors=FALSE)

quadgram <- data.frame(word1 = quadgram$one,
                       word2 = quadgram$two,
                       word3 = quadgram$three,
                       word4 = quadgram$four,
                       freq = quadgram$freq,
                       stringsAsFactors=FALSE)

write.csv(bigram, "bigram.csv", row.names=F)
write.csv(trigram, "trigram.csv", row.names=F)
write.csv(quadgram, "quadgram.csv", row.names=F)

bigram <- read.csv("bigram.csv",stringsAsFactors = F)
trigram <- read.csv("trigram.csv",stringsAsFactors = F)
quadgram <- read.csv("quadgram.csv",stringsAsFactors = F)

saveRDS(bigram,"bigram.RData")
saveRDS(trigram,"trigram.RData")
saveRDS(quadgram,"quadgram.RData")

file.remove("bigram.csv", "trigram.csv", "quadgram.csv")
