########################################
#Course: IST687
#Assignment: HW #10
#Name: Rohini Shrivastava
#Date: 09/15/20
#Notes: Rev 1.0
########################################
install.packages("XML")
library(XML)
install.packages("tm")
library(tm)
#STEP 1

mlk_speech <- readLines(con = "C:/Users/shriv/Downloads/mlk_text.txt")
mlk_speech

afinn <- "C:/Users/shriv/Downloads/afinn.txt"
# scan the file and read the content into "p"
af <- read.delim(afinn, header = TRUE, sep = "\t")
colnames(af)=c("words", "amount")
af

#######################################
#STEP 2

words.vec <- VectorSource(mlk_speech)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
mlk <- TermDocumentMatrix(words.corpus)

mlk

mlk_matrix<- as.matrix(mlk)
count <- rowSums(mlk_matrix)
count

words <- names(count)
total<- sum(count)
af$words
matched<- match(words, af$words, nomatch = 0)
matchtotal<- sum(count[which(matched != 0)])
matchtotal/total
#0.1557
#######################################
#STEP 3
func <- function(quarter){
  
  if (quarter == 1){
    cutpoint1 <- 1
    cutpoint <- round(length(words.corpus)/4)
  }
  if (quarter == 2){
    cutpoint1 <- (round(length(words.corpus)/4))+1
    cutpoint <- 2*(round(length(words.corpus)/4))
  }
  if (quarter == 3){
    cutpoint1 <- 2*((round(length(words.corpus)/4))+1)
    cutpoint <- 3*(round(length(words.corpus)/4))
  }
  if (quarter == 4){
    cutpoint1 <- 3*((round(length(words.corpus)/4))+1)
    cutpoint <- length(words.corpus)
  }
  
  words.corpus1 <- words.corpus[cutpoint1:cutpoint]
  tdm1 <- TermDocumentMatrix(words.corpus1)
  af1 <- as.matrix(tdm1)
  wordCounts1 <- rowSums(af1)
  wordCounts1 <- sort(wordCounts1, decreasing=TRUE)
  totalWords1 <- sum(wordCounts1)
  words1 <- names(wordCounts1)
  matchedP1 <- match(words1, af$words, nomatch = 0)
  ptotalNumber1 <- sum(wordCounts1[which(matchedP1 != 0)])
  ratiop1 <- ptotalNumber1/totalWords1
  
}
q1<-func(1)
q1
q2<-func(2)
q2
q3<-func(3)
q3
q4<-func(4)
q4
#####################################################
#STEP 4

df<- cbind(q1,q2,q3,q4)
barplot(df, names.arg = c("1st Q","2nd Q","3rd Q","4th Q"))