#load the dataset

email<- read.csv("emlc.csv",stringsAsFactors = FALSE)

head(email)

str(email)

email$Type <- factor(email$Type)

str(email$Type)

table(email$Type)

#Data Preparation- processing text data for analysis

#installing tm packages

install.packages('tm')

library(tm)

#loading required package :NLP

email_corpus <- VCorpus(VectorSource(email$Text))

print(email_corpus)

inspect(email_corpus[1:3])



corpus_clean <- tm_map(email_corpus, PlainTextDocument)

corpus_clean <- tm_map(corpus_clean, removeNumbers)


corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())


corpus_clean <- tm_map(corpus_clean, removePunctuation)


corpus_clean <- tm_map(corpus_clean, stripWhitespace)






inspect(corpus_clean [1:3] )


email_dtm <- DocumentTermMatrix(corpus_clean)

inspect(email_dtm[1:2,100:107])


# Data preparation - creating  training and test datasets


#spilitting the raw data frame

email_train <- email[1:110, ]

email_test  <- email[111:168, ]

# now document term matrix

email_dtm_train <- email_dtm[1:110,]



email_dtm_test  <- email_dtm[111:168,]


#finally the corpus

email_corpus_train <- corpus_clean[1:110]


email_corpus_test <- corpus_clean[111:168]


prop.table(table(email_train$Type))



prop.table(table(email_test$Type))


install.packages("wordcloud")

library(wordcloud)

wordcloud(email_corpus_train, min.freq=40, random.order = FALSE)

Spam <-  subset(email_train,Type == "Spam") 


Ham <-  subset(email_train,Type == "Ham") 

wordcloud(Spam$Text, max.words = 40, scale = c(3,0.5))


wordcloud(Ham$Text, max.words = 40, scale = c(3,0.5))


findFreqTerms(email_dtm_train, 5)

email_tran <- DocumentTermMatrix(email_corpus_train)


email_tst <- DocumentTermMatrix(email_corpus_test)


convert_counts <- function(x)
{
  x<- ifelse(x>0,1,0)
  x<- factor(x, levels = c(0,1))
  return(x)
}


email_tran <- apply(email_tran,MARGIN = 2,convert_counts)


email_tst <- apply(email_tst,MARGIN = 2,convert_counts)


#step -3 Tranning a model on data


install.packages("e1071")

library(e1071)


email_classifier <- naiveBayes(email_tran,email_train$Type)

#step_4 Evaluating Model Performance

email_test_pred <- predict(email_classifier,email_tst)

install.packages("gmodels")

library(gmodels)

CrossTable(email_test_pred, email_test$Type, prop.chisq = FALSE, prop.t = FALSE)

#step-5 Improving model performance

email_classifier2 <- naiveBayes(email_tran,email_train$Type,laplace = 1)

email_test_pred2 <- predict(email_classifier2,email_tst)


CrossTable(email_test_pred2, email_test$Type, prop.chisq = FALSE, prop.t = FALSE, prop.r=FALSE)
















