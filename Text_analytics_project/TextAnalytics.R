library(tm)
library(wordcloud)
library(RColorBrewer)
library(readr)
library(readxl)
library(Xplortext)
library(quanteda)
library(lubridate)
library(data.table)
library(readtext)
library(gridExtra)
library(ggplot2)
library(syuzhet)
library(plotly)
library(reshape2)
library(SnowballC)
library(dplyr)
library(quanteda.textmodels)
library(caret)
library(glmnet)

Reviews <- read_csv("C:/Users/PC Palmigiani/Desktop/MDS/Text Analytics and Open Mining/Progetto/Database_Recensioni.csv") #importare dataset QUI
str(Reviews)

##################
# PRE-PROCESSING #
##################

mycorpus<-Corpus(VectorSource(Reviews$Review))
mycorpus<-tm_map(mycorpus, tolower)
mycorpus<-tm_map(mycorpus, removePunctuation)
mycorpus<-tm_map(mycorpus, removeNumbers)
mycorpus<-tm_map(mycorpus, removeWords, stopwords("it"))
tdm<-TermDocumentMatrix(mycorpus)
tdm <- removeSparseTerms(tdm, 0.995)
# transform tdm into a matrix
tdm_reviews <- as.matrix(tdm)
values <- sort(rowSums(tdm_reviews),decreasing=TRUE)
terms <- data.frame(word = names(values),freq=values)

########################
# Quesito 1: WORDCLOUD #
########################

set.seed(1234)
wordcloud(words = terms$word, freq = terms$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

##############################
# Quesito 2: CLASSIFICAZIONE #
##############################

rownames(terms)<-NULL
terms$rank<-c(1:length(terms$word))
##estraiamo per frequenza
top50_words<-terms[1:50,]
top50_words
Reviews$Valutazione[Reviews$Rating == 1] <- "Pessimo"
Reviews$Valutazione[Reviews$Rating == 2] <- "Scarso"
Reviews$Valutazione[Reviews$Rating == 3] <- "Nella Media"
Reviews$Valutazione[Reviews$Rating == 4] <- "Molto Buono"
Reviews$Valutazione[Reviews$Rating == 5] <- "Eccellente"


Reviews$Posizione[Reviews$ID < 200] <- "0-200"
Reviews$Posizione[Reviews$ID >=200 & Reviews$ID <400] <- "200-400"
Reviews$Posizione[Reviews$ID >=400 & Reviews$ID <600] <- "400-600"
Reviews$Posizione[Reviews$ID >=600 & Reviews$ID <800] <- "600-800"
Reviews$Posizione[Reviews$ID >=800] <- ">800"


table(Reviews$Valutazione)
Reviews$Valutazione<-factor(Reviews$Valutazione, levels=c("Eccellente","Molto Buono","Nella Media","Scarso","Pessimo"), ordered=TRUE)
Reviews$Posizione<- factor(Reviews$Posizione, levels=c(">800","600-800","400-600","200-400","0-200"), ordered=TRUE)

str(Reviews)

corpus_df <- data.frame(text =sapply(Reviews$Review, as.character),
                        stringsAsFactors = FALSE)

# Aggiungiamo i metadati
corpus_df$Valutazione <- Reviews$Valutazione
corpus_df$Posizione <- Reviews$Posizione
U_stoplist<-c("m")
library(tm)

# Utilizziamo la variabile Valutazione per aggregare i documenti
# PRIMA TABELLA LESSICALE

res.corpus <- TextData(corpus_df,var.text="text",var.agg = "Valutazione",
                       idiom="it",lower=TRUE, remov.number=TRUE,
                       graph = FALSE, stop.word.tm=TRUE, 
                       Fmin=2000, Dmin=2000)

summary(res.corpus)

# Otteniamo la tabella lessicale (doc_aggregati x termini)
TableLex <-as.matrix(res.corpus$DocTerm)
TableLex <- addmargins(TableLex)

# Calcoliamo alcune statistiche basate sulla metrica X^2
res.chi2 <- chisq.test(TableLex)
tau <- (res.chi2$observed)/res.chi2$expected
tau_df <- as.data.frame(tau)

# Analisi delle corrispondenze lessicali
res.LexCA<-LexCA(res.corpus, graph = FALSE)
summary(res.LexCA,metaWords=TRUE)
plot(res.LexCA,eigen=TRUE)

# Mappe semantiche

plot(res.LexCA,selDoc=NULL, col.word="red",cex=0.5,
     title="Terms representation")

plot(res.LexCA,col.word="red", col.doc="blue",cex=0.5,
     title="Documents and terms representation")


plot(res.LexCA,selWord=top50_words$word,cex=0.5,col.doc="blue",
     title="Documents and top 50 words")

ellipseLexCA(res.LexCA,selWord=NULL,col.doc="grey30",
             title="Confidence ellipses around the documents")

# Otteniamo le coordinate di riga
r <- round(res.LexCA$row$coord, 2)

# Otteniamo le coordinate di colonna
c <- round(res.LexCA$col$coord, 2)

res.LexCA<-LexCA(res.corpus,graph=TRUE,ncp=2) 
HCPC(res.LexCA)


# SECONDA TABELLA LESSICALE

res2.corpus <- TextData(corpus_df,var.text="text",var.agg = "Posizione",
                        idiom="it",lower=TRUE, remov.number=TRUE,
                        graph = FALSE, stop.word.tm=TRUE, 
                        Fmin=4000, Dmin=2000)

summary(res2.corpus)

# Otteniamo la tabella lessicale (doc_aggregati x termini)
TableLex2 <-as.matrix(res2.corpus$DocTerm)
TableLex2 <- addmargins(TableLex2)

# Calcoliamo alcune statistiche basate sulla metrica X^2
res2.chi2 <- chisq.test(TableLex2)
tau2 <- (res2.chi2$observed)/res2.chi2$expected
tau_df2 <- as.data.frame(tau2)

# Analisi delle corrispondenze lessicali
res2.LexCA<-LexCA(res2.corpus, graph = FALSE)
summary(res2.LexCA,metaWords=TRUE)
plot(res2.LexCA,eigen=TRUE)

# Mappe semantiche

plot(res2.LexCA,selDoc=NULL, col.word="red",cex=0.5,
     title="Terms representation")

plot(res2.LexCA,col.word="red", col.doc="blue",cex=0.5,
     title="Documents and terms representation")

plot(res2.LexCA,selWord=top50_words$word,cex=0.5,col.doc="blue",
     title="Documents and top 50 words")

ellipseLexCA(res2.LexCA,selWord=NULL,col.doc="grey30",
             title="Confidence ellipses around the documents")

# Otteniamo le coordinate di riga
r2 <- round(res2.LexCA$row$coord, 2)

# Otteniamo le coordinate di colonna
c2 <- round(res2.LexCA$col$coord, 2)

res2.LexCA<-LexCA(res2.corpus,graph=TRUE,ncp=2) 
HCPC(res2.LexCA)

#################################
# Quesito 3: SENTIMENT ANALYSIS #
#################################

corpus<-corpus(Reviews$Review)
corpus

get_sentiment_dictionary(dictionary = 'nrc', language = "italian")

character<-as.character(corpus)
head(character)

#divido i discorsi nelle varie frasi separate da segni di punteggiatura forti
sentences <- get_sentences(character)
head(sentences)
#calcolo il sentiment delle varie frasi
sentences_sentiment <- get_sentiment(sentences, method = "nrc", language = "italian")
plot(
  sentences_sentiment,
  type="l",
  main="Plot Trajectory",
  xlab = "Time",
  ylab= "Emotional Valence"
)

head(sentences_sentiment)

#calcolo i sentimenti del corpus
corpus_sentiment<-get_nrc_sentiment(corpus, language = "italian")
corpus_sentiment
dim(corpus_sentiment)
corpus_sentiment_df<-data.frame(t(corpus_sentiment))
corpus_sentiment_df
corpus_sentiment_df_new <- data.frame(rowSums(corpus_sentiment_df))
corpus_sentiment_df_new

names(corpus_sentiment_df_new)[1] <- "count"
corpus_sentiment_df_new
corpus_sentiment_df_new <- cbind("sentiment" = rownames(corpus_sentiment_df_new), corpus_sentiment_df_new)
corpus_sentiment_df_new
rownames(corpus_sentiment_df_new) <- NULL
corpus_sentiment_df_new
#grafico dei sentimenti
quickplot(sentiment, data=corpus_sentiment_df_new, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

#calcolo il sentiment dei discorsi
text_sentiment <- get_sentiment(character, method = "nrc", language = "italian")
plot(
  text_sentiment,
  type="l",
  main="Plot Trajectory discorsi",
  xlab = "Time",
  ylab= "Emotional Valence"
)

##########################
# MACHINE LEARNING MODEL #
##########################

#AGGIUNGO LA COLONNA CON IL VALORE DELLA SENTIMENT NEL DATAFRAME DEI DISCORSI
text_sentiment <- data.frame(text_sentiment)
View(text_sentiment)

corpus_final <- character
View(corpus_final)
cor <- data.frame(corpus_final)
dim(cor)
cor$sentiment[rownames(text_sentiment) == rownames(cor)] <- text_sentiment$text_sentiment
View(cor)
dim(cor)

cor2 <- cor
View(cor2)
dim(cor2)

cor3 <- cor
View(cor3)
dim(cor3)

#IN UN NUOVO DATAFRAME AGGIUNGO LA COLONNA SENTIMENTGEN CHE GENERALIZZA IL SENTIMENT IN PARTICOLARE SE ESSO E' >= 0 --> POSITIVO ALTRIMENTI --> NEGATIVO
cor3$sentimentGen[cor3$sentiment >= 0] <- 1
cor3$sentimentGen[cor3$sentiment < 0] <- 0
cor3$sentimentGen <- factor(cor3$sentimentGen, levels=c(0,1), labels=c("Negativo","Positivo"))
#CREO IL CORPUS E INSERISCO IL VALORE DEL SENTIMENT
cor2 <- corpus(cor2$corpus_final)
#creo due matrici dfm
docvars(cor2, "id_numeric") <- 1:ndoc(cor2)
docvars(cor2, "sentiment") <- cor3$sentimentGen
summary(cor2)

#CREO IL DATASET DI TRAI E DI TEST, MA PRIMA MESCOLO IL DATASET IN MODO DA PRENDERE I DUE IN MODO CASUALE
#Uso il 70% dei dati come training e il 30% come test
set.seed(1234)
id_train <- sample(1:118351,82846, replace=F)
head(id_train, 10)

dfmat_train <- corpus_subset(cor2, id_numeric %in% id_train) %>% tokens() %>% dfm()
dfmat_test <- corpus_subset(cor2, !(id_numeric %in% id_train)) %>% tokens %>% dfm()

#COSTRUISCO IL MODELLO
set.seed(245623)
model <- cv.glmnet(x = dfmat_train,
                   y = as.integer(dfmat_train$sentiment == "Positivo"),
                   alpha = 1,
                   family = "binomial",
                   maxit=100000000)
index_best <- which(model$lambda == model$lambda.min)
beta <- model$glmnet.fit$beta[, index_best]

#VALIDO IL MODELLO
head(sort(beta, decreasing = TRUE), 20)
dfmat_matched <- dfm_match(dfmat_test, features =
                             featnames(dfmat_train))
pred <- predict(model, dfmat_matched, type = "response", s =
                  model$lambda.min)
head(pred)
actual_class <- as.integer(dfmat_matched$sentiment == "Positivo")
predicted_class <- as.integer(predict(model, dfmat_matched, type = "class"))
tab_class <- table(actual_class, predicted_class)
tab_class
#Confusion matrix
confusionMatrix(tab_class, mode = "everything")