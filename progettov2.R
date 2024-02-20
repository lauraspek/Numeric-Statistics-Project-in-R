#### 1-2. SELEZIONARE E CARICARE IL DATASET ####
# setto la working directory in modo che possa caricare il dataset 
setwd("~/Desktop/R statistica")

# carico sulla variabile df il dataset originale
df <- read.csv("songs_normalize.csv")

# vediamo una prima panoramica del dataset con queste funzioni
# visualizziamo una prima panoramica del dataset
head(df)
# visualizziamo la dimensione del nostro dataset
dim(df)

# cancello la caratteristiche inutilizzate
df$key=NULL

# creiamo una variabile nel quale inserire una copia del dataset, già ripulito da
# eventuali valori che non ci servono o che sono nulli
df_backup <- df;


#### 3. PRE-PROCESSING #####
# rimuovo i NaN dal dataset
# non cambia niente poichè nella descrizione del dataset viene evidenziato che non vi sono valori nulli
df <- na.omit(df)

# con l’ausilio del comando “summary(df)” si possono verificare diversi valori di ciascuna
# variabile del dataset, tra cui media, mediana, 1° e 3° quantile ma soprattutto il valore
# minimo e massimo che questa assume. 
summary(df)

# trasformo in factor le opportune variabili
df$artist <- factor(df$artist)
df$song <- factor(df$song)
df$explicit <- factor(df$explicit, c("True","False"), c("T","F"))
df$genre <- factor(df$genre)
df$mode <- factor(df$mode, c("1","0"), c("Tonalità Maggiore","Tonalità Minore"))

# trasformo la durata delle canzoni da millisecondi arrotondando a minuti e secondi al fine di avere una leggibilità migliore del dataset
df$duration_ms <- df$duration_ms/1000/60
df$duration <- df$duration_ms
df$duration <- round(df$duration, digits=2)
df$duration_ms <- NULL

# creo un subset da assegnare al database al fine di visualizzare solo le song dal 2000 al 2019 come interessa a noi
df<-subset(df, subset=(df$year>1999&df$year<2020))

# creo un subset per vedere se la dancebility ha solo valori compresi tra 0 e 1 come su kaggle
df<-subset(df, subset=(df$danceability>=0&df$danceability<=1))
# itero il concetto sulle altre variabili
df<-subset(df, subset=(df$energy>=0&df$energy<=1))
df<-subset(df, subset=(df$acousticness>=0&df$acousticness<=1))
df<-subset(df, subset=(df$valence>=0&df$valence<=1))
# la loudness ha un range solitamente che fa da -60 a 0 decibell, quindi considero quella
df<-subset(df, subset=(df$loudness>-61&df$loudness<1))

# abbiamo scelto di ripulire il dataset da tutte le osservazione con la variabile
# popularity=0 perchè essendo un dataset sulle tophits non ci sembra logico che 
# esistano delle valori simili
df<-subset(df, subset=(df$popularity!=0))

# mi copio il df ripulito su df_backup
summary(df);
df_backup=df


#### 4 - SPLITTING ####
N  <- nrow(df)

# stabilisco il numero di dati da mettere in n.train, n.test, n.val
N.train <- 1283
N.test <- 280
N.val <- 269

# creo in train.sample togliendo N.test e N.val
train.sample<-sample(N,N.train)
df.train<-df[train.sample,]
df.test<-df[-train.sample,]

# creo il val.sample prendendo solo N.val
val.sample <- sample(N.test+N.val, N.val)
df.val <- df.test[val.sample,]
df.test <- df.test[-val.sample,]

# copio il train su df in modo da utilizzare solo quello
df <- df.train
N <- nrow(df)


##### 5 - Exploratory Data Analysis (EDA) ####
# installo il pacchetto corrplot e verifico la presenza del pacchetto ggplot2
library(ggplot2)
install.packages ("corrplot")
library (corrplot)

# creo un grafico a torta sul contenuto esplicito
pie(summary(df$explicit), summary(df$explicit),  main="Contenuto Esplicito", col=c("light blue","pink"))
legend(1.0, 1.0, cex = 0.9, legend = c("Explicit", "NoExplicit"), fill = c("light blue","pink"))

# creo un grafico a torta sulla tonalità maggiore o minore
pie(summary(df$mode), summary(df$mode),  main="Tonalità", col=c("red","blue"))
legend(1.0, 1.0, cex = 0.9, legend = c("Maggiore", "Minore"), fill = c("red","blue"))

# creo un istogramma sulle top hits per anno
ggplot(df, aes(x = year)) + geom_histogram(aes(y = ..count..), binwidth = 1,
colour = "goldenrod2", fill = "gold1") + scale_x_continuous(name = "anni",
breaks = seq(2000, 2019, 1),
limits=c(1999, 2020)) + 
scale_y_continuous(name = "Numero di canzoni") + ggtitle("Top hits rilasciate ogni anno")

# creo un plot per visualizzare la durata in sec delle canzoni durante gli anni
plot(df$year,df$duration, xlab = "Years", ylab = "Durata in sec", main="Durata delle canzoni negli anni", xlim=c(2000,2020))

# creo un istogramma sulla popolarità 
ggplot(df, aes(x=popularity)) + geom_histogram(fill = "darkgreen", binwidth = 1) + labs(title="Istogramma sulla popularity")

# istogramma sulla popolarità in base al contenuto esplicito
# ci permette di vedere che il numero di canzoni più popolari sono quelle che non contengono del contenuto esplicito
ggplot(df, aes(x=popularity)) + geom_histogram(fill = "light blue") + labs(title="Popolarità in base al contenuto esplicito") + facet_wrap(~df$explicit)

# istogramma sulla popolarità in base alla tonalità
# ci permette di vedere che il numero di canzoni con una tonalità maggiore siano di più, se pur di poco rispetto alle canzoni con tonalità minore
ggplot(df, aes(x=popularity)) + geom_histogram(fill = "purple") + labs(title="Popolarità in base alla tonalità") + facet_wrap(~df$mode)

# in questo primo paragone si può visualizzare e dedurre che le canzoni preferita siano canzoni che non contengano 
# contenuto esplicito e che abbiano una tonalità maggiore.

# installo un pacchetto per visualizzare più grafici contemporaneamente
# prima creo tutte le singole componenti, ovvero i singoli grafici
p1 <- ggplot(df, aes(x=acousticness)) + geom_bar(fill = "brown", width = 0.5)
p2 <- ggplot(df, aes(x=energy)) + geom_bar(fill = "brown", width = 0.5)
p3 <- ggplot(df, aes(x=danceability)) + geom_bar(fill = "brown", width = 0.5)
p4 <- ggplot(df, aes(x=loudness)) + geom_bar(fill = "brown", width = 0.5)
p5 <- ggplot(df, aes(x=speechiness)) + geom_bar(fill = "brown", width = 0.5)
p6 <- ggplot(df, aes(x=instrumentalness)) + geom_bar(fill = "brown", width = 0.5)
p7 <- ggplot(df, aes(x=liveness)) + geom_bar(fill = "brown", width = 0.5)
p8 <- ggplot(df, aes(x=valence)) + geom_bar(fill = "brown", width = 0.5)
p9 <- ggplot(df, aes(x=tempo)) + geom_bar(fill = "brown", width = 0.5)
p10 <- ggplot(df, aes(x=duration)) + geom_bar(fill = "brown", width = 0.5)
p11 <- ggplot(df, aes(x=popularity)) + geom_bar(fill = "brown", width = 0.5)
p12 <- ggplot(df, aes(x=year)) + geom_bar(fill = "brown", width = 0.5)

library(gridExtra)
grid.arrange(p1,p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, ncol=4, nrow=4)

# installo il pacchetto per fare matrice correlazione
install.packages("corrplot")
library(corrplot)

# creo un dataframe che poi passerò alla funzione cor per creare la matrice di correlazione.
# cor vuole un dataframe solo numerico quindi le colonne che non erano numeriche vengono trasformate in numeri o tolte
df_cor <- df
df_cor$explicit <- as.numeric(factor(df$explicit, c("T","F"), c(1, 0)))
df_cor$mode <- as.numeric(factor(df$mode, c("Tonalità Maggiore","Tonalità Minore"), c(1, 0)))
df_cor$song <- NULL
df_cor$artist <- NULL
df_cor$genre <- NULL

# creo la matrice di correlazione e poi la stampo
cor.matrix = cor(df_cor)
corrplot(cor.matrix, method="circle", tl.col = "black", tl.cex = 0.5)

# dimostro alcne correlazioni trovate nel corrplot tramite due grafici
plot(df$popularity,df$year, xlab = "Popolarità", ylab = "Anno")
plot(df$popularity,df$duration, xlab = "Popolarità", ylab = "Durata")


#### 6) ADDESTRAMENTO DEL MODELLO ####
# installo il pacchetto e1071
install.packages("e1071")
library(e1071)

# addestriamo il modello
# scegliamo di procedere inserendo come cost e degree i medesimi valori della traccia per il progetto
model.SVM <- svm(explicit~., df, kernel = "polynomial", cost=10, degree=10)
print(model.SVM)
summary(model.SVM)


#### 7) VALUTAZIONE DELLA PERFORMANCE ####
# creo la funzione MR (primo tool per valutare il modello)
MR <- function(y.pred, y.true) { 
  res <- mean(y.pred != y.true) 
  return (res)
}

# creo la funzione Acc (secondo tool per valutare il modello)
Acc <- function(y.pred, y.true) { 
  res <- 1 - mean(y.pred != y.true) 
  return (res)
}

# facciamo una prova valutando il modello
y.pred <- predict(model.SVM,df.test)
MR(y.pred, df.test$explicit)
Acc(y.pred, df.test$explicit)


#### 8) HYPER-PARAMETER TUNING ####

# Creo un vettore di 10 elementi e lo assegno alla variabie MR.total
MR.total <- 1:10

# itero il ciclo 10 volte al fine di trovare il valore di degree migliore a costo 10
for(d in 1:10){
  model.SVM <- svm(explicit ~ ., df, kernel = "polynomial", cost=10, degree=d)
  y.pred <- predict(model.SVM, df.val) 
  MR.poly <- MR(y.pred, df.val$explicit) 
  MR.total[d] <- MR.poly
}
# carico in d il degree migliore
d <- which.min(MR.total)

# visualizzo un plot per vedere il deegre ad ogni ciclo
plot(MR.total, type='p', xlab="Grado", ylim=c(0,1))
#faccio uno zoom sulla zona interessata
plot(MR.total, type='p', xlab="Grado", ylim=c(0.25,0.4))

# applico la stessa procedura fatta per il grado anche per il costo
for(c in 1:10){
  model.SVM <- svm(explicit ~ ., df, kernel = "polynomial", cost=c, degree=d)
  y.pred <- predict(model.SVM, df.val) 
  MR.poly <- MR(y.pred, df.val$explicit) 
  MR.total[c] <- MR.poly
}
# carico in c il costo migliore
c <- which.min(MR.total)

# visualizzo un plot per vedere il cost ad ogni ciclo
plot(MR.total, type='p', xlab="Costo", ylim=c(0,1))
plot(MR.total, type='p', xlab="Costo", ylim=c(0.27,0.36))


#### 9) VALUTAZIONE DELLA PERFORMANCE ####

# In questa fase di valutazione prendiamo il modello e lo consideriamo sul test set
model.SVM <- svm(explicit ~ ., df, kernel = "polynomial", cost=c, degree=d)
y.pred <- predict(model.SVM,df.test)

MR.test <- MR(y.pred, df.test$explicit)
MR.test
Acc.test <- Acc(y.pred, df.test$explicit)
Acc.test


#### 10) INTERPRETAZIONE PROBABILISTICA ####

# addestriamo la svm probabilistica con kernel polynomial e i valori di 
# cost e deegre ottimali calcolati in precedenza
SVM.probs <- svm(explicit ~ ., df, kernel = "polynomial", cost=c, degree=d, probability = TRUE)

# usiamo la svm e facciamo predizioni
y.pred <- predict(SVM.probs, df.test, probability = TRUE)
y.pred

# estraggo una singola colonna, poichè mi sarà utile per creare il vettore seguente
y.probs <- attr(y.pred, "probabilities")
y.probs <- y.probs[,1]
y.probs

# prendo il vettore costruito e lo converto in un vettore di predizioni 
# questo vettore conterrà solo valori binari di 0 e 1
y.total <- rep(0, N.test)
y.total[y.probs > 0.5] <- 1 # treshold = 0.5
y.total

# creiamo una tabella che rappresenta la matrice di confusione
table(y.pred, df.test$explicit)


# installo il pacchetto ROCR
install.packages("ROCR")
library(ROCR)

# costruisco la curva di ROCR
pred <- prediction(y.probs, df.test$explicit)
perf <- performance(pred, "tpr", "fpr")

# calcolo l'AUC
auc <- performance(pred, "auc")
auc <- auc@y.values[[1]]

# stampo curva di ROC e AUC
plot(perf, colorize=TRUE, main=auc)

#### 11) STUDIO PROBABILISTICO SUI RISULTATI DELLA VALUTAZIONE ####

# stabilisco quante volte voglio fare ciclare i calcoli successivi
# scelgo 15 poichè è un valore >10 e permette di avere un calcolo "veloce"
k <- 15

# creo i vettori MR.SRS e Acc.SRS di lunghezza pari a k
MR.SRS <- 1:k 
Acc.SRS <- 1:k

# ripeto il codice fatto in precedenza, usando il backup poichè
# il nostro dataset originale era stato usato per il train set
N <- nrow(df_backup)
N.train <- 1283
N.test <- 280
N.val <- 269

# ripeto la fase 6 per addestrare il nostro modello
for (i in 1:k){
  df <- df_backup
  train.sample <- sample(N,N.train)
  df.train <- df[train.sample,]
  df.test <- df[-train.sample,]
  
  val.sample <- sample(N.test + N.val, N.val) 
  df.val <- df.test[val.sample,]
  df.test <- df.test[-val.sample,]
  df <- df.train
  MR.total <- 1:10
  
  # ripeto la fase 8 per prendere gli iperparametri ottimali
  # utilizzo cost=5 per velocizzare il calcolo
  for(d in 1:10){
    model.SVM <- svm(explicit ~ ., df, kernel = "polynomial", cost=5, degree=d)
    y.pred <- predict(model.SVM, df.val) 
    MR.poly <- MR(y.pred, df.val$explicit) 
    MR.total[d] <- MR.poly
  }
  d <- which.min(MR.total)
  
  for(c in 1:10){
    model.SVM <- svm(explicit ~ ., df, kernel = "polynomial", cost=c, degree=d)
    y.pred <- predict(model.SVM, df.val) 
    MR.poly <- MR(y.pred, df.val$explicit) 
    MR.total[c] <- MR.poly
  }
  c <- which.min(MR.total)
  
  model.SVM <- svm(explicit ~ ., df, kernel = "polynomial", cost=c, degree=d) 
  y.pred <- predict(model.SVM, df.test)
  
  #carico sui vettori MR.SRS e Acc.SRS i valori delle valutazioni del modello
  MR.SRS[i] <- MR(y.pred, df.test$explicit)
  Acc.SRS[i] <- Acc(y.pred, df.test$explicit)
}

# per poter usare la libreria ggplot viene richiesta la creazione di un dataset
# contenente i due array con le valutazioni del modello
SRS.data <- data.frame(MR.SRS=MR.SRS, Acc.SRS=Acc.SRS)

# vediamo attraverso degli istogrammi i valori di MR e Acc del nostro modello
# grafico per MR
ggplot(SRS.data, aes(x=MR.SRS)) + 
  geom_histogram(col="black", fill="green", binwidth=0.01) +
  labs(title="Misclassification Rate", y="Frequenza")

# grafico per ACC
ggplot(SRS.data, aes(x=Acc.SRS)) + 
  geom_histogram(col="orange", fill="purple", binwidth=0.01) +
  labs(title="Accuracy", y="Frequenza") 

summary(MR.SRS)
summary(Acc.SRS)

# inferenza riguardo alla distribuzione a cui appartiene il campione
# stima della media
sigma <- sd(MR.SRS)
mu0 <- 0.22
alpha <- 0.05
x <- MR.SRS
m <- mean(x)

# calcolo intervallo di confidenza
zalfa <- qnorm(1 - alpha /2,0,1)
c1 <- m - zalfa * sigma / sqrt(k) 
c2 <- m + zalfa * sigma / sqrt(k) 

# test verifica di ipotesi
if (mu0 < c1 | mu0 > c2) {
  message("Ipotesi: Rejected")
} else {
  message("Ipotesi: Not Rejected")
}

#### 12) FEATURE SELECTION ####

#vediamo inizialmente i valori
MR(y.pred, df.test$explicit) 
Acc(y.pred, df.test$explicit)

# Feature selection
# modello senza la feature loudness
model.SVM <- svm(explicit ~ .-loudness, df, kernel = "polynomial", cost=c, degree=d)
y.pred <- predict(model.SVM, df.test)
MR(y.pred, df.test$explicit) 
Acc(y.pred, df.test$explicit)

# modello senza la feature energy
model.SVM <- svm(explicit ~ .-energy, df, kernel = "polynomial", cost=c, degree=d)
y.pred <- predict(model.SVM, df.test)
MR(y.pred, df.test$explicit) 
Acc(y.pred, df.test$explicit)

