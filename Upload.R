library(stringr)
library(openxlsx)
library(tm)
library(SnowballC)
library(corpus)
library(glmnet)
library(caret)

datafile <- read.xlsx('train.xlsx')

text <- data.frame(NULL)
for (i in 1:length(datafile$Mission.Statement)){
  corpus <- Corpus(VectorSource(datafile$Mission.Statement[i]))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stem_snowball)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  text[i,1] <- corpus$content
}

text <- text_tokens(text$V1)
text <- Corpus(VectorSource(text))
text <- DocumentTermMatrix(text, control = list(minWordLength = 1))
text <- as.matrix(text)

text <- as.data.frame(text)
datafile <- cbind(datafile, text)
datafile <- datafile[,-c(1,2)]

#lasso regression with full model
Independent <- model.matrix(Essential ~., datafile)[,-1]
Dependent <- datafile[,1]

set.seed(95)
cv.lasso <- cv.glmnet(Independent, Dependent, alpha = 1, family = 'binomial')
model <- glmnet(Independent, Dependent, alpha = 1, 
                family = 'binomial', lambda = cv.lasso$lambda.min)
#Otherwise, you can try a different lambda with cv.lasso$lambda.1se
pred <- predict(model, Independent, type = 'response')
confusionMatrix(as.factor(ifelse(pred >= .5, '1', '0')), 
                as.factor(datafile$Essential), positive = '1')

#Lasso Regression with 70% train and 30% test
train.index <- sample(nrow(datafile), nrow(datafile)*.7)
train.data <- datafile[train.index, ]
valid.data <- datafile[-train.index, ]

Independent <- model.matrix(Essential ~., datafile.data)[,-1]
Dependent <- train.data[,1]
cv.lasso <- cv.glmnet(Independent, Dependent, alpha = 1, family = 'binomial')
model <- glmnet(Independent, Dependent, alpha = 1, 
                family = 'binomial', 
                lambda = cv.lasso$lambda.min) #can try cv.lasso$lambda.1se
Independent.valid <- model.matrix(Essential ~., valid.data)[,-1]
pred <- predict(model, Independent.valid, type = 'response')
confusionMatrix(as.factor(ifelse(pred >= 0.5, 1, 0)), as.factor(valid.data$Essential),
                positive = '1')


#ridge regression
Independent <- model.matrix(Essential ~., datafile)[,-1]
Dependent <- datafile[,1]
set.seed(71)
cv.ridge <- cv.glmnet(Independent, Dependent, alpha = 0, family = 'binomial')
model <- glmnet(Independent, Dependent, alpha = 0, family = 'binomial', 
                lambda = cv.ridge$lambda.min) #can try cv.ridge$lambda.1se
pred <- predict(model, Independent, type = 'response')
confusionMatrix(as.factor(ifelse(pred >= .5, '1', '0')), as.factor(datafile$Essential),
                positive = '1')

set.seed(47)
train.index <- sample(nrow(datafile), nrow(datafile)*.7)
train.data <- datafile[train.index, ]
valid.data <- datafile[-train.index, ]
Independent <- model.matrix(Essential ~., train.data)[,-1]
Dependent <- train.data[,1]
cv.ridge <- cv.glmnet(Independent, Dependent, alpha = 0, family = 'binomial')
model <- glmnet(Independent, Dependent, alpha = 0, family = 'binomial', 
                lambda = cv.ridge$lambda.min)
Independent.valid <- model.matrix(Essential ~., valid.data)[,-1]
pred <- predict(model, Independent.valid, type = 'response')
confusionMatrix(as.factor(ifelse(pred >= .5, 1, 0)), as.factor(valid.data$Essential),
                positive = '1')