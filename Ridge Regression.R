#Set up before the work
x <- c('stringr', 'openxlsx', 'tm',
       'SnowballC', 'corpus', 'glmnet', 'caret')
Install.packages(x)

library(stringr)
library(openxlsx)
library(tm)
library(SnowballC)
library(corpus)
library(glmnet)
library(caret)

#Read the datafile
datafile <- read.xlsx('train.xlsx') 
# include 3846 nonprofit organizations with companies names, 
# mission statements and classification either 0-not important and 1-important 

#The idea is to divide a sentence into every single word. For example,
#'I have a big brown cat' will be divided into 'I' 'have' 'a' 'big' 'brown' 'cat'.
#After that, there are other steps to remove as much as unimportant information as possible

text <- data.frame(NULL) # create a data frame to store the transformation
for (i in 1:length(datafile$Mission.Statement)){ 
  corpus <- Corpus(VectorSource(datafile$Mission.Statement[i]))
  corpus <- tm_map(corpus, content_transformer(tolower)) # from uppercase to lower case
  corpus <- tm_map(corpus, removeWords, stopwords('english')) # remove common words such as 'a' 'an' 'the'
  corpus <- tm_map(corpus, removeNumbers) #remove number
  corpus <- tm_map(corpus, stem_snowball) #unify words family to one word such as 'sleep', 'slept', 'sleeping', 'asleep' to 'sleep'
  corpus <- tm_map(corpus, removePunctuation) #remove ',' '.' '!' '?'
  corpus <- tm_map(corpus, stripWhitespace) #remove white space after previous steps
  text[i,1] <- corpus$content
}

#The transformation is complete. However, it is not in the table form yet. Therefore, these steps transforms it to table form
text <- text_tokens(text$V1)
text <- Corpus(VectorSource(text))
text <- DocumentTermMatrix(text, control = list(minWordLength = 1))
text <- as.matrix(text) #matrix should work as well, but it is too heavy. Therefore, the table form is more efficient.
text <- as.data.frame(text) #the transformation is completed.

#Now we need to combine with the original datafile having the Essentials as '0' or '1' which is the dependent variable.
datafile <- cbind(datafile, text)
datafile <- datafile[,-c(1,2)] #Column 1 is company name, which we do not need. Column 2 is mission statement which
# we already completed the transformation.

#Ridge regression with full model
Independent <- model.matrix(Essential ~., datafile)[,-1]
Dependent <- datafile[,1]
set.seed(71)
cv.ridge <- cv.glmnet(Independent, Dependent, alpha = 0, family = 'binomial') # Ridge has alpha = 0 while Lasso has alpha = 1
model <- glmnet(Independent, Dependent, alpha = 0, family = 'binomial', 
                lambda = cv.ridge$lambda.min) #can try cv.ridge$lambda.1se
pred <- predict(model, Independent, type = 'response')
confusionMatrix(as.factor(ifelse(pred >= .5, '1', '0')), as.factor(datafile$Essential),
                positive = '1')

#Ridge Regression with 70% training and 30% testing
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
# Results overall are worse than Lasso Regression. 
