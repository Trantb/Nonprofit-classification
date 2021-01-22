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

#Lasso regression with full model
Independent <- model.matrix(Essential ~., datafile)[,-1] #create matrix of independent variables
Dependent <- datafile[,1] #vector of dependent variable which is the essential
set.seed(95)
cv.lasso <- cv.glmnet(Independent, Dependent, alpha = 1, family = 'binomial')
model <- glmnet(Independent, Dependent, alpha = 1, 
                family = 'binomial', lambda = cv.lasso$lambda.min) #Otherwise, you can try a different lambda with cv.lasso$lambda.1se

pred <- predict(model, Independent, type = 'response')
confusionMatrix(as.factor(ifelse(pred >= .5, '1', '0')), 
                as.factor(datafile$Essential), positive = '1') # compare between the prediction and original results. 
#Overall, the model is correct roughly 83%. However, the problem is the Sensitivity is way too low 64% while Specificity is 93%.

#Lasso Regression with 70% train and 30% test
train.index <- sample(nrow(datafile), nrow(datafile)*0.7)
train.data <- datafile[train.index, ]
valid.data <- datafile[-train.index, ]

Independent <- model.matrix(Essential ~., datafile.data)[,-1]
Dependent <- train.data[,1]
cv.lasso <- cv.glmnet(Independent, Dependent, alpha = 1, family = 'binomial')
model <- glmnet(Independent, Dependent, alpha = 1, 
                family = 'binomial', 
                lambda = cv.lasso$lambda.min) #can try cv.lasso$lambda.1se though the results are even worse.
Independent.valid <- model.matrix(Essential ~., valid.data)[,-1]
pred <- predict(model, Independent.valid, type = 'response')
confusionMatrix(as.factor(ifelse(pred >= 0.5, 1, 0)), as.factor(valid.data$Essential),
                positive = '1')
#The prediction is correct about 73% which Sensitivity is even lower than original model 47% and Specificity is 89%.
