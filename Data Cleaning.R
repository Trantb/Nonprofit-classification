#Set up before the work
x <- c('stringr', 'openxlsx', 'tm',
       'SnowballC', 'corpus', 'glmnet', 'caret'
       'neuralnet')
Install.packages(x)

library(stringr)
library(openxlsx)
library(tm)
library(SnowballC)
library(corpus)
library(glmnet)
library(caret)
library(neuralnet)

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
