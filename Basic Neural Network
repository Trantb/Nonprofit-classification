#A problem I have with cleaning the data is that they do not simply to word only. Instead, it has some unneccessary part
#such as 'c(' ')'. Because of that the package 'neuralnet' does not work with these name, it keeps saying 
#'selected column undefined' although you choose the correct column for dependent variable.
#Therefore, I need to store all the names of variables in one vector and rename so that the package can run it.

names <- as.data.frame(t(t(names(datafile)))) #store all the name in one vector names

for (i in 1:length(datafile)){
  names(datafile)[i] <- paste('Var', as.character(i), sep = '') # rename all columns to 'Var 1' - 'Var 8842'
}
names(datafile)


#Normalization
datafile$Var1 <- as.integer(datafile$Var1) 
for (i in 2:length(datafile)){
  datafile[,i] <- (datafile[,i] - min(datafile[,i]))/
    (max(datafile[,i]) - min(datafile[,i]))
} #because column counts the frequency of each word in the mission statements. We have to normailize them
#so that their values are between 0 and 1

#Neural Network full model
#I choose a simple version of 3 nots
set.seed(54)
n <- neuralnet(Var1 ~ ., data = datafile, linear.output = F,
               hidden = 3, err.fct = 'ce') #If you try original name, it does not work but work with this new name
output <- compute(n, datafile[,-1]) #compute prediction
confusionMatrix(as.factor(ifelse(output$net.result >= .5, '1', '0')),
                as.factor(datafile$Var1), positive = '1')
#The full model predicts correctly 98% (which is too high, maybe overfitting). 
#The later results in testing data did confirm that

#Neural Network with training and testing data
train.index <- sample(nrow(datafile), nrow(datafile)*.7)
train.data <- datafile[train.index, ]
valid.data <- datafile[-train.index, ]

nn <- neuralnet(Var1 ~ ., data = train.data, linear.output = F,
                hidden = 3, err.fct = 'ce') 
#Later, I try a different hidden layer with 'hidden = c(5,3), but it does not different much in results
output1 <- compute(nn, valid.data[,-1])
confusionMatrix(as.factor(ifelse(output1$net.result >= 0.5, '1', '0')),
                as.factor(valid.data[,1]), positive = '1')
#The overall accuracy is only 64% with sensitivity is 76% and specificity is 59% (which is worse compared with
#Lasso regression)
