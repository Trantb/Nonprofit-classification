

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
