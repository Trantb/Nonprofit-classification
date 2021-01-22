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
