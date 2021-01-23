# Deep Learning with Keras and Tensorflow

datafile <- as.matrix(datafile) # data must be converted to matrix form
dimnames(datafile) <- NULL # remove all labels and names

datafile[,-1] <- normalize(datafile[,-1]) # coverts so all variables are between 0 and 1
datafile[,1] <- as.numeric(datafile[,1])
summary(datafile)

#Partition the data to training and testing
train.index <- sample(nrow(datafile), nrow(datafile)*.7)
train.data <- datafile[train.index, -1]
valid.data <- datafile[-train.index, -1]
train.target <- datafile[train.index, 1]
valid.target <- datafile[-train.index, 1]

train.label <- to_categorical(train.target)
valid.label <- to_categorical(valid.target)

#Build the model. There are various options
#However, most of them are overfitting my data.
#Therefore, I just post one example

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 8, activation = 'relu', input_shape = c(8831)) %>% #8831 stands for variables
  layer_dense(units = 2, activation = 'sigmoid') # 2 stands for two options whether important or not
summary(model)

model %>%
  compile(loss = 'binary_crossentropy',
          optimizer = 'adam',
          metrics = 'accuracy')
history <- model %>%
  fit(train.data, train.label, epoch = 20, batch_size = 32,
      validation_split = 0.2)
#You can try to add more layer or increase nots in units. I did try, but the problem is that
#the overfitting keeps increasing as I add anything.


#Testing the model
model %>%
  evaluate(valid.data, test.label)

prob <- model %>%
            predict_proba(valid.data)

pred <- model %>%
            predict_classes(valid.data)

confusionMatrix(as.factor(ifelse(pred >= 0.5, '1', '0')),
                as.factor(valid.target), positive = '1')
#The model is correct roughly 75% in which Sensitivity is around 62% while Specificity is 82%.

                
