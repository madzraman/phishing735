library(caret)
source('helpers.R')


# Load data
train_x <- get_data('train_x')
train_y <- get_data('train_y')
test_x <- get_data('test_x')
test_y <- get_data('test_y')

# Run 
trCtl <- trainControl(method="cv", number=5, savePredictions=F,verboseIter = T)
fit <- train(train_x,train_y,method='rf',trControl = trCtl)


pred_y <- predict(fit,newdata = test_x)
confusionMatrix(data = pred_y,reference = test_y)
saveRDS(fit,'../derived_data/model.RDS')
