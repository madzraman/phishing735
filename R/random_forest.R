library(caret)
library(ranger)
source('helpers.R')


# Load data
train_x <- get_data('train_x')
train_y <- get_data('train_y')
test_x <- get_data('test_x')
test_y <- get_data('test_y')

# Run 

param_grid<-expand.grid(mtry = c(10),
                        splitrule = c("extratrees"),
                       min.node.size = c(1, 5))
trCtl <- trainControl(method="cv", number=5, savePredictions=F,verboseIter = T)

models=list()
for (ntree in c(50,500,5000)){
    set.seed(123)
    rf_model <- train(x = train_x,
                      y = train_y,
                      method = "ranger",
                      trControl = trCtl,
                      tuneGrid = param_grid,
                      num.trees=ntree)
    name=paste0(ntree,"_tr_model")
    models[[name]]=rf_model
}
trCtl <- trainControl(method="cv", number=5, savePredictions=F,verboseIter = T)
fit <- train(train_x,train_y,method='ranger',trControl = trCtl,tuneGrid = param_grid)
fit <- readRDS('../derived_data/model.RDS')


pred_y <- predict(fit,newdata = test_x)
confusionMatrix(data = pred_y,reference = test_y)
saveRDS(fit,'../derived_data/model.RDS')
