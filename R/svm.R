library(caret)
library(ranger)
source('helpers.R')


# Load data
train_x <- get_data('train_x')
train_y <- get_data('train_y')
test_x <- get_data('test_x')
test_y <- get_data('test_y')
# Set up 5-fold Cross Validation
trCtl <- trainControl(method="cv", number=5, savePredictions=F,verboseIter = T)
# Linear SVM
linear_svm_model <- train(x = train_x,
                  y = train_y,
                  method = "svmLinear",
                  trControl = trCtl,
                  tuneGrid = expand.grid(C = seq(0, 2, length = 20)),
                  preProcess = c("center","scale"))

# Radial SVM
param_grid<-expand.grid(C = seq(0, 2, length = 20),
                        sigma = seq(0, 1, length = 20))
radial_svm_model <- train(x = train_x,
                   y = train_y,
                   method = "svmRadial",
                   trControl = trCtl,
                   tuneGrid = param_grid,
                   preProcess = c("center","scale"))
