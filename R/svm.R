#' Using best performed tuning parameter traning svm model and output performance on test set
#' 
#' This function uses pre-tunned hyperparameters for Linear or RBF svm model to output Sensitivity,Specificity,f1_score,kappa of the model in the test set.

#' 
#' @param seed An integer specifying the random seed for model fitting.
#' @param reduced logical,whether run reduced model, which remove unimportant variables
#' @param RBF logical, whether fit RBF svm model. If False, fit linear RBF model.
#' @param train_x matrix, design matrix in training data
#' @param train_y vector, outcome in training data
#' @param test_x matrix, design matrix in testing data
#' @param test_y vector, outcome in testing data
#'
#' @return a list of index of model performance in test set: Sensitivity,Specificity,f1_score,kappa 
#' 
#' @examples
#'
#' run_svm(111,reduced = T,RBF=F,train_x=train_x,train_y=train_y,test_x=test_x,test_y=test_y)
#'
#' @import gtools caret ranger
#' 
#' @export


run_svm <- function(seed=seed,reduced,RBF,
                  train_x=train_x,train_y=train_y,
                  test_x=test_x,test_y=test_y){
    set.seed(seed)
    trCtl <- trainControl(method="cv", number=5, savePredictions=F,verboseIter = T)
    if(reduced==T) {
        a<-which(colnames(train_x)%in%c("n_exclamation","n_plus","n_tilde","n_space","n_comma","n_asterisk","n_dollar","n_hastag"))
        train_x<-train_x[,-a]
        test_x<-test_x[,-a]
        
        if (RBF==T) {
            m <- train(x = train_x,
                        y = train_y,
                        method = "svmRadial",
                        trControl = trCtl,
                        tuneGrid = expand.grid(C = 1.2,sigma=1),
                        preProcess = c("center","scale"))
        }
        
        else {
            m <- train(x = train_x,
                        y = train_y,
                        method = "svmLinear",
                        trControl = trCtl,
                        tuneGrid = expand.grid(C = 1.71),
                        preProcess = c("center","scale"))
            
        }
        
    }
    
    else {
        if (RBF==T) {
            m <- train(x = train_x,
                        y = train_y,
                        method = "svmRadial",
                        trControl = trCtl,
                        tuneGrid = expand.grid(C = 2,sigma=1),
                        preProcess = c("center","scale"))
        }
        
        else {
            m <- train(x = train_x,
                        y = train_y,
                        method = "svmLinear",
                        trControl = trCtl,
                        tuneGrid = expand.grid(C = 1.43),
                        preProcess = c("center","scale"))
        }
    }
    
    ##get the result in test set
    confusion <- confusionMatrix(predict(m, newdata=test_x), test_y)
    sensitivity <- confusion$byClass['Sensitivity']
    specificity <- confusion$byClass['Specificity']
    f1_score <- confusion$byClass['F1']
    kappa_result <- kappa2(data.frame(predicted=predict(m, newdata=test_x), actual=test_y))
    variable_importance=varImp(m)
    
    return(list(sensitivity=sensitivity,
                specificity=specificity,
                f1_score=f1_score,
                kappa_result=kappa_result))
}

