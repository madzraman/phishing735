library(caret)
library(ranger)
library(tidyverse)
source('helpers.R')


# Load data
train_x <- get_data('train_x')
train_y <- get_data('train_y')
test_x <- get_data('test_x')
test_y <- get_data('test_y')

#' Return splitted data
#'
#' This function returns train/test x or y from the original data
#'
#' @param name a string from train_x, train_y, test_x, test_y
#'
#' @return A dataframe for train/test x; a factor of level 0 or 1 for train/test y
#'
#' @examples
#'
#' get_data('train_x')
#'
#' @export
get_data <- function(name) {
    out <- read.csv(paste0('..//derived_data/',name,'.csv'))
    if (ncol(out) == 1) {
        out <- factor(out[,1])
    }
    return(out)
}


# Run
param_grid<-expand.grid(mtry = c(2,10,19),
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

# Collect parameter metrics and save to file
res <- list()
for  (ntree in c(50,500,5000)) {
    name <- paste0(ntree,"_tr_model")
    model <- models[[name]]
    res[[name]] <- model$results %>% mutate(n_tree = ntree)
}
res <- do.call(rbind,res)
write.table(res,'../derived_data/rf_model_selection.csv',row.names=F,col.names = T,quote=F,sep=',')

p <- ggplot(data=res,aes(x = as.factor(n_tree),y=Kappa, color=as.factor(min.node.size))) +
    theme_bw() +
    facet_wrap(~ mtry,labeller = label_both) +
    geom_line() +
    geom_point(position = position_dodge(0.3)) +
    geom_errorbar(aes(ymin = Kappa-KappaSD, ymax=Kappa+KappaSD),width = 0.1, position = position_dodge(0.3)) +
    labs(x = "Number of trees",color = 'Minimal node size')

# Prediction and confusion matrix on test data
pred_y <- predict(fit,newdata = test_x)
confusionMatrix(data = pred_y,reference = test_y)