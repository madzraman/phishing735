library(caret)
library(ranger)
library(tidyverse)
#' Run random forest, output the optimal model, and save parameter tuning result.
#'
#' This function runs random forest classification on the training set of 
#' phishing data. k-fold CV will be run through the user-provided parameter
#' grid. A dataframe of CV will be saved if a local dir is specified. Ouput is the model
#' with the highest Kappa in CV, then fitted based on the complete training set data.
#'
#' @param k an integer for CV's fold number
#' @param mtry a vector of integers, specifying choice of mtry for RF.
#' @param min.node.size a vector of integers, specifying choice of mim.node.size for RF.
#' @param n_tree a vector of integers, specifying choice of num.trees for RF.
#' @param seed An integer specifying the random seed for model fitting.
#' @param trimmed_var NULL for none of a vector of strings indicating features to
#' be removed from x when training model.
#' @param verbose a boolean indicating if verbose for CV iterations should be printed out.
#' @param file NULL for no file output, or a string indicating the position where
#' the CV result is saved at
#'
#' @return A "train" object from caret: the optimal model.
#'
#' @examples
#'
#' run_rf(file='/path/to/output')
#'
#' @importFrom tidyverse caret ranger
#'
#' @export
run_rf <- function(k=5,
                   mtry=c(2,10,19),
                   min.node.size=c(1,5),
                   n_tree=c(50,500,5000),
                   seed=123,
                   verbose=F,
                   trimmed_var=NULL,
                   file = NULL) { 
    set.seed(seed)
    # Read data
    source('get_data.R')
    train_x <- get_data('train_x')
    train_y <- get_data('train_y')
    # Set up paramter grids
    param_grid<-expand.grid(mtry = mtry,
                            splitrule = c("extratrees"),
                            min.node.size = min.node.size)
    
    trCtl <- trainControl(method="cv", number=k, savePredictions=F,verboseIter = verbose)
    
    models=list()
    
    for (ntree in n_tree){
        rf_model <- train(x = train_x,
                          y = train_y,
                          method = "ranger",
                          trControl = trCtl,
                          tuneGrid = param_grid,
                          num.trees=ntree)
        name=paste0(ntree,"_tr_model")
        models[[name]]=rf_model
    }
    models <- readRDS('../derived_data/models.RDS')
    
    
    # Collect parameter metrics and save to file
    res <- list()
    for  (ntree in n_tree) {
        name <- paste0(ntree,"_tr_model")
        model <- models[[name]]
        res[[name]] <- model$results %>% mutate(n_tree = ntree)
    }
    res <- do.call(rbind,res)
    
    # Annotate best model
    res <- res %>% arrange(desc(Kappa)) %>%
        mutate(is_best = 0, running_time = NA)
    # Fit the best model, compute running time for the optimal model
    res[1,]$is_best <- 1
    final_param <- res[1,c('mtry','splitrule','min.node.size')]
    start <- Sys.time()
    out <- train(
        y=train_y,
        x=train_x,
        method = "ranger",
        trControl = trainControl(method = "none"),
        num.trees=50,
        tuneGrid = final_param,
        importance = "impurity"
    )
    t <- Sys.time()-start
    # p <- ggplot(data=res,aes(x = as.factor(n_tree),y=Kappa, color=as.factor(min.node.size))) +
    #     theme_bw() +
    #     facet_wrap(~ mtry,labeller = label_both) +
    #     geom_line() +
    #     geom_point(position = position_dodge(0.3)) +
    #     geom_errorbar(aes(ymin = Kappa-KappaSD, ymax=Kappa+KappaSD),width = 0.1, position = position_dodge(0.3)) +
    #     labs(x = "Number of trees",color = 'Minimal node size')
    # 
    if (!is.null(file)) {
        write.table(res,'file',row.names=F,col.names = T,quote=F,sep=',')
    }
}


