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
#' @param cv an boolean indicating if running CV for parameter selection
#' @param mtry When cv=T: a vector of integers; when cv=F: a integer. Specifies choice of mtry for RF.
#' @param min.node.size  When cv=T: a vector of integers; when cv=F: a integer. Specifies choice of mim.node.size for RF.
#' @param n_tree When cv=T: a vector of integers; when cv=F: a integer. Specifies choice of n_tree for RF.
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
                    cv=T,
                   mtry=c(2,10,19),
                   min.node.size=c(1,5),
                   n_tree=c(5,50,500),
                   splitrule=c('gini','extratrees'),
                   seed=123,
                   verbose=F,
                   trimmed_var=NULL,
                   file = NULL) { 
    set.seed(seed)
    # Read data
    source('helpers.R')
    train_x <- get_data('train_x')
    train_y <- get_data('train_y')
    # Trim variables if required
    if (!is.null(trimmed_var)) {
        train_x <- train_x[,!(colnames(train_x) %in% trimmed_var)]
    }
    # Set up paramter grids
    if (cv) {
        param_grid<-expand.grid(mtry = mtry,
                            splitrule = splitrule,
                            min.node.size = min.node.size)
    
        trCtl <- trainControl(method="cv", number=k, savePredictions=F,verboseIter = verbose)
        
        models=list()
        if (verbose) {
            message("Start CV...")
        }
        
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

        if (verbose) {
            message("CV ends...")
        }
        
        # Collect parameter metrics and save to file
        res <- list()
        for  (ntree in n_tree) {
            name <- paste0(ntree,"_tr_model")
            model <- models[[name]]
            res[[name]] <- model$results %>% mutate(n_tree = ntree)
        }
        res <- do.call(rbind,res)
        
        # res <- read.csv('../derived_data/rf_model_selection.csv')
        # Annotate best model
        res <- res %>% arrange(desc(Kappa)) %>%
            mutate(is_best = 0, running_time = NA)
        # Fit the best model, compute running time for the optimal model
        res[1,]$is_best <- 1
        final_param <- res[1,c('mtry','splitrule','min.node.size')]
        final_ntree <- res[1,]$n_tree
    } else {
        final_param <- data.frame(mtry=mtry,
                                    splitrule=splitrule,
                                    min.node.size=min.node.size)
        final_ntree <- n_tree
    }

    if (verbose) {
        message("Fitting final model...")
    }
    
    
    start <- Sys.time()
    out <- train(
        y=train_y,
        x=train_x,
        method = "ranger",
        trControl = trainControl(method = "none"),
        num.trees= final_ntree,
        tuneGrid = final_param,
        importance = "impurity"
    )
    if (cv) {
        t <- Sys.time()-start
        res[1,]$running_time <- as.numeric(t)
    }

    if (!is.null(file) & cv) {
        write.table(res,file,row.names=F,col.names = T,quote=F,sep=',')
        if (verbose) {
            message(paste("CV results saved at",file))
    }
    }
    out
}


#' Plot parameter tuning results
#'
#' This function returns and (if specified) saves the scatter plot of Kappa
#' for different parameter combinations of random forest model fitted on phishing dataset's
#' training set after k-fold CV.
#'
#' @param file.in a character specifying the input csv of CV result
#' @param title a character specifying the plot title
#' @param file.out NULL if not saving plot locally. A character specifing the location
#' to save the output plot.
#'
#' @return A ggplot object.
#'
#' @examples
#'
#' plot_rf_cv("/path/to/csv","test plot")
#'
#' @importFrom tidyverse
#' 
#' @export
plot_rf_cv <- function(file.in,title,file.out=NULL) {
    df <- read.csv(file.in)
    p <- ggplot(data=df,aes(x = as.factor(n_tree),y=Kappa, color=splitrule)) +
        theme_bw() +
        facet_wrap(~ min.node.size + mtry,labeller = label_both) +
        geom_point(position = position_dodge(0.5)) +
        geom_errorbar(aes(ymin = Kappa-KappaSD, ymax=Kappa+KappaSD),width = 0.1, position = position_dodge(0.5)) +
        labs(x = "Number of trees",color = 'Splitting rule',title=title)
        
        
    p <- p + geom_point(data = df[df$is_best == 1,], 
        aes(x = as.factor(n_tree), y = Kappa), color= 'red',
                    shape = 21, size = 5,position = position_nudge(x = ifelse(df[df$is_best == 1,]$splitrule == 'gini', 0.15,-0.15)))
    if (!is.null(file.out)) {
        ggsave(file.out,p)
    }
    p
}

#' Compute (plot) confusion matrix
#'
#' This function returns the output of caret's confusionMatrix and, if specified, plots the
#' confusion matrix heatmap
#'
#' @param model a "train" object from caret. Inputs the fitted model.
#' @param title a character specifying the plot title
#' @param file.out NULL if not saving plot locally. A character specifing the location
#' to save the output plot.
#'
#' @return A ggplot object.
#'
#' @examples
#'
#' plot_rf_cv("/path/to/csv","test plot")
#'
#' @importFrom tidyverse caret 
#' 
#' @export
confusion_mat <- function(model,title,file.out=F) {
    source("helpers.R")
    test_x <- get_data('test_x')
    test_y <- get_data('test_y')

    pred_y <- predict(model,newdata=test_x)
    m <- confusionMatrix(test_y,pred_y)
    nums <- as.vector(m$table)
    pred_class <- factor(c(0,1,0,1))
    true_class <- factor(c(0,0,1,1))

    df <- data.frame(nums,pred_class,true_class)

    p <- ggplot(data =  df, mapping = aes(x = pred_class, y = true_class)) +
    geom_tile(aes(fill = nums), colour = "white") +
    geom_text(aes(label = sprintf("%1.0f", nums)), vjust = 1) +
    scale_fill_gradient(low = "gray", high = "blue") +
    theme_bw() + theme(legend.position = "none") +
    ggtitle(title)

    print(p)
    if (!is.null(file.out)) {
        ggsave(file.out,p)
    }
    m
}


# run_rf(mtry=c(2,8,16),verbose=T,trimmed_var=c('n_asterisk','n_dollar','n_hastag'),file='../derived_data/reduced_rf_model_selection.csv')
# run_rf(verbose=T,file='../derived_data/rf_model_selection.csv')
# plot_rf_cv("../derived_data/rf_model_selection.csv","Full model parameter tuning","../derived_data/rf_full_cv.jpg")
# plot_rf_cv("../derived_data/reduced_rf_model_selection.csv","Reduced model parameter tuning","../derived_data/rf_reduced_cv.jpg")

# full <- run_rf(cv=F,mtry=10,splitrule='gini',n_tree=50,min.node.size=5,verbose=T)
reduce <- run_rf(cv=F,mtry=8,splitrule='gini',n_tree=500,min.node.size=1,verbose=T,trimmed_var=c('n_asterisk','n_dollar','n_hastag'))
# confusion_mat(full,"Full model prediction confusion matrix","../derived_data/full_model_confusion_mat.jpg")
confusion_mat(reduce,"Reduced model prediction confusion matrix","../derived_data/reduced_model_confusion_mat.jpg")

