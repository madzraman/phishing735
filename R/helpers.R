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
    out <- split_phishing_data()[[name]]
    if (ncol(out) == 1) {
        out <- factor(out[,1])
    }
    return(out)
}


#' Compute metrics and compute confusion matrix
#'
#' This function returns the output of caret's confusionMatrix and, if specified, plots the
#' confusion matrix heatmap
#'
#' @param pred A vector of factor 0,1 as the predicted labels
#' @param true A vector of factor 0,1 as the predicted labels. If NULL, use y_test
#' @param title a character specifying the plot title
#' @param file.out NULL if not saving plot locally. A character specifing the location
#' to save the output plot.
#'
#' @return A ggplot object.
#'
#'
#' @import tidyverse caret 
#' 
#' @export
confusion_mat <- function(pred,true=NULL,title,file.out=F) {
    if (is.null(true)) {
        true <- get_data('test_y')
    }
    m <- confusionMatrix(true,pred,mode='everything')
    nums <- as.vector(m$table)
    pred_class <- factor(c(0,1,0,1))
    true_class <- factor(c(0,0,1,1))
    
    df <- data.frame(nums,pred_class,true_class)
    
    p <- ggplot(data =  df, mapping = aes(x = pred_class, y = true_class)) +
        geom_tile(aes(fill = nums), colour = "white") +
        geom_text(aes(label = sprintf("%1.0f", nums)), vjust = 1,size=10) +
        scale_fill_gradient(low = "gray", high = "blue") +
        theme_bw() + theme(legend.position = "none") +
        ggtitle(title)
        
    
    if (!is.null(file.out)) {
        ggsave(file.out,p)
    }
    print(p)
    return(m)
}