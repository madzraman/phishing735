#' Split data into train and test sets
#'
#' This function splits original phishing data into train and test sets and writes them to csv files.
#'
#' @return train_x, train_y, test_x, test_y csv files written to derived_data directory.
#'
#' @examples
#'
#' split_phishing_data()
#'
#' @export

library(tidyverse)
library(caret)

split_phishing_data <- function(){
    df <- read.csv("phishing/source_data/web-page-phishing.csv")
    set.seed(123)
    
    x <- df[,1:19]
    y <- df[,20]
    train.idx <- createDataPartition(y, p=.80, list=FALSE)
    # table(seq_len(nrow(df)) %in% train.idx, y)
    
    train.x <- x[train.idx,]
    train.y <- y[train.idx]
    test.x <- x[-train.idx,]
    test.y <- y[-train.idx]
    
    # Convert response vectors to matrix form to write to csv
    train.y <- matrix(train.y, ncol = 1, dimnames = list(NULL, "phishing"))
    test.y <- matrix(test.y, ncol = 1, dimnames = list(NULL, "phishing"))
    
    # Confirm dimensions of train and test sets
    dim(train.x)
    length(train.y)
    dim(test.x)
    length(test.y)
    
    # Confirm column names are consistent
    colnames(train.x) == colnames(test.x)
    colnames(train.y) == colnames(test.y)
    
    write.csv(train.x, "derived_data/train_x.csv", row.names = FALSE)
    write.csv(train.y, "derived_data/train_y.csv", row.names = FALSE)
    write.csv(test.x, "derived_data/test_x.csv", row.names = FALSE)
    write.csv(test.y, "derived_data/test_y.csv", row.names = FALSE)

}
