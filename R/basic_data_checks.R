# Basic checks of the original data set.

data_checks <- function(){
    library(Hmisc)
    library(tidyverse)
    df <- read_csv("source_data/web-page-phishing.csv")
    
    cat("*** Percent of 0s in each column: *** \n")
    z <- rep(NA, ncol(df))
    for (i in 1:ncol(df)){
        z[i] <- sum(df[,i] == 0)/nrow(df)
    }
    print(z)  
    cat("We see that there are many sparse variables in this dataset. ")
    
    cat("\n*** Examine univariate distributions of each variable (excluding outlier): *** \n")
    hist.data.frame(df[df$url_length<2000,])
    cat("We see that all of these variables, including the sparse variables, are zero-inflated Poisson-distributed covariates.")
    
    cat("\n***Number of NAs: *** \n")
    print(sum(is.na(df)))
    cat("There are no missing values in the data set at all.")
    
    cat("\n***Data Types: *** \n")
    str(df)
    cat("All variables are numeric counts.")
    
    cat("\n***Summary statistics of each variable: *** \n")
    print(summary(df))
    
    cat("***Variable names and Unique values of each variable/column: *** \n")
    print(colnames(df))
    print(unique(df$phishing))
    
    cat("***Any class imbalance?: *** \n") 
    # Percent of yes responses
    print(mean(df$phishing==1))
    # Percent of yes responses
    print(mean(df$phishing==0))
    cat("Not too bad. 70/30 is still considered a good spread.")
}

data_checks()
