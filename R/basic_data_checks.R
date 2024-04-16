# Basic checks of the original data set.

df <- read.csv("source_data/web-page-phishing.csv")

# Percent of 0s in each column:
z <- rep(NA, ncol(df))
for (i in 1:ncol(df)){
    z[i] <- sum(df[,i] == 0)/nrow(df)
}
z # many sparse zero-inflated Poisson-distributed covariates

# Number of NAs
sum(is.na(df))

# Data Types (all numeric variables) 
str(df)

# Summary statistics of each variable
summary(df)

# Variable names and Unique values of each variable/column
colnames(df)
unique(df$phishing)

# Any class imbalance? not too bad. 70/30 is considered a good spread.
# Percent of yes responses
mean(df$phishing==1)
# Percent of yes responses
mean(df$phishing==0)
