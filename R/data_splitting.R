library(tidyverse)
library(caret)

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

write.csv(train.x, "phishing/derived_data/train_x.csv", row.names = FALSE)
write.csv(train.y, "phishing/derived_data/train_y.csv", row.names = FALSE)
write.csv(test.x, "phishing/derived_data/test_x.csv", row.names = FALSE)
write.csv(test.y, "phishing/derived_data/test_y.csv", row.names = FALSE)

# trx <- read.csv("phishing/derived_data/train_x.csv")
# try <- read.csv("phishing/derived_data/train_y.csv")
# tstx <- read.csv("phishing/derived_data/test_x.csv")
# tsty <- read.csv("phishing/derived_data/test_y.csv")
