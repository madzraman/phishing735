library(table1)
library(tidyverse)
# Summary of data by outcome
df <- read.csv("../source_data/web-page-phishing.csv")
table1(~ . | phishing, data = df)

# PCA
y <- df$phishing
x <- df %>% select(-phishing)
pca <- prcomp(t(x), scale = F)$rotation

pca_aug <- data.frame(index = 1:nrow(x), PC1 = pca[,1],PC2 = pca[,2], is_phishing = as.factor(y))

ggplot(pca_aug, aes(x=PC1, y=PC2,col=is_phishing)) +
    geom_point(alpha=0.1,size=1)

# You would see an outlier at left up corner. It is at row 44518 (with a very long url).
