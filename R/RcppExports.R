# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

LogisticRegression <- function(y, X, batch_size = 50L, learning_rate = 1e-3, epsilon = 1e-6, max_iter = 20000L) {
    .Call('_phishing735_LogisticRegression', PACKAGE = 'phishing735', y, X, batch_size, learning_rate, epsilon, max_iter)
}

