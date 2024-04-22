#' Mini-Batch Logistic Regression
#'
#' Performs logistic regression using mini-batch gradient descent.
#'
#' @param y Numeric vector of binary response variables.
#' @param X Numeric matrix of predictor variables.
#' @param batch_size Size of each batch.
#' @param learning_rate Learning rate for gradient descent.
#' @param epsilon Convergence criterion for stopping gradient descent.
#' @param max_iter Maximum number of iterations.
#' @return A list containing the coefficients and the number of iterations.
#' @export
SGD_logistic <- function(y, X, batch_size = 50, learning_rate = 1e-3, epsilon = 1e-6, max_iter = 20000) {
  # Ensure RcppArmadillo is loaded
  if (!requireNamespace("RcppArmadillo", quietly = TRUE)) {
    stop("RcppArmadillo is required but not available!")
  }
  
  # Call the C++ function
  result <- .Call("LogisticRegression", y = as.numeric(y), X = as.matrix(X), 
                  batch_size, learning_rate, epsilon, max_iter)
  
  return(result)
}
