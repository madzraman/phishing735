// [[Rcpp::depends(RcppArmadillo)]]

#include <cmath>
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List LogisticRegression(arma::vec y, arma::mat X, int batch_size = 50, double learning_rate = 1e-3, double epsilon = 1e-6, int max_iter = 20000) {
	
	arma::uvec indices = arma::shuffle(arma::linspace<arma::uvec>(0, X.n_rows - 1, X.n_rows));


	// concatenate 1 and covariates matrix
	arma::mat X_intercept = arma::join_rows(arma::ones(X.n_rows), X);

	// initialize coefficients
	arma::vec coef(X_intercept.n_cols); coef.zeros();
	arma::vec coef_old = coef;

	// calculate exp(X * coef)
	arma::vec ExpXcoef = arma::exp(X_intercept * coef);

	// initialize gradient
	arma::vec gradient(X_intercept.n_cols);

	// initialize epsilon and number of iterations
	double eps = 1.0;
	int iter = 0;

	// number of batches
	int num_batches = ceil(X.n_rows / double(batch_size));

	// start gradient descent
	while (eps > epsilon && iter < max_iter)
	{
		for (int batch = 0; batch < num_batches; batch++)
		{

			// create mini batch
			int start = batch * batch_size;
            int end = std::min<int>(X.n_rows, (batch + 1) * batch_size) - 1;

            arma::uvec batch_indices = indices.subvec(start, end);
            arma::mat X_batch = X_intercept.rows(batch_indices);
            arma::vec y_batch = y.elem(batch_indices);

            // calculate exp(X * coef)
			arma::vec ExpXcoef = arma::exp(X_batch * coef);

			// calculate gradient
			gradient = X_batch.t() * y_batch;
			for (int i = 0; i < X_batch.n_rows; i++) {
            	gradient -= (ExpXcoef[i] / (1.0 + ExpXcoef[i])) * X_batch.row(i).t();
        	}

        	coef += learning_rate * gradient / X_batch.n_rows;
		}
		// record number of iterations
		eps = arma::accu(arma::abs(coef - coef_old));
		coef_old = coef;
		iter++;
	}

	// save coefficient estimates and number of iterations
	Rcpp::List res;
	res["coefficients"] = Rcpp::NumericVector(coef.begin(), coef.end());
	res["iter"] = iter;
	
	return res;
}