% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{get_data}
\alias{get_data}
\title{Return splitted data}
\usage{
get_data(name)
}
\arguments{
\item{name}{a string from train_x, train_y, test_x, test_y}
}
\value{
A dataframe for train/test x; a factor of level 0 or 1 for train/test y
}
\description{
This function returns train/test x or y from the original data
}
\examples{

get_data('train_x')

}
