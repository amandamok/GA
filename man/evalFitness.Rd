% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Helper Function.R
\name{evalFitness}
\alias{evalFitness}
\title{evalFitness}
\usage{
evalFitness(fitfunc, chrom, dat, family)
}
\arguments{
\item{fitfunc}{method, model selection method(default is AIC)}

\item{chrom}{vector, a chromsome which composed of the binary dataset, 1 means to include in the model}

\item{dat}{dataframe, data to do mutation(default sets the dependent variable in first column and independent varialbes is other columns)}

\item{family}{family,for linear regression model the family should be a continuous probability density function (default is gaussian family)}
}
\description{
Evaluates AIC of linear model corresponding to chromosome
}
