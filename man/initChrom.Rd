% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Initialization.R
\name{initChrom}
\alias{initChrom}
\title{initChrom}
\usage{
initChrom(dat, chrom = NULL, fitfunc = "AIC", family = "gaussian")
}
\arguments{
\item{dat}{dataframe, data to operation(default sets dependent variable in first column and independent varialbes is other columns)}

\item{chrom}{vector, chromesome which is composed of binary data(default is NULL)}

\item{fitfunc}{method, model selection method(default is AIC)}

\item{family}{family,for linear regression model the family should be a continuous probability density function (default is gaussian family)}
}
\description{
Initializes chromosome
}
