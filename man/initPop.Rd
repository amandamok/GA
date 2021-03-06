% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Initialization.R
\name{initPop}
\alias{initPop}
\title{initPop}
\usage{
initPop(dat, popSize = 30, genomes = NULL, fitfunc = "AIC",
  family = "gaussian")
}
\arguments{
\item{dat}{dataframe, data to operation(default sets dependent variable in first column and independent varialbes is other columns)}

\item{popSize}{integer, population size(default=30)}

\item{genomes}{list, composed of chromosomes and fitness value with lenght equal to population size(default=NULL)}

\item{fitfunc}{method, model selection method(default is AIC)}

\item{family}{family,for linear regression model the family should be a continuous probability density function (default is gaussian family)}
}
\description{
Initializes population
}
