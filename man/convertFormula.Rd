% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Helper Function.R
\name{convertFormula}
\alias{convertFormula}
\title{convertFormula}
\usage{
convertFormula(dat, chrom)
}
\arguments{
\item{dat}{dataframe, data to generate offspring(default sets the dependent variable in first column and independent varialbes is other columns)}

\item{chrom}{vector, a chromsome which composed of the binary dataset, 1 means to include in the model}
}
\value{
strings of the regression formula
}
\description{
Creates formula for glm() function
}
\details{
Transfer the input chromesome into a model that composed of independent variable and selected dependent variables in the chromosome
}
