#' @export

convertFormula = function(dat, chrom) {
  ## creates formula for glm() function
  ## output: object of class "formula"
  # dat: data in data frame, with first column as outcome variables
  # chrom: numeric vector of 0/1 for variable inclusion in model
  varNames = colnames(dat)
  if(sum(chrom)==0) {varInclude = "0"}
  varInclude = paste(varNames[2:ncol(dat)][chrom==1],collapse="+")
  return(as.formula(paste0(varNames[1], " ~ ", varInclude)))
}

evalFitness = function(fitfunc, chrom, dat, family) {
  ## evaluates AIC of linear model corresponding to chromosome
  ## output: numeric
  # chrom: numeric vector of 0/1 for variable inclusion in model
  # dat: data in data frame, with first column as outcome variable
  # returns AIC associated with corresponding linear model
  form = convertFormula(dat, chrom)
  mod = glm(form, family=family, data=dat)
  fitness = do.call(fitfunc, list(mod))
  return(fitness)
}

getFitness = function(genomes) {
  ## retrieves AICs from list of chromosomes
  ## output: numeric vector
  # genomes: list of "chromosome" objects
  sapply(genomes, function(obj) obj$fitness)
}

swapCol = function(dat, y_name) {
  # swaps the columns of the data set so that the first column corresponding to the dependent variable
  #output: data.frame
  #dat: the original data set
  #y_name: string of the name of the dependent variable
  # returns a new data set with the first columns as the dependent variable.
  col_names <- colnames(dat)
  y_ind <- which(col_names == y_name)
  new_dat <- cbind(dat[y_ind], dat[, -y_ind])
  return(new_dat)
}

