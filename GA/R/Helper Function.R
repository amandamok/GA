#' @export

convertFormula = function(dat, chrom) {
  ## creates formula for glm() function
  ## output: object of class "formula"
  # dat: data in data frame, with first column as outcome variables
  # chrom: numeric vector of 0/1 for variable inclusion in model
  varNames = colnames(dat)
  varInclude = paste(varNames[2:ncol(dat)][chrom==1],collapse="+")
  return(as.formula(paste0(varNames[1], " ~ ", varInclude)))
}

evalAIC = function(chrom, dat, family) {
  ## evaluates AIC of linear model corresponding to chromosome
  ## output: numeric
  # chrom: numeric vector of 0/1 for variable inclusion in model
  # dat: data in data frame, with first column as outcome variable
  # returns AIC associated with corresponding linear model
  form = convertFormula(dat, chrom)
  mod = glm(form, family=family, data=dat)
  return(AIC(mod))
}

getFitness = function(genomes) {
  ## retrieves AICs from list of chromosomes
  ## output: numeric vector
  # genomes: list of "chromosome" objects
  sapply(genomes, function(obj) obj$fitness)
}
