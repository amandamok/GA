#' @export

initChrom = function(dat, chrom=NULL, fitfunc="AIC", family="gaussian") {
  ## initializes new objects of class "chromosome"
  ## output: object of class "chromosome"
  # dat: data in data frame, with first column as outcome variable
  # chrom: numeric vector of 0/1 for variable inclusion in model
  # fitfunc: function name to evaluate fitness on glm object
  # family: family argument for glm function
  if(is.null(chrom)) {
    chrom = sample(c(0,1), size = ncol(dat) - 1, replace=T)
  }
  fitness = do.call(evalFitness, list(fitfunc, chrom, dat, family))
  obj = list(chrom, fitness)
  names(obj) = c("chrom", "fitness")
  class(obj) = "chromosome"
  return(obj)
}

initPop = function(dat, popSize=30, genomes=NULL, fitfunc="AIC", family="gaussian") {
  ## initializes new objects of class "population"
  ## output: object of class "population"
  # dat: data in data frame, with first column as outcome variable
  # popSize: number of chromosomes in population
  if(is.null(genomes)) {
    genomes = lapply(1:popSize,
                     function(x) initChrom(dat, fitfunc=fitfunc, family=family))
  }
  fitness = getFitness(genomes)
  ##########################################
  bestChrom = genomes[[which.min(fitness)]]
  ##########################################
  obj = list(dat, genomes, bestChrom)
  names(obj) = c("data","genomes", "bestChrom")
  class(obj) = "population"
  return(obj)
}
