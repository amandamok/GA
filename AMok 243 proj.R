initChrom = function(dat, chrom=NA, AIC=NA, fitfunc="evalAIC", family="gaussian") {
  ## initializes new objects of class "chromosome"
  ## output: object of class "chromosome"
  # dat: data in data frame, with first column as outcome variable
  # chrom: numeric vector of 0/1 for variable inclusion in model
  # fitness: fitness (defaults to AIC) associated with corresponding linear model
  # fitfunc: fitness function
  # family: family argument for glm function
  if(is.na(chrom)) {
    chrom = sample(c(0,1), size = ncol(dat)-1, replace=T)
    fitness = do.call(fitfunc, list(chrom, dat, family))
  }
  obj = list(chrom, fitness)
  names(obj) = c("chrom", "fitness")
  class(obj) = "chromosome"
  return(obj)
}

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

initPop = function(dat, popSize, genomes=NULL) {
  ## initializes new objects of class "population"
  ## output: object of class "population"
  # dat: data in data frame, with first column as outcome variable
  # popSize: number of chromosomes in population
  if(is.null(genomes)) {
    genomes = lapply(1:popSize, function(x) initChrom(dat))
  }
  fitness = getFitness(genomes)
  bestChrom = genomes[[which.max(fitness)]]
  obj = list(genomes, bestChrom)
  names(obj) = c("genomes", "bestChrom")
  class(obj) = "population"
  return(obj)
}

getFitness = function(genomes) {
  ## retrieves AICs from list of chromosomes
  ## output: numeric vector
  # genomes: list of "chromosome" objects
  sapply(genomes, function(obj) obj$fitness)
}

breed = function(pop, pSelect=0.2) {
  ## removes lowest 100*pSelect% of chromosomes from population
  ## repopulate: offspring are replicates, parents selected proportional to fitness
  ## output: object of class "population"
  # pop: object of class "population"
  # pSelect: proportion of population to select out
  browser()
  test_that("pSelect is proportion", {
    expect_true(pSelect > 0)
    expect_true(pSelect < 1)
  })
  numRemove = floor(pSelect*length(pop$genomes))
  fitness = getFitness(pop$genomes)
  oldGenomes = pop$genomes[order(fitness, decreasing=F)>numRemove]
  weights = getFitness(oldGenomes)/sum(getFitness(oldGenomes))
  newGenomes = lapply(1:numRemove, function(x) sample(oldGenomes, weights))
  genomes = list(unlist(oldGenomes), unlist(newGenomes))
  newPop = initPop(dat=NA, popSize=NA, genomes=genomes)
  return(newPop)
}

crossover = function(chromA, chromB) {
  # chromA, chromB: two parent chromosomes
  
}

mutate = function(pop, pMutate) {
  # pop: population of chromosomes
  # pMutate: probability of mutation per locus
}