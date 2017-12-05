library(testthat)

initChrom = function(chrom=NULL, fitfunc="evalAIC", family="gaussian", n=3) {
  ## initializes new objects of class "chromosome"
  ## output: object of class "chromosome"
  # chrom: numeric vector of 0/1 for variable inclusion in model
  # fitfunc: fitness function associated with corresponding linear model
  # family: family argument for glm function
  # n: argument for parent.frame() to find original data
  dat = get("dat", envir=parent.frame(n))
  if(is.null(chrom)) {
    chrom = sample(c(0,1), size = ncol(dat)-1, replace=T)
  }
  fitness = do.call(fitfunc, list(chrom))
  obj = list(chrom, fitness)
  names(obj) = c("chrom", "fitness")
  class(obj) = "chromosome"
  return(obj)
}

convertFormula = function(chrom) {
  ## creates formula for glm() function
  ## output: object of class "formula"
  # chrom: numeric vector of 0/1 for variable inclusion in model
  dat = get("dat", envir=parent.frame(n=2))
  varNames = colnames(dat)
  varInclude = paste(varNames[2:ncol(dat)][chrom==1],collapse="+")
  return(as.formula(paste0(varNames[1], " ~ ", varInclude)))
}

evalAIC = function(chrom) {
  ## evaluates AIC of linear model corresponding to chromosome
  ## output: numeric
  # chrom: numeric vector of 0/1 for variable inclusion in model
  dat = get("dat", envir=parent.frame())
  family = get("family", envir=parent.frame())
  form = convertFormula(chrom)
  mod = glm(form, family=family, data=dat)
  return(AIC(mod))
}

initPop = function(popSize=30, genomes=NULL) {
  ## initializes new objects of class "population"
  ## output: object of class "population"
  # popSize: number of chromosomes in population
  dat = get("dat", envir=parent.frame())
  if(is.null(genomes)) {
    genomes = lapply(1:popSize, function(x) initChrom())
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

nextGen = function(pop, pSelect=0.2, pMutate=0.01) {
  ## 1. remove lowest pSelect*100% of chromosomes from population
  ## 2. repopulate with offspring
  ## 2.a. select parents with probability proportional to fitness
  ## 2.b. perform crossover between parent chromosomes
  ## 3. generate random mutations in population
  ## output: object of class "population"
  # pop: object of class "population"
  # pSelect: proportion of population to select out
  # pMutate: probability of mutation for single locus
  
  # 1. remove lowest pSelect*100% of chromosomes from population
  numRemove = floor(pSelect*length(pop$genomes))
  fitness = getFitness(pop$genomes)
  oldGenomes = pop$genomes[order(fitness, decreasing=F)>numRemove]
  
  # 2. repopulate with offspring
  weights = getFitness(oldGenomes)/sum(getFitness(oldGenomes))
  newGenomes = lapply(1:numRemove, 
                      function(x) {
                        crossover(sample(oldGenomes, size=2, prob=weights))
                      })
  genomes = unlist(list(oldGenomes, newGenomes), recursive=F)
  newPop = initPop(genomes=genomes)
  
  # 3. generate random mutations in population
  nMutations = rbinom(n = length(genomes), size=length(genomes[[1]]$chrom), prob=pMutate)
  newPop = mutatePop(newPop, nMutations)
  
  return(newPop)
}

crossover = function(parents) {
  ## performs crossover between 2 parent chromosomes
  ## output: object of class "chromosome"
  # parents: list of 2 "chromosome" objects
  chromA = parents[[1]]
  chromB = parents[[2]]
  nVars = length(chromA$chrom)
  pos = sample.int(nVars, size=1)
  chrom = c(chromA$chrom[1:pos], chromB$chrom[(pos+1):nVars])
  obj = initChrom(chrom=chrom, n=5)
  return(obj)
}

mutateChrom = function(chrom, nMutate) {
  ## performs mutation on single chromosomes
  ## output: object of class "chromosome"
  # chrom: object of class "chromosome"
  # nMutate: number of mutations to perform on chromosome
  nVars = length(chrom$chrom)
  posMutate = sample.int(nVars, size=nMutate)
  newChrom = chrom$chrom
  newChrom[posMutate] = abs(newChrom[posMutate]-1)
  obj = initChrom(chrom=newChrom)
  return(obj)
}

mutatePop = function(pop, nMutations) {
  ## performs mutations on population
  ## output: object of class "population"
  # pop: object of class "population"
  # nMutations: number of mutations to perform on each chromosome in pop
  toMutate = which(nMutations > 0)
  for(i in toMutate) {
    pop$genomes[[i]] = mutateChrom(pop$genomes[[i]], nMutations[i])
  }
  return(pop)
}

GA = function(dat, numIter = 1000, fitfunc="evalAIC", family="gaussian", 
              pSelect = 0.2, pMutate = 0.01) {
  ## wrapper function for genetic algorithm
  ## output: object of class "chromosome" corresponding to best model
  # dat: data in data frame, with first column as outcome variable
  # fitfunc: fitness function associated with corresponding linear model
  # family: family argument for glm function 
  # pSelect: proportion of population to select out
  # pMutate: probability of mutation for single locus
  
  # test inputs
  test_that("dat is data frame", {
    expect_equal(class(dat), "data.frame")
  })
}