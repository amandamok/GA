#' @export

nextGen = function(pop, pSelect=0.2, pMutate=0.01, fitfunc="AIC", family="gaussian") {
  ## 1. remove lowest pSelect*100% of chromosomes from population
  ## 2. repopulate with offspring
  ## 2.a. select parents with probability proportional to fitness
  ## 2.b. perform crossover between parent chromosomes
  ## 3. generate random mutations in population
  ## output: object of class "population"
  # pop: object of class "population"
  # pSelect: proportion of population to select out

  # 1. remove lowest pSelect*100% of chromosomes from population
  numRemove = floor(pSelect*length(pop$genomes))
  fitness = getFitness(pop$genomes)


  ####################################
  oldGenomes = pop$genomes[order(fitness, decreasing=F)[1:(length(fitness) - numRemove)]]
  ####################################


  # 2. repopulate with offspring

  ##########Might Wanna change our method here??################
  #adjust_fitness = (getFitness(oldGenomes) - max(getFitness(oldGenomes)))
  #if(sum(adjust_fitness) == 0)  return(pop)
  #weights = abs(adjust_fitness/sum(adjust_fitness))
  #if(length(which(weights == 1)) != 0)  return(pop)
  ##########Might wanna change our method here??################
  nChrom = length(oldGenomes)
  weights = 2*order(getFitness(oldGenomes), decreasing=T)/(nChrom*(nChrom+1))
  #if(length(which(weights == 1)) != 0)  return(pop)

  newGenomes = lapply(1:numRemove,
                      function(x) {
                        crossover(sample(oldGenomes, size=2, prob=weights),
                                  dat=pop$data, fitfunc=fitfunc, family=family)
                      })
  genomes = unlist(list(oldGenomes, newGenomes), recursive=F)
  newPop = initPop(dat=pop$data, popSize=NA, genomes=genomes,
                   fitfunc=fitfunc, family=family)

  # 3. generate random mutations in population
  nMutations = rbinom(n = length(genomes), size=(ncol(pop$data)-1), prob=pMutate)
  newPop = mutatePop(newPop, nMutations)

  return(newPop)
}
