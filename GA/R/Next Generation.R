#' @export

source("/Users/jytang/Desktop/GA/R/Helper Function.R")
source("/Users/jytang/Desktop/GA/R//Cross_over & Mutation.R")

nextGen = function(pop, pSelect=0.2, pMutate=0.01, fitfunc="evalAIC", family="gaussian") {
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
  oldGenomes = pop$genomes[order(fitness, decreasing=F)>numRemove]

  # 2. repopulate with offspring
  weights = getFitness(oldGenomes)/sum(getFitness(oldGenomes))
  newGenomes = lapply(1:numRemove,
                      function(x) {
                        crossover(sample(oldGenomes, size=2, prob=weights),
                                  dat=pop$data, fitfunc=fitfunc, family=family)
                      })
  genomes = unlist(list(oldGenomes, newGenomes), recursive=F)
  newPop = initPop(dat=pop$data, popSize=NA, genomes=genomes)

  # 3. generate random mutations in population
  nMutations = rbinom(n = length(genomes), size=(ncol(pop$data)-1), prob=pMutate)
  newPop = mutatePop(newPop, nMutations)

  return(newPop)
}
