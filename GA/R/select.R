#' @export


select <- function(dat, y_name, fitfunc="AIC", family="gaussian",
                   popSize=30, pSelect=0.2, pMutate=0.01, max_iter=1000) {
  new_dat = swapCol(dat = dat, y_name = y_name)
  init_pop = initPop(dat = new_dat, popSize = popSize, genomes=NULL,
                     fitfunc = fitfunc, family = family)
  best_fitness = rep(NA, max_iter+1)
  best_fitness[1] = init_pop$bestChrom$fitness
  avg_fitness = rep(NA, max_iter+1)
  avg_fitness[1] = mean(getFitness(init_pop$genomes))
  next_gen = nextGen(init_pop, pSelect=pSelect, pMutate=pMutate, fitfunc=fitfunc, family=family)
  best_gen = next_gen
  for(i in 1:max_iter) {
    best_fitness[i+1] = next_gen$bestChrom$fitness
    avg_fitness[i+1] = mean(getFitness(next_gen$genomes))
    updated_gen = nextGen(next_gen, pSelect=pSelect,
                          pMutate=pMutate, fitfunc=fitfunc, family=family)
#    set1 <-c(next_gen, NA)
#    set2 <-c(updated_gen, NA)
#    if(setequal(set1, set2)) {
#      print(paste("The best model selected by ", formals(nextGen)$fitfunc, " using ",
#                  formals(nextGen)$family, " distribution is generated at the ",
#                  i, "th iteration.", "The fitness value for the model is ",
#                  best_gen$bestChrom$fitness, ".", sep = ""))
#      break
#      }
    next_gen = updated_gen
    if(min(best_fitness, na.rm=T) >= updated_gen$bestChrom$fitness) {
      best_gen = updated_gen
    }
    if (i %% 100 == 0) print(paste("Finished the ", i, "th iteration.", sep = ""))
    if (i == max_iter) print(paste("This select function reaches the number of maximum iterations.",
                                   "The best model selected by ", formals(nextGen)$fitfunc, " using ",
                                   formals(nextGen)$family, " distribution within the maximum iterations",
                                   " has the fitness value as ", best_gen$bestChrom$fitness,
                                   ".", sep = ""))
  }
  par(mfrow=c(1,2))
  plot(best_fitness, main="best fitness per generation", xlab="iteration")
  plot(avg_fitness, main="average fitness per generation", xlab="iteration")
  selected_var_ind <- which(best_gen$bestChrom$chrom == 1)
  selected_var <- paste0(colnames(new_dat)[selected_var_ind + 1], collapse = " + ")
  best_model <- paste(y_name, " ~ ", selected_var)
  result <- list(best_model,
                 best_gen$bestChrom$chrom,
                 best_gen$bestChrom$fitness)
  names(result) <- c("model", "chrom", "fitness")
  return(result)
}
