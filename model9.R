# Model 8 ####
# Advice is disconnected from initial decision
parallel <- T

# Agents have direct access to one another's confidence.
ARC <- Sys.info()[[1]] != 'Windows'
if(parallel)
  ARC <- T

# Storage path for results
resultsPath <- ifelse(ARC,'results/','results/')
time <- format(Sys.time(), "%F_%H-%M-%S")
resultsPath <- paste0(resultsPath,time)


# Libraries
if(!require('parallel')) {
  install.packages(repos="http://cran.r-project.org",'parallel')
  library(parallel)
}

if(ARC) {
  # sink(paste(resultsPath, 'log.txt'))
  print(Sys.info())
  # Set up the parallel execution capabilities
  nCores <- detectCores()
  cl <- makeCluster(nCores)
  print(paste('Running in parallel on', nCores, 'cores.'))
  reps <- nCores
} else {
  reps <- 1
}

# Clear the result storage variables
suppressWarnings(rm('rawdata'))
suppressWarnings(rm('results'))

# Define the function
runModel <- function(spec) {
  source('evoSim/evoSim/R/evoSim.R')
  data <- evoSim(agentCount = spec$agents,
                 agentDegree = spec$degree,
                 decisionCount = spec$decisions,
                 generationCount = 25,#00,
                 mutationChance = 0.01,
                 other = list(sensitivity = spec$sensitivity, 
                              sensitivitySD = spec$sensitivitySD,
                              startingEgoBias = spec$startingEgoBias,
                              adviceNoise = spec$adviceNoise,
                              badAdviceProb = spec$badAdviceProb),
                 makeAgentFun = function(modelParams, parents = NULL) {
                   # Inherit egoBias if there's a previous generation and we're not mutating
                   if(!is.null(parents)) {
                     if(runif(1) < modelParams$mutationChance) {
                       # mutate
                       egoBias <- rnorm(1, parents$egoBias, 0.1)
                     } else {
                       egoBias <- parents$egoBias
                     }
                   }
                   else {
                     # print(paste('novelty',parents$generation))
                     egoBias <- modelParams$other$startingEgoBias#rnorm(1, .5, 1)
                   }
                   sensitivity <- abs(rnorm(1, mean = modelParams$other$sensitivity, 
                                            sd = modelParams$other$sensitivitySD))
                   # Keep egoBias to within [0-1]
                   egoBias <- clamp(egoBias, maxVal = 1, minVal = 0)
                   return(data.frame(sensitivity, egoBias))
                 },
                 selectParentsFun = function(modelParams, agents, world, ties) {
                   tmp <- agents[which(agents$generation == world$generation),]
                   tmp <- tmp[order(tmp$fitness, decreasing = T),]
                   # drop the worst half of the population
                   # tmp <- tmp[1:2,]#(floor(nrow(tmp)/2)), ]
                   # the others get weighted by relative fitness which are transformed to +ve values
                   tmp$fitness <- tmp$fitness - min(tmp$fitness) + 1
                   # scale appropriately
                   while(any(tmp$fitness < 10))
                     tmp$fitness <- tmp$fitness * 10
                   # and round off
                   tmp$fitness <- round(tmp$fitness)
                   tickets <- vector(length = sum(tmp$fitness)) # each success buys a ticket in the draw
                   i <- 0
                   for(a in 1:nrow(tmp)) {
                     tickets[(i+1):(i+1+tmp$fitness[a])] <- a
                     i <- i + 1 + tmp$fitness[a]
                   }
                   winners <- sample(tickets, modelParams$agentCount, replace = T)
                   # The winners clone their egocentric discounting
                   winners <- tmp[winners,'id']
                   return(winners)
                 },
                 getAdviceFun = function(modelParams, agents, world, ties) {
                   mask <- which(agents$generation == world$generation)
                   agents$advisor[mask] <- apply(ties, 1, function(x) sample(which(x != 0),1))
                   # Fetch advice as a vector
                   n <- length(mask)
                   agents$advice[mask] <- rnorm(n, 
                                                rep(world$state, n), 
                                                clamp(agents$sensitivity[agents$advisor[mask]]
                                                      + modelParams$other$adviceNoise,
                                                      Inf))
                   agents$advice[mask & (runif(n) < modelParams$other$badAdviceProb)] <-
                     world$state + modelParams$other$sensitivity + 3*modelParams$other$sensitivitySD
                   return(agents)
                 },
                 getWorldStateFun = function(modelParams, world) {
                   return(50)
                 })
  # save results
  n <- length(unique(data$agents$generation))
  results <- data.frame(generation = unique(data$agents$generation), 
                        modelDuration = rep(data$duration, n),
                        sensitivity = rep(spec$sensitivity, n),
                        sensitivitySD = rep(spec$sensitivitySD, n))
  # bind in the stats of interest aggregated by the generation
  results <- cbind(results, 
                   aggregate(data$agents, 
                             list(data$agents$generation), 
                             mean)[ ,c('fitness', 
                                       'degree', 
                                       'sensitivity', 
                                       'egoBias', 
                                       'initialDecision', 
                                       'finalDecision')])
  rawdata <- data
  rawdata$rep <- 1
  return(list(rawdata = rawdata, results = results))
}


# Parameter space to explore
specs <- list()
for(x in c(1000)) 
  for(y in c(10)) 
    for(z in c(30)) 
      for(s in c(10,100)) 
        for(sSD in c(10)) 
          for(sEB in c(0.01, 0.99))
            for(aN in c(0))
              for(bA in c(.1,.1,.5))
                specs[[length(specs)+1]] <- list(agents=x,degree=y,decisions=z,
                                                 sensitivity=s,sensitivitySD=sSD,
                                                 startingEgoBias=sEB,
                                                 adviceNoise = aN,
                                                 badAdviceProb = bA)
# Testing code for debugging parallel stuff
rm('x','y','z','s','sSD','sEB','aN','bA')
runModel(specs[[1]])

# Run the models
# make sure the children can see the degree variable

# Run parallel repetitions of the model with these settings
startTime <- Sys.time()
if(!ARC) {
  degreeResults <- lapply(specs, runModel)
} else {
  print('Executing parallel operations...')
  degreeResults <- parLapply(cl, specs, runModel)
}
print('...combining results...')
# Join up results
for(res in degreeResults) {
  if(!exists('results'))
    results <- res$results
  else
    results <- rbind(results, res$results)
  if(!exists('rawdata'))
    rawdata <- list(res$rawdata)
  else
    rawdata[[length(rawdata)+1]] <- res$rawdata
}
print(paste0('...complete.'))
print(Sys.time() - startTime)

# Cleanup
if(ARC)
  stopCluster(cl)

print('Saving data...')
# Save data
write.csv(results, paste(resultsPath, 'results.csv'))
save(rawdata, file = paste(resultsPath, 'rawdata.Rdata'))
print('...complete.')

