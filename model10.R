# Model 10 ####

#' Models 6-9 established that, for a simple estimation problem, an evolutionary
#' pressure can emerge favouring egocentric bias where advice is communicated
#' noisily (models 6 & 7), made with less sensitivity than initial decisions
#' (model 8), or is occasionally deliberately misleading (model 10).

#' Here we explore whether these findings remain true for a different type of
#' decision, namely a bounded estimation problem. Again, the true answer each
#' time is 50, but this time agents' decisions and advice are always capped to
#' be between 0 and 100.

# Advice is disconnected from initial decision


# Libraries
if(!require('parallel')) {
  install.packages(repos="http://cran.r-project.org",'parallel')
  library(parallel)
}

parallel <- T

# Agents have direct access to one another's confidence.
ARC <- Sys.info()[[1]] != 'Windows'
if(ARC)
  setwd(paste0(getwd(), '/EvoEgoBias'))
if(parallel)
  ARC <- T

if(ARC) {
  print(Sys.info())
  # Set up the parallel execution capabilities
  nCores <- detectCores()
  cl <- makeCluster(nCores)
  print(paste('Running in parallel on', nCores, 'cores.'))
  reps <- nCores
} else {
  reps <- 1
}

# Define the function
runModel <- function(spec) {
  print(spec)
  setwd(spec$wd)
  source('evoSim/evoSim/R/evoSim.R')
  data <- evoSim(agentCount = spec$agents,
                 agentDegree = spec$degree,
                 decisionCount = spec$decisions,
                 generationCount = 2500,
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
                 getDecisionFun = function(modelParams, agents, world, ties, initial = F) {
                   mask <- which(agents$generation == world$generation)
                   if(initial) {
                     # initial decision - look and see
                     n <- length(mask)
                     agents$initialDecision[mask] <- rnorm(n, 
                                                           rep(world$state, n), 
                                                           clamp(agents$sensitivity[mask],Inf))
                   } else {
                     # Final decision - take advice
                     # Use vector math to do the advice taking
                     out <- NULL
                     noise <- rnorm(length(mask), 0, modelParams$other$adviceNoise)
                     out <- (agents$initialDecision[mask] * agents$egoBias[mask]) + 
                       ((1-agents$egoBias[mask]) * (agents$advice[mask] + noise))
                     out <- clamp(out, 100)
                     out[is.na(out)] <- agents$initialDecision[mask][is.na(out)]
                     agents$finalDecision[mask] <- out
                   }
                   return(agents)
                 },
                 getWorldStateFun = function(modelParams, world) {
                   return(50)
                 },
                 getAdviceFun = spec$getAdviceFun)
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

# Advice functions - there's noisy advice and bad advice
noisyAdviceFun <- function(modelParams, agents, world, ties) {
  mask <- which(agents$generation == world$generation)
  agents$advisor[mask] <- apply(ties, 1, function(x) sample(which(x != 0),1))
  # Fetch advice as a vector
  n <- length(mask)
  agents$advice[mask] <- rnorm(n, 
                               rep(world$state, n), 
                               clamp(agents$sensitivity[agents$advisor[mask]]
                                     + modelParams$other$adviceNoise,
                                     Inf))
  agents$advice[mask] <- clamp(agents$advice[mask], 100)
  return(agents)
}

badAdviceFun <- function(modelParams, agents, world, ties) {
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
  agents$advice[mask] <- clamp(agents$advice[mask], 100)
  return(agents)
}

adviceFunctions <- c(noisyAdviceFun, badAdviceFun)

for(modelNumber in 1:3) {
  # Storage path for results
  resultsPath <- ifelse(ARC,'results/','results/')
  time <- format(Sys.time(), "%F_%H-%M-%S")
  resultsPath <- paste0(resultsPath,'mdl',modelNumber,'_',time)
  
  # Clear the result storage variables
  suppressWarnings(rm('rawdata'))
  suppressWarnings(rm('results'))
  
  # Parameter space to explore
  specs <- list()
  for(x in c(1000)) 
    for(y in c(10)) 
      for(z in c(30)) 
        for(s in c(10,100)) 
          for(sSD in c(10)) 
            for(sEB in c(0.01, 0.99))
              for(aN in c(0, 10))
                for(bA in c(.1,.1,.5))
                  specs[[length(specs)+1]] <- list(agents=x,degree=y,decisions=z,
                                                   sensitivity=s,sensitivitySD=sSD,
                                                   startingEgoBias=sEB,
                                                   adviceNoise = aN,
                                                   badAdviceProb = bA,
                                                   wd = getwd())
  
  if(modelNumber <= length(adviceFunctions))
    for(spec in specs)
      spec$getAdviceFun <- adviceFunctions[[modelNumber]]
  
  # Testing code for debugging parallel stuff
  # rm('x','y','z','s','sSD','sEB','aN','bA')
  # runModel(specs[[1]])
  #specs <- specs[1:24]
  
  # Run the models
  
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
  
  
  print('Saving data...')
  # Save data
  write.csv(results, paste(resultsPath, 'results.csv'))
  print('...saved csv...')
  save(rawdata, file = paste(resultsPath, 'rawdata.Rdata'))
  print('...saved rawdata...')
  # Smaller datafile for stopping me running out of memory during analysis
  allAgents <- NULL
  for(rd in rawdata) {
    rd$agents$agentCount <- rep(rd$model$agentCount,nrow(rd$agents))
    rd$agents$agentDegree <- rep(rd$model$agentDegree,nrow(rd$agents))
    rd$agents$decisionCount <- rep(rd$model$decisionCount,nrow(rd$agents))
    rd$agents$modelDuration <- rep(rd$duration,nrow(rd$agents))
    rd$agents$meanSensitivity <- rep(rd$model$other$sensitivity,nrow(rd$agents))
    rd$agents$sdSensitivity <- rep(rd$model$other$sensitivitySD,nrow(rd$agents))
    rd$agents$startingEgoBias <- rep(rd$model$other$startingEgoBias,nrow(rd$agents))
    rd$agents$adviceNoise <- rep(rd$model$other$adviceNoise,nrow(rd$agents))
    rd$agents$badAdviceProb <- rep(rd$model$other$badAdviceProb,nrow(rd$agents))
    # only take a subset because of memory limitations
    allAgents <- rbind(allAgents, rd$agents[rd$agents$generation%%50 == 1
                                            | (rd$agents$generation%%25 == 1 & rd$agents$generation < 250), ])
  }
  save(allAgents, file = paste(resultsPath, 'rawdata_subset.Rdata'))
  print('...saved subset...')
  print('...data saved.')
}

# Cleanup
if(ARC)
  stopCluster(cl)
print('Complete.')

