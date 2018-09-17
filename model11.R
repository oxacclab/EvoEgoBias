# Model 11 ####

#' Models 6-9 established that, for a simple estimation problem, an evolutionary
#' pressure can emerge favouring egocentric bias where advice is communicated
#' noisily (models 6 & 7), made with less sensitivity than initial decisions
#' (model 8), or is occasionally deliberately misleading (model 9).

#' Here we explore whether these findings remain true for a different type of
#' decision, namely a categorical problem. Here the world value is uniformly
#' distributed between 0 and 100 (excluding 50), and values of 0-49 should be
#' categorised as 0 while values of 51-100 should be categorised as 1. Advice
#' comes graded by confidence and is combined with an internal estimate of
#' confidence to arrive at a final categorical decision.


# Libraries
if(!require('parallel')) {
  install.packages(repos="http://cran.r-project.org",'parallel')
  library(parallel)
}

if(!require('ggplot2')) {
  install.packages(repos="http://cran.r-project.org",'ggplot2')
  library(ggplot2)
}

style <- theme_light() +
  theme(legend.position = 'top',
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

parallel <- T


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
                 generationCount = 1000,
                 mutationChance = 0.01,
                 other = list(sensitivity = spec$sensitivity, 
                              sensitivitySD = spec$sensitivitySD,
                              startingEgoBias = spec$startingEgoBias,
                              adviceNoise = spec$adviceNoise,
                              manipulation = spec$manipulation,
                              shortDesc = spec$shortDesc),
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
                   while(any(abs(tmp$fitness) < 10))
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
                 getAdviceFun = spec$getAdviceFun,
                 getWorldStateFun = spec$getWorldStateFun,
                 getDecisionFun = spec$getDecisionFun,
                 getFitnessFun = spec$getFitnessFun)
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

# Decision functions - uncapped continuous (default), capped continuous, and discrete ####
uncappedDecisionFun <- function(modelParams, agents, world, ties, initial = F) {
  adviceNoise <- ifelse(modelParams$other$manipulation, modelParams$other$adviceNoise, 0)
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
    noise <- rnorm(length(mask), 0, adviceNoise)
    agents$adviceNoise[mask] <- noise
    out <- (agents$initialDecision[mask] * agents$egoBias[mask]) + 
      ((1-agents$egoBias[mask]) * (agents$advice[mask] + noise))
    #out <- clamp(out, 100)
    out[is.na(out)] <- agents$initialDecision[mask][is.na(out)]
    agents$finalDecision[mask] <- out
  }
  return(agents)
}

cappedDecisionFun <- function(modelParams, agents, world, ties, initial = F) {
  adviceNoise <- ifelse(modelParams$other$manipulation, modelParams$other$adviceNoise, 0)
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
    noise <- rnorm(length(mask), 0, adviceNoise)
    agents$adviceNoise[mask] <- noise
    out <- (agents$initialDecision[mask] * agents$egoBias[mask]) + 
      ((1-agents$egoBias[mask]) * (clamp(agents$advice[mask], 100) + noise))
    out <- clamp(out, 100)
    out[is.na(out)] <- agents$initialDecision[mask][is.na(out)]
    agents$finalDecision[mask] <- out
  }
  return(agents)
}

# World state functions - return 50 and return 0-49|51-100 ####
staticWorldStateFun = function(modelParams, world) {
  return(50)
}

variedWorldStateFun = function(modelParams, world) {
    x = 50
    while(x==50)
      x <- round(runif(1,0,100))
    return(x)
}

# Fitness functions - continuous (default) or categorical ####
categoricalFitnessFun <- function(modelParams, agents, world, ties) {
  mask <- which(agents$generation==world$generation)
  # fitness (error) increases by 1 for an incorrect answer
  answer <- world$state > 50
  answers <- agents$finalDecision[mask] > 50
  agents$fitness[mask] <- agents$fitness[mask] + as.numeric(answers==answer)
  return(agents)
}

# Advice functions - there's noisy advice and bad advice ####
noisyAdviceFun <- function(modelParams, agents, world, ties) {
  adviceNoise <- ifelse(modelParams$other$manipulation, 10, 0)
  mask <- which(agents$generation == world$generation)
  agents$advisor[mask] <- apply(ties, 1, function(x) sample(which(x != 0),1))
  # Fetch advice as a vector
  n <- length(mask)
  agents$advice[mask] <- rnorm(n, 
                               rep(world$state, n), 
                               clamp(agents$sensitivity[agents$advisor[mask]]
                                     + adviceNoise,
                                     Inf))
  agents$advice[mask] <- clamp(agents$advice[mask], 100)
  return(agents)
}

badAdviceFun <- function(modelParams, agents, world, ties) {
  badAdviceProb <- ifelse(modelParams$other$manipulation, .1, 0)
  mask <- which(agents$generation == world$generation)
  agents$advisor[mask] <- apply(ties, 1, function(x) sample(which(x != 0),1))
  # Fetch advice as a vector
  n <- length(mask)
  agents$advice[mask] <- rnorm(n, 
                               rep(world$state, n), 
                               clamp(agents$sensitivity[agents$advisor[mask]],Inf))
  badActors <- mask & (runif(n) < badAdviceProb)
  # bad actors give advice as certain in the other direction
  agents$advice[badActors] <- ifelse(agents$advice[badActors] < 50, 
                                     50+3*modelParams$other$sensitivity, 
                                     -(50+3*modelParams$other$sensitivity)) 
  return(agents)
}

for(decisionType in 1:3) {
  for(adviceType in 1:3) {
    # Storage path for results
    resultsPath <- ifelse(ARC,'results/','results/')
    time <- format(Sys.time(), "%F_%H-%M-%S")
    resultsPath <- paste0(resultsPath,'d',decisionType,'a',adviceType,'_',time)
    
    # Clear the result storage variables
    suppressWarnings(rm('rawdata'))
    suppressWarnings(rm('results'))
    
    # Parameter space to explore
    specs <- list()
    for(s in c(1, 10))
      for(x in c(F, T))
        specs[[length(specs)+1]] <- list(agents=1000,degree=10,decisions=30,
                                         sensitivity=s,sensitivitySD=s,
                                         startingEgoBias=.99,
                                         adviceNoise=0,
                                         manipulation=x,
                                         wd = getwd())
    
    if(decisionType == 1) {
      for(i in 1:length(specs)) {
        specs[[i]]$getWorldStateFun <- staticWorldStateFun
        specs[[i]]$getDecisionFun <- uncappedDecisionFun
        specs[[i]]$shortDesc <- 'Uncapped decisions'
      }
    } else if(decisionType == 2) {
      for(i in 1:length(specs)) {
        specs[[i]]$getWorldStateFun <- staticWorldStateFun
        specs[[i]]$getDecisionFun <- cappedDecisionFun
        specs[[i]]$shortDesc <- 'Capped decisions'
      }
    } else {
      for(i in 1:length(specs)) {
        specs[[i]]$getWorldStateFun <- variedWorldStateFun
        specs[[i]]$getDecisionFun <- uncappedDecisionFun
        specs[[i]]$getFitnessFun <- categoricalFitnessFun
        specs[[i]]$shortDesc <- 'Categorical decisions'
      }
    }
    
    # Noisy advice = advice made with +10sd on error
    if(adviceType == 1) {
      for(i in 1:length(specs)) {
        specs[[i]]$getAdviceFun <- noisyAdviceFun
        specs[[i]]$shortDesc <- paste(specs[[i]]$shortDesc, 'with noisy advice')
      }
    # Bad advice = advice which is 3*mean sensitivity in the opposite direction
    } else if(adviceType == 2) {
      for(i in 1:length(specs)) {
        specs[[i]]$getAdviceFun <- badAdviceFun
        specs[[i]]$shortDesc <- paste(specs[[i]]$shortDesc, 'with bad advice')
      }
    # Noisy communication means noise is added at evalutation time rather than decision time
    } else {
      for(i in 1:length(specs)) {
        # getAdviceFun is NULL
        specs[[i]]$adviceNoise <- 10
        specs[[i]]$shortDesc <- paste(specs[[i]]$shortDesc, 'with noisy communication')
      }
    }
    
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
      # if(!exists('results'))
      #   results <- res$results
      # else
      #   results <- rbind(results, res$results)
      if(!exists('rawdata'))
        rawdata <- list(res$rawdata)
      else
        rawdata[[length(rawdata)+1]] <- res$rawdata
    }
    print(paste0('...complete.'))
    print(Sys.time() - startTime)
    
    print('Estimated data size:')
    print(object.size(rawdata), units = 'auto')
    print('Saving data...')
    # Save data
    # write.csv(results, paste(resultsPath, 'results.csv'))
    # print('...saved csv...')
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
      rd$agents$manipulation <- rep(rd$model$other$manipulation,nrow(rd$agents))
      rd$agents$description <- rep(rd$model$other$shortDesc, nrow(rd$agents))
      # only take a subset because of memory limitations
      allAgents <- rbind(allAgents, rd$agents[rd$agents$generation%%50 == 1
                                              | (rd$agents$generation%%25 == 1 & rd$agents$generation < 250), ])
    }
    save(allAgents, file = paste(resultsPath, 'rawdata_subset.Rdata'))
    print('...saved subset...')
    
    # Plot
    ggplot(allAgents, 
           aes(x=generation, y=egoBias, 
               colour = manipulation)) +
      geom_hline(yintercept = 0.5, linetype = 'dashed') +
      stat_summary(geom = 'point', fun.y = mean, size = 3, alpha = 0.25) +
      stat_summary(fun.data = mean_cl_boot, fun.args=(conf.int = .99), geom = 'errorbar', size = 1) +
      scale_y_continuous(limits = c(0,1)) +
      facet_wrap(~meanSensitivity, labeller = label_both) +
      labs(title = allAgents$description[1]) +
      style 
    ggsave(paste(resultsPath, 'graph.png'))
    
    print('...saved graph...')
    print('...data saved.')
  }
}


# Cleanup
if(ARC)
  stopCluster(cl)
print('Complete.')

