# Model 22 ####

#' Small model to check everything still runs okay with R version 4

#' Here we allow sensitivity to evolve (up to a maximum (in)sensitivity of .1).
#' We expect this senstivity evolution to initially suppress egocentric bias,
#' and then increase it, although there are various reasons why this may not
#' happen.
#' 
#' The outcome measure has changed from a self/other weighting to a probability 
#' of averaging (weighting = .5) as opposed to simply sticking with the initial
#' response.


# Libraries
library(parallel)
library(ggplot2)

style <- theme_light() +
  theme(legend.position = 'top',
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

parallel <- T


ARC <- Sys.info()[[1]] != 'Windows'
if (ARC)
  setwd(paste0(getwd(), '/EvoEgoBias'))
if (parallel)
  ARC <- T

if (ARC) {
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
  
  tmpName <- paste0(spec$shortDesc, '-tmpfile-', 
                    runif(1, max = 1000000), '-')
  
  i <- 0
  
  # Break up very long models into sequential shorter models
  genCount <- spec$generations
  
  generationZero <- NULL # model's seed generation data frame
  
  while (genCount > 0) {
    
    i <- i + 1
    
    print(paste0(spec$shortDesc, " [", i, "]genCount = ", genCount))
    
    setwd(spec$wd)
    
    # Load the evoSim package
    source('evoSim/evoSim/R/evoSim.R')
    
    # Run the model for spec$maxGenerations
    data <- evoSim(genZero = generationZero,
                   agentCount = spec$agents,
                   agentDegree = spec$degree,
                   decisionCount = spec$decisions,
                   generationCount = min(genCount, spec$maxGenerations),
                   mutationChance = 0.01,
                   # recordDecisions = 100,
                   other = list(sensitivity = spec$sensitivity, 
                                sensitivitySD = spec$sensitivitySD,
                                startingEgoBias = spec$startingEgoBias,
                                adviceNoise = spec$adviceNoise,
                                manipulation = spec$manipulation,
                                shortDesc = spec$shortDesc),
                   makeAgentsFun = function(modelParams, previousGeneration = NULL, parents = NULL) {
                     # Population size and generation extracted from current population or by
                     # modelParams$agentCount
                     if (is.null(previousGeneration)) {
                       n <- modelParams$agentCount
                       g <- 0
                     }
                     else {
                       n <- dim(previousGeneration)[1]
                       g <- previousGeneration$generation[1]
                     }
                     
                     g <- g + 1 # increment generation
                     
                     # mutants and fresh spawns get randomly assigned egoBias
                     makeAgents.agents <- data.frame(egoBias = rep(modelParams$other$startingEgoBias,
                                                                   modelParams$agentCount),
                                                     sensitivity = abs(rnorm(modelParams$agentCount,
                                                                             modelParams$other$sensitivity,
                                                                             modelParams$other$sensitivitySD)),
                                                     generation = rep(g, modelParams$agentCount),
                                                     adviceNoise = rep(NA, modelParams$agentCount),
                                                     confidenceScaling = sample(1:5, 1))
                     
                     # Overwrite the agents' egobias by
                     # inheritance from parents where applicable
                     if (!is.null(parents)) {
                       evolveCols <- c('egoBias')
                       makeAgents.agents[ , evolveCols] <- previousGeneration[parents, evolveCols]
                       # mutants inherit from a normal distribution
                       for (x in evolveCols) {
                         mutants <- runif(modelParams$agentCount) < modelParams$mutationChance
                         makeAgents.agents[mutants, x] <- rnorm(sum(mutants),
                                                                previousGeneration[parents[mutants], x],
                                                                0.1)
                       }
                     }
                     
                     # Keep values in sensible ranges 
                     makeAgents.agents$egoBias <- clamp(makeAgents.agents$egoBias, maxVal = 1, minVal = 0)
                     makeAgents.agents$sensitivity <- clamp(makeAgents.agents$sensitivity, maxVal = 1, minVal = 0.1)
                     
                     # Connect agents together
                     ties <- modelParams$connectAgents(modelParams, makeAgents.agents)
                     makeAgents.agents$degree <- sapply(1:nrow(makeAgents.agents), getDegree, ties)
                     return(list(agents = makeAgents.agents, ties = ties))
                   },
                   selectParentsFun = function(modelParams, agents, world, ties) {
                     tmp <- agents[which(agents$generation == world$generation),]
                     tmp <- tmp[order(tmp$fitness, decreasing = T),]
                     # the others get weighted by relative fitness which are transformed to +ve values
                     tmp$fitness <- tmp$fitness - min(tmp$fitness) + 1
                     # scale appropriately
                     while (any(abs(tmp$fitness) < 10))
                       tmp$fitness <- tmp$fitness * 10
                     # and round off
                     tmp$fitness <- round(tmp$fitness)
                     tickets <- vector(length = sum(tmp$fitness)) # each success buys a ticket in the draw
                     i <- 0
                     for (a in 1:nrow(tmp)) {
                       tickets[(i + 1):(i + 1 + tmp$fitness[a])] <- a
                       i <- i + 1 + tmp$fitness[a]
                     }
                     winners <- sample(tickets, modelParams$agentCount, replace = T)
                     # The winners clone their egocentric discounting
                     winners <- tmp[winners,'genId']
                     return(winners)
                   },
                   getAdviceFun = spec$getAdviceFun,
                   getWorldStateFun = spec$getWorldStateFun,
                   getDecisionFun = spec$getDecisionFun,
                   getFitnessFun = spec$getFitnessFun)
    
    # update the seed generation
    generationZero <- data$agents[data$agents$generation == 
                                    max(data$agents$generation), ]
    
    # Slim down the results if required
    if (!is.null(spec$saveEveryNthGeneration)) 
      data$agents <- data$agents[data$agents$generation %% 
                                   spec$saveEveryNthGeneration == 0, ]

    # Save temporary results
    saveRDS(data, file = paste0(tmpName, i))
    data <- NULL
    
    genCount <- genCount - spec$maxGenerations
  }
  
  # Collate and save full results
  rawdata <- list()
  
  for (n in 1:i) {
    tmp <- readRDS(paste0(tmpName, n))
    rawdata$agents <- rbind(rawdata$agents, tmp$agents)
    if (is.null(rawdata$model))
      rawdata$model <- tmp$model
    rawdata$duration <- c(rawdata$duration, tmp$duration)
    
    file.remove(paste0(tmpName, n))
  }
  
  rawdata$rep <- 1
  
  return(list(rawdata = rawdata))
}

# Decision functions - uncapped continuous (default), capped continuous, and discrete ####
uncappedPickOrAverageFun <- function(modelParams, agents, world, ties, initial = F) {
  adviceNoise <- ifelse(modelParams$other$manipulation > 0, 
                        modelParams$other$adviceNoise[modelParams$other$manipulation],
                        0)
  mask <- which(agents$generation == world$generation)
  if (initial) {
    # initial decision - look and see
    n <- length(mask)
    agents$initialDecision[mask] <- rnorm(n, 
                                          rep(world$state, n), 
                                          clamp(agents$sensitivity[mask],Inf))
  } else {
    # Final decision - take advice with probability (1 - egoBias)
    # Taking advice equates to simple averaging
    # Use vector math to do the advice taking
    out <- NULL
    noise <- rnorm(length(mask), 0, adviceNoise)
    roll <- runif(length(mask))
    agents$adviceNoise[mask] <- noise
    out <- ifelse(roll >= agents$egoBias[mask], 
                  (agents$initialDecision[mask] + agents$advice[mask] + noise) / 2,
                  agents$initialDecision[mask])
    #out <- clamp(out, 100)
    out[is.na(out)] <- agents$initialDecision[mask][is.na(out)]
    agents$roll[mask] <- roll
    agents$finalDecision[mask] <- out
  }
  return(agents)
}

uncappedWeightedAvgFun <- function(modelParams, agents, world, ties, initial = F) {
  adviceNoise <- ifelse(modelParams$other$manipulation > 0, 
                        modelParams$other$adviceNoise[modelParams$other$manipulation],
                        0)
  mask <- which(agents$generation == world$generation)
  if (initial) {
    # initial decision - look and see
    n <- length(mask)
    agents$initialDecision[mask] <- rnorm(n, 
                                          rep(world$state, n), 
                                          clamp(agents$sensitivity[mask],Inf))
  } else {
    # Final decision - take advice with probability (1 - egoBias)
    # Taking advice equates to simple averaging
    # Use vector math to do the advice taking
    out <- NULL
    noise <- rnorm(length(mask), 0, adviceNoise)
    roll <- runif(length(mask))
    agents$adviceNoise[mask] <- noise
    out <- (agents$initialDecision[mask] * agents$egoBias[mask]) + 
      ((1 - agents$egoBias[mask]) * (agents$advice[mask] + noise))
    #out <- clamp(out, 100)
    out[is.na(out)] <- agents$initialDecision[mask][is.na(out)]
    agents$roll[mask] <- roll
    agents$finalDecision[mask] <- out
  }
  return(agents)
}

# World state functions - return 50 and return 0-49|51-100 ####
staticWorldStateFun = function(modelParams, world) {
  return(50)
}

# Advice functions - there's noisy advice and bad advice ####
noisyAdviceFun <- function(modelParams, agents, world, ties) {
  adviceNoise <- ifelse(modelParams$other$manipulation > 0, 
                        modelParams$other$adviceNoise[modelParams$other$manipulation],
                        0)
  mask <- which(agents$generation == world$generation)
  agents$advisor[mask] <- apply(ties, 1, function(x) sample(which(x != 0),1))
  # Fetch advice as a vector
  n <- length(mask)
  agents$advice[mask] <- rnorm(n, 
                               rep(world$state, n), 
                               clamp(agents$sensitivity[mask][agents$advisor[mask]]
                                     + adviceNoise,
                                     Inf))
  agents$advice[mask] <- clamp(agents$advice[mask], 100)
  return(agents)
}

badAdviceFun <- function(modelParams, agents, world, ties) {
  badAdviceProb <- ifelse(modelParams$other$manipulation > 0, 
                          modelParams$other$adviceNoise[modelParams$other$manipulation]/100,
                          0)
  mask <- which(agents$generation == world$generation)
  agents$advisor[mask] <- apply(ties, 1, function(x) sample(which(x != 0),1))
  # Fetch advice as a vector
  n <- length(mask)
  agents$advice[mask] <- rnorm(n, 
                               rep(world$state, n), 
                               clamp(agents$sensitivity[mask][agents$advisor[mask]],Inf))
  badActors <- mask & (runif(n) < badAdviceProb)
  # bad actors give advice as plausible-but-extreme in a random direction from 
  # the best guess (i.e. initial estimate)
  agents$advice[badActors] <- ifelse(runif(sum(badActors)) < .5, 
                                     agents$advice[badActors] + 3 * agents$sensitivity[badActors], 
                                     agents$advice[badActors] - 3 * agents$sensitivity[badActors]) 
  return(agents)
}

for (decisionType in c(1, 2)) {
  for (adviceType in 2) {
    # Storage path for results
    resultsPath <- ifelse(ARC,'results/','results/')
    time <- format(Sys.time(), "%F_%H-%M-%S")
    
    # Clear the result storage variables
    suppressWarnings(rm('rawdata'))
    
    # Parameter space to explore
    specs <- list()
    for (s in 1)
      for (x in 1:11)
        specs[[length(specs) + 1]] <- list(agents = 750, degree = 10,
                                           generations = 100,
                                           decisions = 60,
                                           sensitivity = s, sensitivitySD = s,
                                           startingEgoBias = .45, # .45 is neither optimal nor extreme
                                           adviceNoise = seq(0,2,.2),
                                           manipulation = x,
                                           wd = getwd(),
                                           saveEveryNthGeneration = 5,
                                           maxGenerations = 1500)
    
    if (decisionType == 1) {
      for (i in 1:length(specs)) {
        specs[[i]]$getWorldStateFun <- staticWorldStateFun
        specs[[i]]$getDecisionFun <- uncappedPickOrAverageFun
        specs[[i]]$shortDesc <- 'Pick-or-Average'
      }
    } else if (decisionType == 2) {
      for (i in 1:length(specs)) {
        specs[[i]]$getWorldStateFun <- staticWorldStateFun
        specs[[i]]$getDecisionFun <- uncappedWeightedAvgFun
        specs[[i]]$shortDesc <- 'Weighted-Average'
      }
    } 
    
    # Noisy advice = advice made with +adviceNoise sd on error
    if (adviceType == 1) {
      for (i in 1:length(specs)) {
        specs[[i]]$getAdviceFun <- noisyAdviceFun
        specs[[i]]$shortDesc <- paste(specs[[i]]$shortDesc, 'with noisy advice')
      }
    # Bad advice = advice which is 3*mean sensitivity in the opposite direction
    } else if (adviceType == 2) {
      for (i in 1:length(specs)) {
        specs[[i]]$getAdviceFun <- badAdviceFun
        specs[[i]]$shortDesc <- paste(specs[[i]]$shortDesc, 'with bad advice')
      }
    # Noisy communication means noise is added at evalutation time rather than decision time
    } else if (adviceType == 3) {
      for (i in 1:length(specs)) {
        # getAdviceFun is NULL
        specs[[i]]$shortDesc <- paste(specs[[i]]$shortDesc, 'with noisy communication')
      }
    }
    
    # Run the models
    
    # Run parallel repetitions of the model with these settings
    startTime <- Sys.time()
    
    print(paste('Processing models:', specs[[1]]$shortDesc))
    
    if (!ARC) {
      degreeResults <- lapply(specs, runModel)
    } else {
      print('Executing parallel operations...')
      degreeResults <- parLapply(cl, specs, runModel)
    }
    print('...combining results...')
    # Join up results
    for (res in degreeResults) {
      if (!exists('rawdata'))
        rawdata <- list(res$rawdata)
      else
        rawdata[[length(rawdata) + 1]] <- res$rawdata
    }
    print(paste0('...complete.'))
    print(Sys.time() - startTime)
    
    print(paste0('Estimated data size: ', object.size(rawdata) * 1e-6, 'MB'))
    print('Saving data...')
    
    # Save data with a nice name
    resultsPath <- paste0(resultsPath, time, '_',
                          sub(" ", "-", 
                              sub(" decisions with ", "_", specs[[1]]$shortDesc)))
    save(rawdata, file = paste0(resultsPath, '_rawdata.Rdata'))
    # Smaller datafile for stopping me running out of memory during analysis
    allAgents <- NULL
    #allDecisions <- NULL
    for (rd in rawdata) {
      rd$agents$agentCount <- rep(rd$model$agentCount,nrow(rd$agents))
      rd$agents$agentDegree <- rep(rd$model$agentDegree,nrow(rd$agents))
      rd$agents$decisionCount <- rep(rd$model$decisionCount,nrow(rd$agents))
      rd$agents$modelDuration <- rep(sum(rd$duration),nrow(rd$agents))
      rd$agents$meanSensitivity <- rep(rd$model$other$sensitivity,nrow(rd$agents))
      rd$agents$sdSensitivity <- rep(rd$model$other$sensitivitySD,nrow(rd$agents))
      rd$agents$startingEgoBias <- rep(rd$model$other$startingEgoBias,nrow(rd$agents))
      rd$agents$manipulation <- rep(rd$model$other$manipulation,nrow(rd$agents))
      rd$agents$manipulationValue <- rd$model$other$adviceNoise[rd$agents$manipulation]
      rd$agents$description <- rep(rd$model$other$shortDesc, nrow(rd$agents))
      # only take a subset because of memory limitations
      allAgents <- rbind(allAgents, rd$agents)
      # allAgents <- rbind(allAgents, rd$agents[rd$agents$generation %% 50 == 1
      #                                         | (rd$agents$generation %% 25 == 1 & rd$agents$generation < 250), ])
      #allDecisions <- rbind(allDecisions, rd$decisions[rd$decisions$generation %in% allAgents$generation, ])
    }
    
    save(allAgents, file = paste0(resultsPath, '_rawdata-subset.Rdata'))
    
    # Plot
    ggplot(allAgents, 
           aes(x = generation, y = egoBias, 
               fill = factor(manipulationValue))) +
      geom_hline(yintercept = 0.5, linetype = 'dashed') +
      # stat_summary(geom = 'point', fun.y = mean, size = 3, alpha = 0.25) +
      # stat_summary(fun.data = mean_cl_boot, fun.args = (conf.int = .99), geom = 'errorbar', size = 1) +
      stat_summary(geom = 'ribbon', fun.data = mean_cl_boot, fun.args = (conf.int = .99), alpha = 1,
                   linetype = 0) +
      scale_y_continuous(limits = c(0,1)) +
      facet_grid(~meanSensitivity, labeller = label_both) +
      labs(title = allAgents$description[1], y = 'egocentricity') +
      style
      
    ggsave(paste0(resultsPath, '_graph.png'))
    
    print('...data saved.')
  }
}

# How does fitness change over time?
allAgents$manipulationValue <- factor(allAgents$manipulationValue)
ggplot(allAgents, aes(x = generation, y = fitness, colour = manipulationValue,
                      fill = manipulationValue,
                      linetype = description)) +
  geom_smooth(se = F) +
  facet_wrap(~manipulationValue, labeller = label_value) +
  labs(title = "Fitness by strategy and environment") + 
  style +
  theme(legend.title = element_blank())

# Cleanup
if (ARC)
  stopCluster(cl)
print('Complete.')

