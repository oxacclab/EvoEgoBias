# Thesis simulations ####

#' * Agents evolve only their selfWeight
#' * Three scenarios:
#'   * Bad advice
#'   * Noisy advice
#'   * Confidence communication

#' Each run generates around 300MB of data, because for historical reasons the 
#' code saves subsets of data as well as the data itself. Each run takes around 
#' 20 minutes to run on my gaming computer (16 cores, 64GB RAM).
#' Over 50 runs you will need around 15GB of storage space.

runCount <- 50

# Libraries
library(parallel)
library(tidyverse)
library(gridExtra)
library(scales)
library(patchwork)

style <- theme_light() +
  theme(legend.position = 'top',
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

parallel <- T


ARC <- Sys.info()[[1]] != 'Windows'
if (ARC)
  setwd(paste0(getwd(), '/EvopPickSelf'))
if (parallel)
  ARC <- T

if (ARC) {
  print(Sys.info())
  # Set up the parallel execution capabilities
  nCores <- detectCores()
  cl <- makeCluster(nCores)
  print(paste('Running in parallel on', nCores, 'cores.'))
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
    data <- evoSim(
      genZero = generationZero,
      agentCount = spec$agents,
      agentDegree = spec$degree,
      decisionCount = spec$decisions,
      generationCount = min(genCount, spec$maxGenerations),
      mutationChance = 0.01,
      # recordDecisions = 100,
      other = list(
        sensitivity = spec$sensitivity, 
        sensitivitySD = spec$sensitivitySD,
        adviceNoise = spec$adviceNoise,
        manipulation = spec$manipulation,
        shortDesc = spec$shortDesc,
        startingSelfWeight = spec$startingSelfWeight
      ),
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
        
        # mutants and fresh spawns get randomly assigned pPickSelf
        makeAgents.agents <- data.frame(
          selfWeight = rep(modelParams$other$startingSelfWeight,
                           modelParams$agentCount),
          sensitivity = abs(rnorm(modelParams$agentCount,
                                  modelParams$other$sensitivity,
                                  modelParams$other$sensitivitySD)),
          generation = rep(g, modelParams$agentCount),
          adviceNoise = rep(NA, modelParams$agentCount),
          # Half the agents report confidence directly, half multiply it by .5 * manipulationValue
          confidenceScaling = rep(sample(c(0, .5), modelParams$agentCount, replace = T))
        )
        
        # Overwrite the agents' selfWeight by
        # inheritance from parents where applicable
        if (!is.null(parents)) {
          evolveCols <- c('selfWeight')
          makeAgents.agents[ , evolveCols] <- previousGeneration[parents, evolveCols]
          # mutants inherit from a normal distribution
          for (x in evolveCols) {
            mutants <- runif(modelParams$agentCount) < modelParams$mutationChance
            makeAgents.agents[mutants, x] <- rnorm(
              sum(mutants),
              previousGeneration[parents[mutants], x],
              0.1
            )
          }
        }
        
        # Keep values in sensible ranges 
        makeAgents.agents$selfWeight <- clamp(makeAgents.agents$selfWeight, maxVal = 1, minVal = 0)
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
      getFitnessFun = spec$getFitnessFun
    )
    
    # update the seed generation
    generationZero <- data$agents[data$agents$generation == 
                                    max(data$agents$generation), ]
    
    # Slim down the results if required
    if (!is.null(spec$saveEveryNthGeneration)) 
      data$agents <- data$agents[data$agents$generation < 
                                   spec$saveEveryNthGeneration |
                                   data$agents$generation %% 
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

# Decision function ####
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
    # Final decision - take advice by weighted averaging
    # Use vector math to do the advice taking
    out <- NULL
    noise <- rnorm(length(mask), 0, adviceNoise)
    roll <- runif(length(mask))
    agents$adviceNoise[mask] <- noise
    out <- (agents$initialDecision[mask] * agents$selfWeight[mask]) + 
      ((1 - agents$selfWeight[mask]) * (agents$advice[mask] + noise))
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

variableWorldStateFun = function(modelParams, world) {
  return(rnorm(1, 50, 1))
}

# Advice functions - there's noisy advice, bad advice, and confidence-weighted advice ####
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

confidenceAdviceFun <- function(modelParams, agents, world, ties) {
  # confidence scaling magnitude implements manipulation strength
  mag <- ifelse(
    modelParams$other$manipulation > 0,
    modelParams$other$adviceNoise[modelParams$other$manipulation],
    0
  )
  mask <- which(agents$generation == world$generation)
  agents$advisor[mask] <- apply(ties, 1, function(x) sample(which(x != 0),1))
  # Fetch advice as a vector
  n <- length(mask)
  midpoint <- 50
  # answer with confidence is obtained by: percept - 50 (halfway point)
  agents$advice[mask] <- rnorm(
    n, 
    rep(world$state, n), 
    clamp(agents$sensitivity[mask][agents$advisor[mask]], Inf)
  ) - midpoint
  # Scale for advisor confidence expression
  agents$advice[mask] <- agents$advice[mask] * 
    (agents$confidenceScaling[mask][agents$advisor[mask]] * mag + 1)
  agents$advice[mask] <- clamp(agents$advice[mask], midpoint, -midpoint)
  # Scale for judge confidence interpretation
  agents$advice[mask] <- agents$advice[mask] / 
    (agents$confidenceScaling[mask] * mag + 1)
  agents$advice[mask] <- agents$advice[mask] + midpoint
  return(agents)
}
for (r in 1:runCount) {
  for (adviceType in 3:1) {
    # Storage path for results
    resultsPath <- ifelse(ARC,'results/','results/')
    time <- format(Sys.time(), "%F_%H-%M-%S")
    
    # Clear the result storage variables
    suppressWarnings(rm('rawdata'))
    
    # Parameter space to explore
    specs <- list()
    for (s in 1) {
      for (x in 1:7) {
        specs[[length(specs) + 1]] <- list(
          agents = 1000, 
          startingSelfWeight = .45, # neither optimal nor discounted
          degree = 10,
          generations = 100,
          decisions = 60,
          sensitivity = s, 
          sensitivitySD = s,
          adviceNoise = seq(0,2,.3),
          manipulation = x,
          wd = getwd(),
          saveEveryNthGeneration = 1,
          maxGenerations = 500,
          getWorldStateFun = staticWorldStateFun,
          getDecisionFun = uncappedWeightedAvgFun,
          shortDesc = 'Weighted-Average'
        )
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
      # Confidence communication means agents have their own understanding of answers
    } else if (adviceType == 3) {
      for (i in 1:length(specs)) {
        specs[[i]]$getAdviceFun <- confidenceAdviceFun
        specs[[i]]$shortDesc <- paste(specs[[i]]$shortDesc, 'with confidence communication')
        specs[[i]]$getWorldStateFun <- variableWorldStateFun
      }
    }
    
    # Run the models
    
    # Run parallel repetitions of the model with these settings
    startTime <- Sys.time()
    
    print(paste0("Starting run ", r, "/", runCount))
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
      rd$agents$manipulation <- rep(rd$model$other$manipulation,nrow(rd$agents))
      rd$agents$manipulationValue <- rd$model$other$adviceNoise[rd$agents$manipulation]
      rd$agents$description <- rep(rd$model$other$shortDesc, nrow(rd$agents))
      # only take a subset because of memory limitations
      allAgents <- rbind(allAgents, rd$agents)
      # allAgents <- rbind(allAgents, rd$agents[rd$agents$generation %% 50 == 1
      #                                         | (rd$agents$generation %% 25 == 1 & rd$agents$generation < 250), ])
      #allDecisions <- rbind(allDecisions, rd$decisions[rd$decisions$generation %in% allAgents$generation, ])
    }
    
    allAgents$manipulationValue <- factor(allAgents$manipulationValue)
    save(allAgents, resultsPath, 
         file = paste0(resultsPath, '_rawdata-subset.Rdata'))
    
    # Plots
    # Inspect both variables for their evolution over time
    gg.selfWeight <- ggplot(allAgents, aes(x = generation, y = selfWeight,
                                           fill = manipulationValue)) +
      geom_hline(yintercept = 0.5, linetype = 'dashed') +
      stat_summary(geom = 'ribbon', fun.data = mean_cl_boot, 
                   fun.args = (conf.int = .99), alpha = 1, linetype = 0) +
      scale_y_continuous(limits = c(0,1)) +
      labs(title = allAgents$description[1], y = "self weight") +
      style + 
      theme(legend.position = "top")
    
    # Inspect fitness correlations of each variable
    predictors <- c('selfWeight')
    
    # bin the parameter values
    for (v in predictors)
      allAgents[, paste0(v, ".cat")] <- cut(allAgents[, v], 
                                            breaks = seq(0, 1.1, 0.1), 
                                            ordered_result = T,
                                            right = F)
    
    # reshape the data for neat plotting
    tmp <- gather(allAgents[, c("fitness", "manipulationValue",
                                paste0(predictors, ".cat"))], 
                  "parameter", "egocentricity", 3:(2 + length(predictors)), 
                  factor_key = T)
    
    gg.fitness <- ggplot(tmp, aes(x = egocentricity, y = fitness, 
                                  colour = manipulationValue, 
                                  fill = manipulationValue)) +
      stat_summary(geom = "point", aes(group = manipulationValue), size = 3,
                   fun = "mean") +
      geom_smooth(method = "lm", formula = y ~ x, aes(group = manipulationValue), se = F) +
      scale_y_continuous(limits = c(quantile(tmp$fitness, seq(0, 1, 0.05)[2]),
                                    0)) +
      facet_grid(.~parameter) +
      style
    
    gg <- gg.selfWeight / gg.fitness
    
    ggsave(paste0(resultsPath, '_graph.png'), gg,
           width = 11.2, height = 11.2, dpi = 1000)
    
    print('...data saved.')
  }
}

# Cleanup
if (ARC)
  stopCluster(cl)
print('Complete.')

