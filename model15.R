# Model 16 ####

#' Here we explore whether egocentric bias is diverted from the optimum set
#' point when agents have one of 5 possible scaling factors they use in
#' confidence communication. The decision is cast as a categorical problem, with
#' the true answer normally distributed around the midline.
#'
#' Now, however, we also allow sensitivity to evolve (up to a maximum
#' (in)sensitivity of .1). We expect this senstivity evolution to initially
#' suppress egocentric bias, and then increase it, although there are various
#' reasons why this may not happen.


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
                 generationCount = 3000,
                 mutationChance = 0.01,
                 other = list(sensitivity = spec$sensitivity, 
                              sensitivitySD = spec$sensitivitySD,
                              startingEgoBias = spec$startingEgoBias,
                              adviceNoise = spec$adviceNoise,
                              manipulation = spec$manipulation,
                              shortDesc = spec$shortDesc),
                 makeAgentsFun = function(modelParams, previousGeneration = NULL, parents = NULL) {
                   # Population size and generation extracted from current population or by
                   # modelParams$agentCount
                   if(is.null(previousGeneration)) {
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
                   
                   # Overwrite the agents' egobias and sensitivity by
                   # inheritance from parents where applicable
                   if(!is.null(parents)) {
                     evolveCols <- c('egoBias', 'sensitivity')
                     makeAgents.agents[ , evolveCols] <- previousGeneration[parents, evolveCols]
                     # mutants inherit from a normal distribution
                     for(x in evolveCols) {
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
                   winners <- tmp[winners,'genId']
                   return(winners)
                 },
                 getAdviceFun = spec$getAdviceFun,
                 getWorldStateFun = spec$getWorldStateFun,
                 getDecisionFun = spec$getDecisionFun,
                 getFitnessFun = spec$getFitnessFun)
  # save results
  rawdata <- data
  rawdata$rep <- 1
  return(list(rawdata = rawdata))
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

# Confidence decision function decodes advice according to one's own habitual use of the confidence scale
confDecisionFun <- function(modelParams, agents, world, ties, initial = F) {
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
    # scale the advice by the agent's understanding of confidence scale
    if(modelParams$other$manipulation)
      scaled <- ((agents$advice[mask] - 50) * agents$confidenceScaling[mask]) + 50
    else
      scaled <- agents$advice[mask]
    out <- (agents$initialDecision[mask] * agents$egoBias[mask]) + 
      ((1-agents$egoBias[mask]) * scaled)
    #out <- clamp(out, 100)
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
    # round(ifelse(runif(1)>.5, runif(1,0,49), runif(1,51,100)))
  rnorm(1, mean = 50)
}

# Fitness functions - continuous (default) or categorical ####
categoricalFitnessFun <- function(modelParams, agents, world, ties) {
  mask <- which(agents$generation==world$generation)
  # fitness (error) increases by 1 for an incorrect answer
  answer <- world$state > 50
  answers <- agents$finalDecision[mask] > 50
  agents$fitness[mask] <- agents$fitness[mask] - as.numeric(answers!=answer)
  return(agents)
}

# Advice functions - there's noisy advice and bad advice ####
noisyAdviceFun <- function(modelParams, agents, world, ties) {
  adviceNoise <- ifelse(modelParams$other$manipulation, modelParams$other$adviceNoise, 0)
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
  badAdviceProb <- ifelse(modelParams$other$manipulation, modelParams$other$adviceNoise/100, 0)
  mask <- which(agents$generation == world$generation)
  agents$advisor[mask] <- apply(ties, 1, function(x) sample(which(x != 0),1))
  # Fetch advice as a vector
  n <- length(mask)
  agents$advice[mask] <- rnorm(n, 
                               rep(world$state, n), 
                               clamp(agents$sensitivity[mask][agents$advisor[mask]],Inf))
  badActors <- mask & (runif(n) < badAdviceProb)
  # bad actors give advice as certain in the other direction
  agents$advice[badActors] <- ifelse(agents$advice[badActors] < 50, 
                                     50+3*modelParams$other$sensitivity, 
                                     50-3*modelParams$other$sensitivity) 
  return(agents)
}

# Advice communicated with a particular confidence
confAdviceFun <- function(modelParams, agents, world, ties) {
  mask <- which(agents$generation == world$generation)
  agents$advisor[mask] <- apply(ties, 1, function(x) sample(which(x != 0),1))
  #' Fetch advice as a vector, scale it to center around 0 rather than 50,
  #' multiply it by a scaling factor, then add 50 again restore its
  #' positioning
  if(modelParams$other$manipulation)
    agents$advice[mask] <- ((agents$initialDecision[mask][agents$advisor[mask]] - 50) *
                              agents$confidenceScaling[mask][agents$advisor[mask]]) + 50
  else
    agents$advice[mask] <- agents$initialDecision[mask][agents$advisor[mask]]
  return(agents) 
}

for(decisionType in 3) {
  for(adviceType in 4) {
    # Storage path for results
    resultsPath <- ifelse(ARC,'results/','results/')
    time <- format(Sys.time(), "%F_%H-%M-%S")
    resultsPath <- paste0(resultsPath,'d',decisionType,'a',adviceType,'_',time)
    
    # Clear the result storage variables
    suppressWarnings(rm('rawdata'))
    
    # Parameter space to explore
    specs <- list()
    for(s in c(1, 10))
      for(x in c(F, T))
        specs[[length(specs)+1]] <- list(agents=1000,degree=10,decisions=30,
                                         sensitivity=s,sensitivitySD=s,
                                         startingEgoBias=.45, # .45 is neither optimal nor extreme
                                         adviceNoise=5,
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
    } else if(decisionType == 3) {
      for(i in 1:length(specs)) {
        specs[[i]]$getWorldStateFun <- variedWorldStateFun
        specs[[i]]$getDecisionFun <- cappedDecisionFun
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
    } else if(adviceType == 3) {
      for(i in 1:length(specs)) {
        # getAdviceFun is NULL
        specs[[i]]$shortDesc <- paste(specs[[i]]$shortDesc, 'with noisy communication')
      }
    } else {
      if(decisionType < 3)
        next() # don't run confidence communication version with non-categorical decisions
      for(i in 1:length(specs)) {
        specs[[i]]$getDecisionFun <- confDecisionFun # overwrite decision function
        specs[[i]]$getAdviceFun <- confAdviceFun
        specs[[i]]$shortDesc <- paste(specs[[i]]$shortDesc, 'with confidence communication')
      }
    }
    
    # Testing code for debugging parallel stuff
    # rm('x','y','z','s','sSD','sEB','aN','bA')
    # testData <- runModel(specs[[1]])
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
    save(rawdata, file = paste(resultsPath, 'rawdata.Rdata'))
    print('...saved rawdata...')
    # Smaller datafile for stopping me running out of memory during analysis
    allAgents <- NULL
    #allDecisions <- NULL
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
      # allAgents <- rbind(allAgents, rd$agents)
      allAgents <- rbind(allAgents, rd$agents[rd$agents$generation%%50 == 1
                                              | (rd$agents$generation%%25 == 1 & rd$agents$generation < 250), ])
      #allDecisions <- rbind(allDecisions, rd$decisions[rd$decisions$generation %in% allAgents$generation, ])
    }
    #toSave <- list(allAgents, allDecisions)
    save(allAgents, file = paste(resultsPath, 'rawdata_subset.Rdata'))
    #rm('toSave')
    print('...saved subset...')
    
    # Plot
    ggplot(allAgents, 
           aes(x=generation, y=egoBias, 
               colour = manipulation)) +
      geom_hline(yintercept = 0.5, linetype = 'dashed') +
      stat_summary(geom = 'point', fun.y = mean, size = 3, alpha = 0.25) +
      stat_summary(fun.data = mean_cl_boot, fun.args=(conf.int = .99), geom = 'errorbar', size = 1) +
      scale_y_continuous(limits = c(0,1)) +
      facet_grid(~meanSensitivity, labeller = label_both) +
      labs(title = allAgents$description[1]) +
      style 
    ggsave(paste(resultsPath, 'graph.png'))
    
    print('...saved graph...')
    print('...data saved.')
  }
}

ggplot(allAgents[allAgents$meanSensitivity == 1 & allAgents$generation %% 1000 == 1, ], 
       aes(x = egoBias, y = sensitivity)) +
  geom_point() +
  facet_wrap(~generation, labeller = label_both) +
  # stat_summary(geom = 'point', fun.y = mean, size = 3) +
  style

# Cleanup
if(ARC)
  stopCluster(cl)
print('Complete.')

