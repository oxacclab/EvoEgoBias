# Model 2 ####
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

if(!require('ggplot2')) {
  install.packages(repos="http://cran.r-project.org",'ggplot2')
  library(ggplot2)
}

style <- theme_light() +
  theme(legend.position = 'top',
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

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
  setwd(spec$wd)
  source('evoSim/evoSim/R/evoSim.R')
  data <- evoSim(agentCount = spec$agents,
                 agentDegree = spec$degree,
                 decisionCount = spec$decisions,
                 generationCount = 1000,#2500,
                 mutationChance = 0.01,
                 other = list(sensitivity = spec$sensitivity, 
                              sensitivitySD = spec$sensitivitySD,
                              startingEgoBias = spec$startingEgoBias),
                 makeAgentFun = function(modelParams, parents = NULL) {
                   # Inherit egoBias if there's a previous generation and we're not mutating
                   if(!is.null(parents)) {
                     # print(paste0('id: ', parents$id,
                     #              '; egoBias: ', round(parents$egoBias,3),
                     #              '; fitness: ', round(parents$fitness,3)))
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
                   while(any(tmp$fitness) < 10)
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
                   #print(cor(agents$egoBias, abs(agents$fitness-.5)))
                   # print(summary(agents[agents$id %in% winners, c('egoBias', 'fitness')]))
                   # print(tmp[tmp$id %in% winners,])
                   print(paste('Selected parents for generation',world$generation))
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
                     out <- (agents$initialDecision[mask] * agents$egoBias[mask]) + 
                       ((1-agents$egoBias[mask]) * agents$advice[mask])
                     out[is.na(out)] <- agents$initialDecision[mask][is.na(out)]
                     agents$finalDecision[mask] <- out
                   }
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
for(x in c(1000)) {
  for(y in c(10)) {
    for(z in c(30)) {
      for(s in c(1,100)) {
        for(sSD in c(1, 100)) {
          for(sEB in c(0.01, 0.99))
          specs[[length(specs)+1]] <- list(agents=x,degree=y,decisions=z,
                                           sensitivity=s,sensitivitySD=sSD,
                                           startingEgoBias=sEB,
                                           wd=getwd())
        }
      }
    }
  }
}


# Run the models
# make sure the children can see the degree variable
# if(ARC)
#   clusterExport(cl, "sensitivitySD")

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
allAgents <- NULL
for(rd in rawdata) {
  rd$agents$agentCount <- rep(rd$model$agentCount,nrow(rd$agents))
  rd$agents$agentDegree <- rep(rd$model$agentDegree,nrow(rd$agents))
  rd$agents$decisionCount <- rep(rd$model$decisionCount,nrow(rd$agents))
  rd$agents$modelDuration <- rep(rd$duration,nrow(rd$agents))
  rd$agents$meanSensitivity <- rep(rd$model$other$sensitivity,nrow(rd$agents))
  rd$agents$sdSensitivity <- rep(rd$model$other$sensitivitySD,nrow(rd$agents))
  rd$agents$startingEgoBias <- rep(rd$model$other$startingEgoBias,nrow(rd$agents))
  # only take a subset because of memory limitations
  allAgents <- rbind(allAgents, rd$agents[rd$agents$generation%%5 == 1, ])
}
ggplot(allAgents, 
       aes(x=generation, y=egoBias)) +
  geom_hline(yintercept = 0.5, linetype = 'dashed') +
  stat_summary(geom = 'point', fun.y = mean, size = 3, alpha = 0.25) +
  stat_summary(fun.data = mean_cl_boot, fun.args=(conf.int = .99), geom = 'errorbar', size = 1) +
  scale_y_continuous(limits = c(0,1)) +
  facet_wrap(meanSensitivity ~ sdSensitivity, labeller = label_both) +
  labs(title = 'Model 5 (different sensitivities)') +
  style 
ggsave(paste(resultsPath, 'graph.png'))
print('...complete.')

