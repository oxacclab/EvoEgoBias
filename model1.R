# Model 1 ####
# Agents have direct access to one another's confidence.
ARC <- !(Sys.info()[[1]] == 'Windows')

# Storage path for results
resultsPath <- ifelse(ARC,'/data/xpsy-acc/wolf5224/EvoEgoBias/','results/')
time <- format(Sys.time(), "%F_%H-%M-%S")
resultsPath <- paste0(resultsPath,time)

sink(paste(resultsPath, 'log.txt'))
Sys.info()

# Libraries
library(tidyverse)
library(parallel)

# Set up the parallel execution capabilities
nCores <- ifelse(ARC,detectCores(),detectCores() - 4)
cl <- makeCluster(nCores)

print(paste('Running in parallel on', nCores, 'cores.'))

# Parameter space to explore
degrees <- c(2)
sensitivitySDs <- c(0.0, 0.05, 0.1, 0.25, 1)
reps <- nCores

# Clear the result storage variables
suppressWarnings(rm('rawdata'))
suppressWarnings(rm('results'))

# Run the models
for(sensitivitySD in sensitivitySDs) {
  # make sure the children can see the degree variable
  clusterExport(cl, "sensitivitySD")
  # Run parallel repetitions of the model with these settings
  # degreeResults <- lapply(1:4, function(x) {
  startTime <- Sys.time()
  degreeResults <- parLapply(cl, 1:reps, function(x) {
    source('evoSim/evoSim/R/evoSim.R')
    data <- evoSim(agentCount = 200,
                   agentDegree = 50,
                   decisionCount = 100,
                   generationCount = 500,
                   mutationChance = 0.05,
                   other = list(sensitivitySD = sensitivitySD),
                   makeAgentFun = function(modelParams, parents = NULL) {
                     # Inherit egoBias if there's a previous generation and we're not mutating
                     if(!is.null(parents))
                       agent <- data.frame(egoBias = parents$egoBias[1],
                                           sensitivity = runif(1))
                     else
                       agent <- data.frame(egoBias = runif(1),
                                           sensitivity = runif(1))
                     agent$sensitivity <- rnorm(1, mean = 0.5, sd = modelParams$other$sensitivitySD)
                     if(runif(1) < modelParams$mutationChance)
                       agent$egoBias <- .5
                     return(agent)
                   },
                   selectParentsFun = function(modelParams, agents, world, ties) {
                     tmp <- agents[which(agents$generation == world$generation),]
                     tmp <- tmp[order(tmp$fitness, decreasing = T),]
                     # drop the worst half of the population
                     tmp <- tmp[1:(floor(nrow(tmp)/2)), ]
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
                     #print(cor(agents$egoBias, abs(agents$fitness-.5)))
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
                       out <- (agents$initialDecision[mask] * agents$egoBias[mask]) + ((1-agents$egoBias[mask]) * agents$advice[mask])
                       out[is.na(out)] <- agents$initialDecision[mask][is.na(out)]
                       agents$finalDecision[mask] <- out
                     }
                     return(agents)
                   })
    # save results
    n <- length(unique(data$agents$generation))
    results <- data.frame(repetition = rep(x, n),
                          # degree = rep(degree, n),
                          sensitivitySD = rep(sensitivitySD, n),
                          generation = unique(data$agents$generation), 
                          modelDuration = rep(data$duration, n))
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
    rawdata$rep <- x
    return(list(rawdata = rawdata, results = results))
  })
  for(res in degreeResults) {
    if(!exists('results'))
      results <- res$results
    else
      results <- rbind(results, res$results)
    if(!exists('rawdata'))
      rawdata <- res$rawdata
    else
      rawdata <- rbind(rawdata, res$rawdata)
  }
  print(paste0('Completed reps for SD = ', sensitivitySD,':'))
  Sys.time() = startTime
}

# Cleanup
stopCluster(cl)

# Save data
write.csv(results, paste(resultsPath, 'results.csv'))
write.csv(rawdata, paste(resultsPath, 'rawdata.csv'))

# Neat output graph
w <- 0.2
ggplot(results[results$generation%%10==1, ], aes(x = generation, y = egoBias, color = as.factor(sensitivitySD))) + 
  #geom_point(alpha = 0.5, position = position_dodge(w)) +
  stat_summary(geom = 'point', fun.y = mean, size = 3, shape = 18, position = position_dodge(w)) + 
  stat_summary(geom = 'errorbar', fun.data = mean_cl_boot, size = 1, width = 0.2, position = position_dodge(w)) +
  stat_summary(geom = 'line', fun.y = mean, position = position_dodge(w)) +
  theme_light() +
  scale_y_continuous(limits = c(0,1), expand = c(0,0))
ggsave(paste(resultsPath,'overview.png'))
# 
# ggplot(results, aes(x = initialDecision, color = as.factor(degree))) + geom_histogram(bins = 50)
# ggplot(results, aes(x = fitness, color = as.factor(degree))) + geom_histogram(bins = 50)
# ggplot(results, aes(x = finalDecision, color = as.factor(degree))) + geom_histogram(bins = 50)
# 
ggplot(results, aes(x = initialDecision, y = finalDecision, colour = as.factor(sensitivitySD))) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0) +
  coord_fixed()
ggsave(paste(resultsPath,'autocor.png'))
# 
# ggplot(results, aes(x = generation, y=modelDuration, colour = as.factor(degree))) +
#   stat_summary(geom = 'point', fun.y = mean, size = 3, shape = 18, position = position_dodge(w)) +
#   stat_summary(geom = 'errorbar', fun.data = mean_cl_boot, size = 1, width = 0.2, position = position_dodge(w)) +
#   stat_summary(geom = 'line', fun.y = mean, position = position_dodge(w)) +
#   theme_light()
