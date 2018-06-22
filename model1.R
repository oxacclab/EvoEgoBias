# Model 1 ####

# Agents have direct access to one another's confidence.
library(tidyverse)
library(parallel)
#library(evoSim)

# Storage path for results
resultsPath <- ''

# Set up the parallel execution capabilities
nCores <- detectCores()
nCores <- nCores - 2
cl <- makeCluster(nCores)

# Parameter space to explore
degrees <- c(1, 50, 150)
reps <- nCores * 2

# Clear the result storage variables
suppressWarnings(rm('rawdata'))
suppressWarnings(rm('results'))

# Run the models
for(degree in degrees) {
  # make sure the children can see the degree variable
  clusterExport(cl, "degree")
  # Run parallel repetitions of the model with these settings
  degreeResults <- parLapply(cl, 1:reps, function(x) {
    library(evoSim)
    data <- evoSim(agentCount = 200,
                   agentDegree = degree,
                   decisionCount = 100,
                   generationCount = 250,
                   mutationChance = 0.001)
    # save results
    n <- length(unique(data$agents$generation))
    results <- data.frame(degree = rep(degree, n),
                          repetition = rep(x, n),
                          generation = unique(data$agents$generation))
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
}

# Cleanup
stopCluster(cl)

# Save data
write.csv(results, paste0(resultsPath, 'results.csv'))
write.csv(rawdata, paste0(resultsPath, 'rawdata.csv'))

# Neat output graph
w <- 0.2
ggplot(results, aes(x = generation, y = egoBias, color = as.factor(degree))) + 
  #geom_point(alpha = 0.5, position = position_dodge(w)) +
  stat_summary(geom = 'point', fun.y = mean, size = 3, shape = 18, position = position_dodge(w)) + 
  stat_summary(geom = 'errorbar', fun.data = mean_cl_boot, size = 1, width = 0.2, position = position_dodge(w)) +
  stat_summary(geom = 'line', fun.y = mean, position = position_dodge(w)) +
  theme_light()
ggsave('overview.png')

ggplot(results, aes(x = initialDecision, color = as.factor(degree))) + geom_histogram(bins = 50)
ggplot(results, aes(x = fitness, color = as.factor(degree))) + geom_histogram(bins = 50)
ggplot(results, aes(x = finalDecision, color = as.factor(degree))) + geom_histogram(bins = 50)

ggplot(results[results$generation==1,], aes(x = initialDecision, y = finalDecision, colour = as.factor(degree))) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0) +
  coord_fixed()

# show <- data.frame(generation=integer(), duration, degree)
# for(i in rawdata$model)
#   show <- rbind(show, data.frame(generation = i$generationCount, duration = i$duration, degree = i$agentDegree))
# ggplot(rawdata$model, aes(x = generation, y=duration, colour = degree)) +
#   stat_summary(geom = 'point', fun.y = mean, size = 3, shape = 18, position = position_dodge(w)) + 
#   stat_summary(geom = 'errorbar', fun.data = mean_cl_boot, size = 1, width = 0.2, position = position_dodge(w)) +
#   stat_summary(geom = 'line', fun.y = mean, position = position_dodge(w)) +
#   theme_light()
