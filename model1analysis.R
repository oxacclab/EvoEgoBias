
if(!require('tidyverse')) {
  install.packages('tidyverse')
  library(tidyverse)
}

style <- theme_light() +
  theme(legend.position = 'top')

## Collate the data into a neat results dataframe
r <- data.frame(agentCount=integer(),
                agentDegree=integer(),
                decisionCount=integer(),
                modelDuration=double(),
                generation = integer(),
                fitness = double(), 
                degree = double(), 
                sensitivity = double(), 
                egoBias = double(), 
                initialDecision = double(), 
                finalDecision = double(),
                egoBias.sd = double())
allAgents <- NULL
for(d in degreeResults) {
  rd <- d$rawdata
  rd$agents$agentCount <- rep(rd$model$agentCount,nrow(rd$agents))
  rd$agents$agentDegree <- rep(rd$model$agentDegree,nrow(rd$agents))
  rd$agents$decisionCount <- rep(rd$model$decisionCount,nrow(rd$agents))
  rd$agents$modelDuration <- rep(rd$duration,nrow(rd$agents))
  allAgents <- rbind(allAgents, rd$agents)
  # tmp <- data.frame(agentCount = rd$model$agentCount,
  #                   agentDegree= rd$model$agentDegree,
  #                   decisionCount = rd$model$decisionCount,
  #                   modelDuration = rd$duration)
  # tmp <- cbind(tmp, aggregate(rd$agents, 
  #                             list(rd$agents$generation), 
  #                             mean)[ ,c('generation',
  #                                       'fitness', 
  #                                       'degree', 
  #                                       'sensitivity', 
  #                                       'egoBias', 
  #                                       'initialDecision', 
  #                                       'finalDecision')])
  # tmp <- cbind(tmp, aggregate(rd$agents, list(rd$agents$generation),
  #                             sd)[ ,c('egoBias')])
  # names(tmp)[ncol(tmp)] <- 'egoBias.sd'
  # r <- rbind(r, tmp)
}

# tmp <- r[r$generation%%50==1, ]
# tmp <- r[r$generation%%50==1 & r$agentCount!=10 & r$agentDegree!=10 & r$decisionCount!=10,]
# tmp <- r[r$generation%%50==1 & r$modelDuration < 1000,]

tmp <- allAgents[allAgents$generation%%50 == 1
                 | (allAgents$generation%%25 ==1 & allAgents$generation < 250), ]

# ggplot(tmp, aes(x=as.factor(generation), y=egoBias, color = as.factor(agentCount), 
#                 linetype=as.factor(agentDegree), shape=as.factor(decisionCount))) +
#   geom_boxplot()

w <- 1

# egoBias
ggplot(tmp, 
       aes(x=generation, y=egoBias, 
           colour = as.factor(agentCount), 
           linetype = as.factor(agentDegree), 
           shape = as.factor(decisionCount))) +
  geom_hline(yintercept = 0.5, linetype = 'dashed') +
  stat_summary(geom = 'point', fun.y = mean, size = 3) +
  stat_summary(geom = 'errorbar', fun.data = function(x){data.frame(ymin=min(x), ymax=max(x))}, 
               size = 1, alpha = 0.25) +
  stat_summary(geom = 'errorbar', fun.data = mean_cl_boot, size = 0.25) +
  scale_y_continuous(limits = c(0,1)) +
  style 

# fitness
ggplot(tmp, 
       aes(x=generation, y=fitness/decisionCount, 
           colour = as.factor(agentCount), 
           linetype = as.factor(agentDegree), 
           shape = as.factor(decisionCount))) +
  stat_summary(geom = 'point', fun.y = mean, size = 3) +
  stat_summary(geom = 'errorbar', fun.data = mean_cl_boot, size = .5) +
  style 

ggplot(tmp, 
       aes(x="", y=modelDuration, 
           colour = as.factor(agentCount), 
           shape = as.factor(decisionCount))) +
  geom_point(size = 3, position = position_dodge(width = 0.2)) +
  facet_wrap(~agentDegree) +
  style
  
ggplot(tmp, aes(x = initialDecision, y = finalDecision,  
                colour = as.factor(agentCount), 
                shape = as.factor(decisionCount))) +
  geom_point(alpha = 0.05) +
  geom_abline(slope = 1, intercept = 0) +
  coord_fixed() +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(0,100)) +
  style

# Neat output graph
tmp <- rawdata[[1]]$agents
# ggplot(results, aes(x = generation, y = egoBias, color = as.factor(sensitivitySD))) +
#   #geom_point(alpha = 0.5, position = position_dodge(w)) +
#   stat_summary(geom = 'point', fun.y = mean, size = 3, shape = 18, position = position_dodge(w)) +
#   stat_summary(geom = 'errorbar', fun.data = mean_cl_boot, size = 1, width = 0.2, position = position_dodge(w)) +
#   stat_summary(geom = 'line', fun.y = mean, position = position_dodge(w)) +
#   theme_light() +
#   scale_y_continuous(limits = c(0,1), expand = c(0,0))
# ggsave(paste(resultsPath,'overview.png'))
if(max(tmp$generation) > 100) {
  ggplot(tmp, aes(x = generation, y = 1/abs(fitness))) + 
    #geom_boxplot(aes(group = generation), fill='lightblue', color='blue', alpha = 0.25) +
    #geom_boxplot(aes(group = generation, y = egoBias), fill='pink', color='red', alpha = 0.25) +
    stat_summary(geom = 'errorbar', fun.data = mean_cl_boot, color = 'lightblue') +
    stat_summary(geom = 'line', fun.y = mean, color='blue') +
    stat_summary(aes(y=egoBias), geom = 'errorbar', fun.data = mean_cl_boot, color = 'pink') +
    stat_summary(aes(y=egoBias), geom = 'line', fun.y = mean, color='red', size = 1) +
    stat_summary(aes(y=sensitivity), geom = 'point', fun.y = mean, color='black', alpha=0.5) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    theme_light()  
} else {
  ggplot(tmp, aes(x = generation, y = 1/abs(fitness))) + 
    geom_boxplot(aes(group = generation), fill='lightblue', color='blue', alpha = 0.25) +
    geom_boxplot(aes(group = generation, y = egoBias), fill='pink', color='red', alpha = 0.25) +
    stat_summary(geom = 'line', fun.y = mean, color='blue') +
    stat_summary(aes(y=egoBias), geom = 'line', fun.y = mean, color='red') +
    stat_summary(aes(y=sensitivity), geom = 'line', fun.y = mean, color='black', linetype='dashed') +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    theme_light()
}
# 
# ggplot(tmp, aes(x = generation, y = egoBias)) +
#   #geom_point(alpha = 0.5, position = position_dodge(w)) +
#   # stat_summary(geom = 'point', fun.y = mean, size = 3, shape = 18, position = position_dodge(w)) +
#   # stat_summary(geom = 'errorbar', fun.data = mean_cl_boot, size = 1, width = 0.2, position = position_dodge(w)) +
#   # stat_summary(geom = 'line', fun.y = mean, position = position_dodge(w)) +
#   geom_boxplot(aes(group = generation)) +
#   geom_smooth(aes(y=sd(egoBias)), method='loess') +
#   # stat_summary(geom = 'point', color = 'red', fun.y = min) + 
#   # stat_summary(geom = 'point', color = 'blue', fun.y = max) +
#   theme_light()

ag <- aggregate(tmp,list(tmp$generation),sd)
cor(ag$egoBias, ag$sensitivity)

# 
# ggplot(results, aes(x = initialDecision, color = as.factor(degree))) + geom_histogram(bins = 50)
# ggplot(results, aes(x = fitness, color = as.factor(degree))) + geom_histogram(bins = 50)
# ggplot(results, aes(x = finalDecision, color = as.factor(degree))) + geom_histogram(bins = 50)
# 
# ggplot(results, aes(x = initialDecision, y = finalDecision, colour = generation)) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0) +
#   coord_fixed()
# ggsave(paste(resultsPath,'autocor.png'))
# 
# ggplot(results, aes(x = generation, y=modelDuration, colour = as.factor(degree))) +
#   stat_summary(geom = 'point', fun.y = mean, size = 3, shape = 18, position = position_dodge(w)) +
#   stat_summary(geom = 'errorbar', fun.data = mean_cl_boot, size = 1, width = 0.2, position = position_dodge(w)) +
#   stat_summary(geom = 'line', fun.y = mean, position = position_dodge(w)) +
#   theme_light()
# 
# ggplot(rawdata[[1]]$agents, aes(x=sensitivity, y=fitness)) +
#   geom_point(alpha = 0.15) +
#   geom_smooth(method='lm',formula=y~x) +
#   geom_smooth(method = 'lm', aes(x=initialDecision), formula=y~x, colour = 'red') +
#   geom_smooth(method = 'lm', aes(x=finalDecision), formula=y~x, colour = 'green') +
#   geom_smooth(aes(x=egoBias), formula=y~x, colour = 'purple')

# ggplot(tmp, aes(x=egoBias, y=fitness)) +
#   geom_point() +
#   geom_smooth(formula=y~x) +
#   facet_wrap(~generation)