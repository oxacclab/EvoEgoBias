
if(!require('tidyverse')) {
  install.packages('tidyverse')
  library(tidyverse)
}

load(file.choose())

style <- theme_light() +
  theme(legend.position = 'top')


# egoBias
ggplot(allAgents, 
       aes(x=generation, y=egoBias, 
           colour = as.factor(badAdviceProb), 
           shape = as.factor(meanSensitivity))) +
  geom_hline(yintercept = 0.5, linetype = 'dashed') +
  stat_summary(geom = 'point', fun.y = mean, size = 3, alpha = 0.5) +
  stat_summary(geom = 'errorbar', fun.data = function(x){data.frame(ymin=min(x), ymax=max(x))}, 
               size = 1, alpha = 0.25) +
  stat_summary(geom = 'errorbar', fun.data = mean_cl_boot, size = 0.25) +
  scale_y_continuous(limits = c(0,1)) +
  facet_grid(startingEgoBias ~ ., labeller = label_both) +
  style 

# fitness
prefix.m <- function(s) paste('m =',s)
prefix.sd <- function(s) paste('sd =',s)
ggplot(allAgents, 
       aes(x=generation, y=fitness/decisionCount,  
           colour = as.factor(badAdviceProb), 
           shape = as.factor(meanSensitivity))) +
  stat_summary(geom = 'point', fun.y = mean, size = 3, alpha = 0.5) +
  stat_summary(geom = 'errorbar', fun.data = mean_cl_boot, size = .5) +
  facet_grid(startingEgoBias ~ .) +
  style 

ggplot(allAgents, 
       aes(x="", y=modelDuration,  
           colour = as.factor(adviceNoise), 
           shape = as.factor(adviceNoise))) +
  geom_point(size = 3, position = position_dodge(width = 0.2)) +
  facet_wrap(~startingEgoBias) +
  style

ggplot(allAgents, aes(x = initialDecision, y = finalDecision,  
                      colour = as.factor(adviceNoise), 
                      shape = as.factor(adviceNoise))) +
  geom_point(alpha = 0.05) +
  geom_abline(slope = 1, intercept = 0) +
  coord_fixed() +
  # scale_y_continuous(limits = c(0,100)) +
  # scale_x_continuous(limits = c(0,100)) +
  style