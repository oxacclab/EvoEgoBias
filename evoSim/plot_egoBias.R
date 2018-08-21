# Produce the EgoBias plot for various models

if(!require('tidyverse')) {
  install.packages('tidyverse')
  library(tidyverse)
}

style <- theme_light() +
  theme(legend.position = 'top')


# Model 7, 8
ggplot(allAgents[allAgents$startingEgoBias==.99, ], 
       aes(x=generation, y=egoBias, 
           colour = as.factor(adviceNoise))) +
  geom_hline(yintercept = 0.5, linetype = 'dashed') +
  stat_summary(geom = 'point', fun.y = mean, size = 3, alpha = 0.25) +
  stat_summary(fun.data = mean_cl_boot, fun.args=(conf.int = .99), geom = 'errorbar', size = 1) +
  scale_y_continuous(limits = c(0,1)) +
  facet_wrap(~meanSensitivity, labeller = label_both) +
  style 

# Model 9
ggplot(allAgents, 
       aes(x=generation, y=egoBias, 
           colour = as.factor(badAdviceProb), 
           shape = as.factor(meanSensitivity))) +
  geom_hline(yintercept = 0.5, linetype = 'dashed') +
  stat_summary(geom = 'point', fun.y = mean, size = 3, alpha = 0.25) +
  stat_summary(fun.data = mean_cl_boot, fun.args=(conf.int = .99), geom = 'errorbar', size = 1) +
  scale_y_continuous(limits = c(0,1)) +
  facet_wrap(~meanSensitivity, labeller = label_both) +
  style 