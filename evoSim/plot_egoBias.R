# Produce the EgoBias plot for various models

if(!require('tidyverse')) {
  install.packages('tidyverse')
  library(tidyverse)
}

style <- theme_light() +
  theme(legend.position = 'top',
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

load(file.choose())

ripTitle <- function(str) {substr(str, 1, regexpr(' (', str, fixed = T)[1]-1)}

# Plot
ggplot(allAgents, 
       aes(x=generation, y=egoBias, 
           colour = manipulation)) +
  geom_hline(yintercept = 0.5, linetype = 'dashed') +
  stat_summary(geom = 'point', fun.y = mean, size = 3, alpha = 0.25) +
  stat_summary(fun.data = mean_cl_boot, fun.args=(conf.int = .99), geom = 'errorbar', size = 1) +
  scale_y_continuous(limits = c(0,1)) +
  facet_wrap(~meanSensitivity, labeller = label_both) +
  labs(title = ripTitle(allAgents$description[1])) +
  style 

# Old
ggplot(allAgents[allAgents$startingEgoBias==.99, ], 
       aes(x=generation, y=egoBias, 
           colour = as.factor(badAdviceProb))) +
  geom_hline(yintercept = 0.5, linetype = 'dashed') +
  stat_summary(geom = 'point', fun.y = mean, size = 3, alpha = 0.25) +
  stat_summary(fun.data = mean_cl_boot, fun.args=(conf.int = .99), geom = 'errorbar', size = 1) +
  scale_y_continuous(limits = c(0,1)) +
  facet_wrap(~meanSensitivity, labeller = label_both) +
  style 
