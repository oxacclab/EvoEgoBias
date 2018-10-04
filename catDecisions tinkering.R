library(ggplot2)
ggplot(decisions[decisions$genId==15 & decisions$generation==2,], 
       aes(x=initialDecision, y=advice, colour=as.factor(decision))) + 
  geom_point(alpha = 0.3) +
  #stat_summary(geom='point', fun.y = mean, alpha = 0.4) +
  NULL
  
hist(decisions$advice, breaks = 100)
hist(decisions$initialDecision, breaks = 100)

d <- decisions[decisions$generation == 51, ]

ggplot(d) + geom_histogram(aes(x=advice), fill='blue') + geom_histogram(aes(x=initialDecision), fill='red')
