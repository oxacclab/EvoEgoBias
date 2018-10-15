library(ggplot2)
niceName <- function(x) {basename(x)}
decisionNames <- c('uncapped', 'capped', 'categorical')
adviceNames <- c('noisy advice', 'bad advice', 'noisy communication', 'different confidence')

# Load data
fileList <- choose.files(caption = 'Select model files', filters = Filters[c("RData"), ])
agents <- NULL
allAgents <- NULL
for(file in fileList) {
  load(file)
  fileName <- niceName(file)
  allAgents$file <- fileName
  allAgents$decisionType <- as.numeric(substr(fileName,2,2))
  allAgents$decisionName <- sapply(allAgents$decisionType, function(x) decisionNames[x])
  allAgents$adviceType <- as.numeric(substr(fileName,4,4))
  allAgents$adviceName <- sapply(allAgents$adviceType, function(x) adviceNames[x])
  if(allAgents$decisionType[1] == 3)
    agents <- rbind(agents, allAgents[allAgents$meanSensitivity == 1, ])
}

mean_range <- function(x) {data.frame(y = mean(x), ymin = range(x)[1], ymax = range(x)[2])}

# Plot data
gg <- list()
for(a in unique(agents$file))
  gg[[a]] <- 
  ggplot(agents[agents$file==a, ], 
         aes(x=generation, y=egoBias,
             fill = manipulation)) +
    geom_hline(yintercept = 0.5, linetype = 'dashed') +
    stat_summary(geom = 'ribbon', fun.data = mean_cl_boot, fun.args = (conf.int = .99), alpha = 1,
                 linetype = 0) +
    # stat_summary(geom = 'line', fun.y = mean, size = 1) +
    # stat_summary(geom = 'point', fun.y = mean, size = 3, alpha = 0.25) +
    # stat_summary(fun.data = mean_cl_boot, fun.args=(conf.int = .99), geom = 'errorbar', size = 1) +
    scale_y_continuous(limits = c(0,1), name = 'egocentric bias') +
  scale_fill_manual(name = agents$adviceName[agents$file==a][1], values = c('#4A8EF2', '#002147', '#5AA2AE')) +
    theme_light() +
    theme(legend.position = 'top',
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())

