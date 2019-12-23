library(tidyverse)

# load data ---------------------------------------------------------------

d <- NULL

for (f in list.files("./results", full.names = T)) {
  if (!length(grep("2019-06-1[56]_[^.]+rawdata.Rdata", f)))
    next()
  
  if (f %in% unique(d$run))
    next()
  
  load(f)
  
  for (i in 1:length(rawdata)) {
    tmp <- as.tibble(rawdata[[i]]$agents[seq(1, nrow(rawdata[[i]]$agents) + 1, 2), 
                                         c("generation", "selfWeight")])
    
    desc <- strsplit(rawdata[[i]]$model$other$shortDesc, " with ")
    tmp$model <- desc[[1]][1]
    tmp$scenario <- desc[[1]][2]
    
    tmp$run <- f
    
    tmp$corruption <- rawdata[[i]]$model$other$adviceNoise[
      rawdata[[i]]$model$other$manipulation]
    
    d <- rbind(d, tmp)
  }
}


# save because loading takes forever --------------------------------------

save(d, file = "aeginaData.Rdata")

# load("aeginaData.Rdata")

d <- d[complete.cases(d), ]

d$corruption <- factor(d$corruption)
d$model <- factor(d$model)
d$run <- as.numeric(factor(d$run))


# plot --------------------------------------------------------------------

# tweaks for display reasons
# d$scenario[d$scenario == "noisy communication"] <- "noisy\ncommunication"

ggplot(d, aes(x = generation, y = selfWeight, 
              colour = corruption, fill = corruption)) + 
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal) +
  # stat_summary(geom = "line", fun.y = mean, size = 2, alpha = .75) +
  facet_grid(scenario~.) +
  scale_y_continuous(limits = c(.45, 1), expand = c(0,0)) +
  scale_x_continuous(limits = c(0, 50)) +
  scale_color_brewer(palette = "Dark2", type = "qual") + 
  scale_fill_brewer(palette = "Dark2", type = "qual") +
  theme_light() +
  guides(fill = guide_legend(ncol = length(unique(d$corruption)),
                               label.position = "top",
                               keywidth = 2,
                               title = "corruption:    "),
         colour = "none") +
  labs(y = "population mean self weight") +
  theme(legend.position = 'top',
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 28),
        strip.background = element_rect(fill = "#002148"),
        axis.title = element_text(size = 50), 
        axis.title.x = element_text(margin = margin(b = 4)),
        axis.title.y = element_text(margin = margin(r = 10)))

ggsave("aeginaGraph.png", width = 16, height = 12, dpi = 1200)
