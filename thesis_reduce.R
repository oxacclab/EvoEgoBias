library(tidyverse)

pattern <- "2021-09-[^.]+rawdata.Rdata"

# load data ---------------------------------------------------------------

d <- NULL

for (f in list.files("./results", pattern = pattern, full.names = T)) {
  
  if (f %in% unique(d$run))
    next()
  
  load(f)
  
  for (i in 1:length(rawdata)) {
    
    desc <- strsplit(rawdata[[i]]$model$other$shortDesc, " with ")
    
    tmp <- as_tibble(rawdata[[i]]$agents) %>%
      select(generation, selfWeight) %>%
      mutate(
        model = desc[[1]][1],
        scenario = desc[[1]][2],
        run = f,
        corruption = rawdata[[i]]$model$other$adviceNoise[
          rawdata[[i]]$model$other$manipulation
        ]
      ) %>%
      nest(d = selfWeight) %>%
      mutate(d = map(d, mean_cl_normal)) %>%
      unnest(d) %>%
      rename(selfWeight_mean = y) %>%
      rename_with(~ str_replace(., "^y(.+)", "selfWeight_ci95_\\1"))
    
    d <- rbind(d, tmp)
  }
}

# save because loading takes forever --------------------------------------

save(d, file = "thesis.Rdata")
