library(knitr)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(readr)
library(stringr)

library(ggplot2)
library(ggpubr)
library(viridis)

library(rstan)
library(loo)
library(tidybayes)
library(bayesplot)
library(randtoolbox)

# Set wd
setwd("~/Users/MarienhagenJonathan/rlforaging")

## Experimental data

# Read data
path = "~/Users/MarienhagenJonathan/thesisvc/data/data_discrete_1s.csv"
d = read.csv(path,colClasses = c(rep(NA, 8), rep("character", 2), rep(NA, 4)))

# Set player id unique across sessions
d = d %>% mutate(id = (session - 1) * 5  + player) %>% select(-player)

# Get solo trials
d = d %>% filter(social.fac == 1)

# Transform
d=d %>%
  mutate(decision = correct) %>% mutate(reward = catch) %>% mutate(nplayers = 5) %>%
  mutate(decision = decision + 1) # Transform [0, 1] to [1, 2]

# Put experimental data in list
stan.data.d = with(d, list(
  OBSERVATIONS=nrow(d),
  SESSIONS=max(unique(session)), session=session,
  TRIALS=max(unique(trial)), trial=trial,
  MAXIMUM=length(unique(max.fac)), maximum=max.fac,
  RATIO=length(unique(ratio.fac)), ratio=ratio.fac,
  PLAYERS=unique(nplayers),
  ID=max(id), id=id,
  TIMES=max(unique(time.rounded)), time=time.rounded,
  DECISIONS=length(unique(decision)), decision=decision, 
  REWARDS=length(unique(reward)), reward=reward
))

# Load 
diagnostics = read.csv("~/Users/MarienhagenJonathan/rlforaging/asocial/results/modelcomp/diagnostics/m4.1.diagnostics.csv",
                       row.names = 1)

# 
id = diagnostics %>% 
  mutate(loglik_indx = rownames(.)) %>%
  mutate(loglik_indx = ifelse(grepl("log_lik", loglik_indx), as.numeric(str_extract(loglik_indx, "\\d+" )), NA)) %>%
  mutate(session = stan.data.d$session[.$loglik_indx]) %>%
  mutate(trial = stan.data.d$trial[.$loglik_indx]) %>%
  mutate(id = stan.data.d$id[.$loglik_indx])



# Filter out loglik
id = id[!grepl(pattern = "log_lik", rownames(id)), ] 




