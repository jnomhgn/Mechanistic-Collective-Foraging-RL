### Setup ####
# Set path
setwd("/mnt/home/marienhagen/Users/MarienhagenJonathan/rlforaging")


# Load libraries
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
library(ggdist)

library(rstan)
library(loo)
library(tidybayes)
library(bayesplot)
library(randtoolbox)

#### Alone condition ####

# Run model comparison
source("rl/code/alone/analyses/modelcomp.R")
rm(list = ls())

# Run parameter recovery
source("rl/code/alone/analyses/parrecov.R")
rm(list = ls())

#### No catches condition ####

# Run numerical simulations
source("rl/code/nocatches/analyses/numsims.R")
rm(list = ls())

# Run model comparison
source("rl/code/nocatches/analyses/modelcomp.R")
rm(list = ls())


#### Catches condition ####

# Run numerical simulations
source("rl/code/catches/analyses/numsims.R")
rm(list = ls())

# Run model comparison
source("rl/code/catches/analyses/modelcomp.R")
rm(list = ls())

# Run parameter recovery
source("rl/code/catches/analyses/parrecov.R")
rm(list = ls())

#### Create figures ####

# Set path to virtual environment
use_condaenv("/path/to/anaconda/venvironment")

# Plot figures
source_python("rl/code/figures/RLFit.py")
source_python("rl/code/figures/RLSimu.py")
source_python("rl/code/figures/RLsimuSupp.py")
