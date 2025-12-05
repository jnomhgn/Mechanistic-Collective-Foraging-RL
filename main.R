#### Setup #####

# Set path to project root if necessary
getwd() 

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

# Python integration setup
library(reticulate)
use_virtualenv(virtualenv = file.path(getwd(), "venv"), required = TRUE)

# Add seed for reproducibility
set.seed(42)


#### Data Pre-Processing ####

# Pre process data from raw data to formatted data for analyses
source("./preprocessing/preprocessing.R")

# Convert preprocessed data to numpy arrays.
source_python("preprocessing/numpy_conversion.py")

#### Run Behavioral Analyses ####
source("behavioral/code/behavioral.R")

#### Run Bayesian Agent Analysis ####
source("bayesianforager/code/BayesianForager.R")

# #### Alone condition ####

# Run model comparison
source("rl/code/alone/analyses/modelcomp.R")
rm(list = ls())

# Run parameter recovery
source("rl/code/alone/analyses/parrecov.R")
rm(list = ls())

# #### No catches condition ####

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

# Plot figures
source_python("figures/code/figures.py")
