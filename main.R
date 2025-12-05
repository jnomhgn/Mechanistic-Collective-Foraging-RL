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
library(parallel)

# Python integration setup
library(reticulate)
use_virtualenv(virtualenv = file.path(getwd(), "venv"), required = TRUE)

# Add seed for reproducibility
set.seed(42)


#### Data Pre-Processing ####

# Pre process data from raw data to formatted data for analyses
print("Preprocessing data...")
source("./preprocessing/preprocessing.R")

# Convert preprocessed data to numpy arrays.
print("Converting data to numpy arrays...")
source_python("preprocessing/numpy_conversion.py")

#### Run Behavioral Analyses ####
print("Running behavioral analyses...")
source("behavioral/code/behavioral.R")

#### Run Bayesian Agent Analysis ####
print("Running Bayesian Forager analyses...")
source("bayesianforager/code/BayesianForager.R")

# #### Alone condition ####

# Run model comparison
print("Running model comparison for alone condition...")
source("rl/code/alone/analyses/modelcomp.R")
rm(list = ls())

# Run parameter recovery
print("Running parameter recovery for alone condition...")
source("rl/code/alone/analyses/parrecov.R")
rm(list = ls())

# #### No catches condition ####

# Run numerical simulations
print("Running numerical simulations for no catches condition...")
source("rl/code/nocatches/analyses/numsims.R")
rm(list = ls())

# Run model comparison
print("Running model comparison for no catches condition...")
source("rl/code/nocatches/analyses/modelcomp.R")
rm(list = ls())


#### Catches condition ####

# Run numerical simulations
print("Running numerical simulations for catches condition...")
source("rl/code/catches/analyses/numsims.R")
rm(list = ls())

# Run model comparison
print("Running model comparison for catches condition...")
source("rl/code/catches/analyses/modelcomp.R")
rm(list = ls())

# Run parameter recovery
print("Running parameter recovery for catches condition...")
source("rl/code/catches/analyses/parrecov.R")
rm(list = ls())

#### Create figures ####

# Plot figures
print("Creating figures...")
source_python("figures/code/figures.py")
