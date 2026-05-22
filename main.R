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
library(reshape2)
library(splines)

library(ggplot2)
library(ggpubr)
library(viridis)
library(ggdist)

library(rstan)
library(brms)
library(loo)
library(tidybayes)
library(posterior)
library(bayesplot)
library(randtoolbox)
library(parallel)
library(future)
library(future.apply)
options(future.globals.maxSize = 1.0 * 1e9)

# Pipeline configuration
source(file.path("code", "pipeline_config.R"))
pipeline_mode <- "test" # "full" or "test"
options(
	pipeline_mode = pipeline_mode,
	pipeline_config = make_pipeline_config(pipeline_mode)
)

# Python integration setup
library(reticulate)
use_virtualenv(virtualenv = file.path(getwd(), "venv"), required = TRUE)

# Add seed for reproducibility
set.seed(42)


#### Data Pre-Processing ####

# Pre process data from raw data to formatted data for analyses
print("Preprocessing data...")
source(file.path("code", "preprocessing", "preprocessing.R"))

# Convert preprocessed data to numpy arrays.
print("Converting data to numpy arrays...")
source_python(file.path("code", "preprocessing", "numpy_conversion.py"))

#### Run Behavioral Analyses ####
print("Running behavioral analyses...")
source(file.path("code", "behavioral", "behavioral.R"))

#### Run Bayesian Agent Analysis ####
print("Running Bayesian Forager analyses...")
source(file.path("code", "bayesianforager", "BayesianForager.R"))

#### Alone condition ####

# Run model comparison
print("Running model comparison for alone condition...")
source(file.path("code", "rl", "alone", "analyses", "modelcomp.R"))
rm(list = ls())

# Run parameter recovery
print("Running parameter recovery for alone condition...")
source(file.path("code", "rl", "alone", "analyses", "parrecov.R"))
rm(list = ls())

#### No catches condition ####

# Run numerical simulations
print("Running numerical simulations for no catches condition...")
source(file.path("code", "rl", "nocatches", "analyses", "numsims.R"))
rm(list = ls())

# Run model comparison
print("Running model comparison for no catches condition...")
source(file.path("code", "rl", "nocatches", "analyses", "modelcomp.R"))
rm(list = ls())


#### Catches condition ####

# Run numerical simulations
print("Running numerical simulations for catches condition...")
source(file.path("code", "rl", "catches", "analyses", "numsims.R"))
rm(list = ls())

# Run model comparison
print("Running model comparison for catches condition...")
source(file.path("code", "rl", "catches", "analyses", "modelcomp.R"))
rm(list = ls())

# Run parameter recovery
print("Running parameter recovery for catches condition...")
source(file.path("code", "rl", "catches", "analyses", "parrecov.R"))
rm(list = ls())

# #### Create figures ####

# # Plot figures
# print("Creating figures...")
# source_python(file.path("figures", "code", "figures.py"))

#### Run model recovery for all conditions at the end due to computation time ####

# Alone condition
print("Running model recovery for alone condition...")
source(file.path("code", "rl", "alone", "analyses", "modelrecov.R"))
rm(list = ls())

# No catches condition
print("Running model recovery for no catches condition...")
source(file.path("code", "rl", "nocatches", "analyses", "modelrecov.R"))
rm(list = ls())

# Catches condition
print("Running model recovery for catches condition...")
source(file.path("code", "rl", "catches", "analyses", "modelrecov.R"))
rm(list = ls())