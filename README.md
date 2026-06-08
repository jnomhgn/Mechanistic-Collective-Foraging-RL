# Mechanistic Collective Foraging
This repository contains the full data and scripts to reproduce all analyses and figures in:

**Marienhagen, J, Blum Moyse, L., Schakowski, A., Kahl, B., Davidson, J., El Hady, A., Kurvers, R. & Deffner, D. (2025). *Bridging drift-diffusion and reinforcement-learning modeling to uncover the cognitive processes underlying collective foraging.* [Manuscript submitted for publication].**

Previous versions of the code are available under [Releases](https://github.com/jnomhgn/Mechanistic-Collective-Foraging-DDM-RLM/releases).

## Code Execution

### With Docker

Build the image from the project root and run the container. The container runs `Rscript code/main.R` by default and expects `data/` and `results/` to be mounted.

**Build:**
```
docker build -t rlforaging .
```

**Run:**
```
docker run --rm -v "${PWD}/data:/rlforaging/data" -v "${PWD}/results:/rlforaging/results" rlforaging
```

### Without Docker

Prerequisites: Python 3.13.9 and R 4.5.2.

**Set up Python:**
```
python3 -m venv venv
source venv/bin/activate
pip install --upgrade pip
pip install -r requirements.txt
```

**Set up R:**
```
R -e "install.packages('renv', repos = 'https://cran.rstudio.com')"
R -s -e "renv::init(bare=TRUE)"
R -e 'install.packages(c("RcppParallel", "RcppEigen"))'
R -e 'install.packages("StanHeaders")'
R -s -e "renv::restore()"
R -e 'cmdstanr::install_cmdstan()'
```

**Run the analyses:**
```
Rscript code/main.R
```

## Code Overview

"code/main.R" is the entry point for the full analysis pipeline. It runs preprocessing, behavioral analyses, Bayesian forager simulations, all RL analyses across the three social-information conditions, and figure generation.

### Data Processing
The "code/preprocessing" subdirectory contains the preprocessing scripts.

- "preprocessing.R" reads in the raw data collected during the experiment and creates a discrete time series with a 1s resolution.

- "numpy_conversion.py" converts the processed data from CSV to numpy arrays.

### Behavioral Analyses
"code/behavioral/behavioral.R" runs the Bayesian logistic regression predicting players’ probability to forage at the better lake while accounting for individual- and group-level heterogeneity.

### Bayesian Foragers
"code/bayesianforager/BayesianForager.R" runs the simulations for the optional Bayesian foragers and plots the results.

### Reinforcement Learning

#### Analyses

Each condition has an "analyses" subdirectory under "code/rl/\<condition\>/analyses" containing a subset of the following scripts:

- "numsims.R" runs the numerical simulations.

- "modelcomp.R" runs the model comparison and computes the posterior predictions implied by the winning model.

- "modelrecov.R" runs a model recovery analysis.

- "parrecov.R" runs a parameter recovery analysis for the winning model.

#### Stan Files

Each condition has a "stan" subdirectory under "code/rl/\<condition\>/stan". Across all conditions, "fixed" and "hierarch" denote whether the model is implemented as a fixed- or varying-effects model, and "ll" denotes that the stan code computes the log-likelihood in the "generated quantities" section.

##### "Alone" Condition

- "m1.1" denotes the model w/o asymmetric personal learning weights and w/o a persistence parameter.

- "m2.1" denotes the model w asymmetric personal learning weights and w/o a persistence parameter.

- "m3.1" denotes the model w/o asymmetric personal learning weights and w a persistence parameter.

- "m4.1" denotes the model w asymmetric personal learning weights and w a persistence parameter.

- "m4.2" denotes the model equivalent to m4.1 but w maximum-catch-dependent asymmetric personal learning weights.

- "m4.3" denotes the model equivalent to m4.1 but w catch-ratio-and-maximum-catch-dependent asymmetric personal learning weight.

##### "No Catches" Condition

- "arl" denotes the ARL model m4.1.

- "dbn" denotes the SRL model integrating location-related social information via DB.

- "vsn" denotes the SRL model integrating location-related social information via VS.

- "1" and "2" denote the non-adaptive and adaptive variants of the SRL models that do and do not allow social learning weights to vary between environments respectively.

##### "Catches" Condition

- "arl" denotes the ARL model m4.1.

- "dbn" denotes the SRL model integrating location-related social information via DB.

- "vsn" denotes the SRL model integrating location-related social information via VS.

- "dbr" denotes the SRL model integrating catch-related social information via DB.

- "vsr" denotes the SRL model integrating catch-related social information via VS.

- "dbnvsr" denotes the SRL integrating location-related social information via DB and catch-related social information via VS.

- "dbndbr" denotes the SRL integrating both location- and catch-related social information via DB.

- "vsndbr" denotes the SRL integrating location-related social information via VS and catch-related social information via DB.

- "vsnvsr" denotes the SRL integrating both location- and catch-related social information via VS.

- "1" and "2" denote the non-adaptive and adaptive variants of the SRL models that do and do not allow social learning weights to vary between environments respectively.

#### Functions

Each condition has a "functions" subdirectory under "code/rl/\<condition\>/functions" containing the following scripts:

- "fixed.sim.R" files follow the same naming convention as the stan files and simulate synthetic data from the respective model.

- "getmodels.R" is sourced during analysis and provides information with regards to the parameters contained in each model as well as the paths to the corresponding simulation functions and stan model code.

#### Figures

The "code/figures" subdirectory contains the figure scripts.

- "figures.py" plots the RL model fits (Figs 3, 5 and 7) and the numerical simulation results (Figs 2, 4 and 6).
