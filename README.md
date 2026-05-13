# Mechanistic Collective Foraging
This repository contains the full data and scripts to reproduce all analyses and figures in:

**Marienhagen, J, Blum Moyse, L., Schakowski, A., Kahl, B., Davidson, J., El Hady, A., Kurvers, R. & Deffner, D. (2025). *Bridging drift-diffusion and reinforcement-learning modeling to uncover the cognitive processes underlying collective foraging.* [Manuscript submitted for publication].**

## Code Execution

### With Docker

To run the code via Docker, run the following commands from the project root directory to build the Docker image and run the Docker container

```
docker build -t rlforaging .
docker run --rm `
  -v "${PWD}\data:/rlforaging/data" `
  -v "${PWD}\results:/rlforaging/results" `
  rlforaging
```

## Code Overview

## Data Processing
"preprocessing.R" reads in the raw data collected during the experiment and creates a discrete time series with a 1s resolution.

## Behavioral Analyses
"behavioral.R" runs the Bayesian logistic regression predicting players’ probability to forage at the better lake while accounting for individual- and group-level heterogeneity.

## Bayesian Foragers
"BayesianForager.R" runs the simulations for the optional Bayesian foragers and calls the "Bayesian.py" script which creates the plot.

## Reinforcement Learning
"main.R" sources the analyses conducted for each social-information condition.

### "Alone" Condition

***Analyses*** The "code/alone/analyses" subdirectory contains the analyses for the "alone" condition.

- "modelcomp.R" runs the model comparison, computes the posterior predictions implied by the chosen model m4.1 and plots the results.

- "parrecov.R" runs a parameter recovery analysis for m4.1.

***Stan Files*** The "code/alone/stan" subdirectory contains the stan code for the ARL models.

- "m1.1" denotes the model w/o asymmetric personal learning weights and w/o a persistence parameter.

- "m2.1" denotes the model w asymmetric personal learning weights and w/o a persistence parameter.

- "m3.1" denotes the model w/o asymmetric personal learning weights and w a persistence parameter.

- "m4.1" denotes the model w asymmetric personal learning weights and w a persistence parameter.

- "m4.2" denotes the model equivalent to m4.1 but w maximum-catch-dependent asymmetric personal learning weights.

- "m4.3" denotes the model equivalent to m4.1 but w catch-ratio-and-maximum-catch-dependent asymmetric personal learning weight.

- "fixed" and "hierarch" denote whether the model is implemented as a fixed- or varying-effects model.

- "ll" denotes that the stan code computes the log-likelihood in the "generated quantities" section.

***Functions*** The "code/alone/functions" subdirectory contains the functions used during the analysis. 

- "fixed.sim.R" files follow the same naming convention as the stan files mentioned above and simulate synthetic data from the respective model.

- "getmodels.R" is sourced during analysis and provides information with regards to the parameters contained in each model as well as the paths to the corresponding simulation functions and stan model code.

### "No Catches" Condition

***Analyses*** The "code/nocatches/analyses" subdirectory contains the analyses for the "no catches" condition.

- "numsims.R" runs the numerical simulations and plots the results.

- "modelcomp.R" runs the model comparison, computes the posterior predictions implied by the winning model and plots the results.

- "parrecov.R" runs a parameter recovery analysis for the winning model.

***Stan Files*** The "code/nocatches/stan" subdirectory contains the stan code for the SRL-NC models.

- "arl" denotes the ARL model m4.1.

- "dbn" denotes the SRL model integrating location-related social information via DB.

- "vsn" denotes the SRL model integrating location-related social information via VS.

- "fixed" and "hierarch" denote whether the model is implemented as a fixed- or varying-effects model.

- "ll" denotes that the stan code computes the log-likelihood in the "generated quantities" section.

- "2" and "1" denote the adaptive and non-adaptive variants of the SRL models that do and do not allow social learning weights to vary between environments respectively.

***Functions*** The "code/nocatches/functions" subdirectory contains the functions used during the analysis. 

- "fixed.sim.R" files follow the same naming convention as the stan files mentioned above and simulate synthetic data from the respective model.

- "getmodels.R" is sourced during analysis and provides information with regards to the parameters contained in each model as well as the paths to the corresponding simulation functions and stan model code.

### "Catches" Condition

***Analyses*** The "code/catches/analyses" subdirectory contains the analyses for the "catches" condition.

- "numsims.R" runs the numerical simulations and plots the results.

- "modelcomp.R" runs the model comparison, computes the posterior predictions implied by the winning model and plots the results.

- "parrecov.R" runs a parameter recovery analysis for the winning model.

***Stan Files*** The "code/nocatches/stan" subdirectory contains the stan code for the SRL-C models.

- "arl" denotes the ARL model m4.1.

- "dbn" denotes the SRL model integrating location-related social information via DB.

- "vsn" denotes the SRL model integrating location-related social information via VS.

- "dbr" denotes the SRL model integrating catch-related social information via DB.

- "vsr" denotes the SRL model integrating catch-related social information via VS.

- "dbnvsr" denotes the SRL integrating location-related social information via DB and catch-related social information via VS.

- "dbndbr" denotes the SRL integrating both location- and catch-related social information via DB.

- "vsndbr" denotes the SRL integrating location-related social information via VS and catch-related social information via DB.

- "vsnvsr" denotes the SRL integrating both location- and catch-related social information via VS.

- "fixed" and "hierarch" denote whether the model is implemented as a fixed- or varying-effects model.

- "ll" denotes that the stan code computes the log-likelihood in the "generated quantities" section.

***Functions*** The "code/nocatches/functions" subdirectory contains the functions used during the analysis. 

- "fixed.sim.R" files follow the same naming convention as the stan files mentioned above and simulate synthetic data from the respective model.

- "getmodels.R" is sourced during analysis and provides information with regards to the parameters contained in each model as well as the paths to the corresponding simulation functions and stan model code.


### Figures

- "RLfit.py": plots the fits of the RL models of Figs 3, 5 and 7 (alone, no catches, and catches conditions)

- "RLsimu.py": plots the differences of mean accuracy as a function of social weights of Figs 4 and 6 

- "RLsimuSupp.py": plots the differences of accuracy and switch rate as a function of time for the supplementary figures


## Drift-Diffusion Models

### In all folders
- NumSimu.py : simulates the DDM equations

### Fig1_and_2
- Fig1.py : generates the DDM plot of Fig 1 (c)
- Fig2.py : generates Fig 2

### data_analysis
- ExpAccuracy.py : extracts accuracy as a function of time from the empirical data (alone, no catches, and catches conditions)
- PlotExpFig.py : plots empirical accuracy as a function of time (alone, no catches, and catches conditions)

### alone
- FitSoloparam.py : infers the parameters to fit the empirical data 
- PlotFit1.py : plots the simulated accuracy with the inferred parameters as a fonction of time of Fig 3

### nocatches
- Fit2param.py : infers the parameters to fit the empirical data 
- PlotFit2.py : plots the simulated accuracy with the inferred parameters as a fonction of time of Fig 5
- SimuSocial2.py : simulates the DDM for different social weights values
- DDMsimu_plot.py : plots the differences of mean accuracy as a function of social weights of Fig 4, and the differences of accuracy and switch rate as a function of time of the supplementary figures

### catches
- Fit3param.py : infers the parameters to fit the empirical data 
- PlotFit3.py : plots the simulated accuracy with the inferred parameters as a fonction of time of Fig 7
- SimuSocial3.py : simulates the DDM for different social weights values
- DDMsimu_plot.py : plots the differences of mean accuracy as a function of social weights of Fig 6, and the differences of accuracy and switch rate as a function of time of the supplementary figures
