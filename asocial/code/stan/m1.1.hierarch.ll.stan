// Data
data{
  int<lower = 1> OBSERVATIONS; // number of observations
  
  int<lower = 1> MAXIMUM; // Number of levels for maximum catch probability, i.e. levels of resource abundance
  int<lower = 1, upper = MAXIMUM> maximum[OBSERVATIONS]; // Vector of maximum catch probabilities for each observation
  
  int<lower = 1> RATIO; // Number of levels for catch probability ratio, i.e. levels of discriminability
  int<lower = 1, upper = RATIO> ratio[OBSERVATIONS]; // Vector of catch probability ratios for each observation
  
  int<lower = 1> PLAYERS; // Number of players in each trial, i.e. group size
  
  int<lower = 1> ID; // Number of players across all trials, i.e. max uniqe id
  int<lower = 1, upper = ID> id[OBSERVATIONS]; // Vector of unique ids
  
  int<lower = 0> TIMES; // Max trial length
  int<lower = 0, upper = TIMES> time[OBSERVATIONS]; // vector of time steps

  int<lower = 1> DECISIONS; // Number of decision outcomes [2]
  int<lower = 0, upper = DECISIONS> decision[OBSERVATIONS]; // Vector of observed decisions coded as 1 / 2 for softmax indexing
 
  int<lower = 1> REWARDS; // Number of reward outcomes
  int<lower = 0, upper = REWARDS-1> reward[OBSERVATIONS]; // Vector of observed rewards coded as 0 / 1
}

// Parameters to estimate
parameters{
  
  // Asocial reinforcement learning parameters: 
  
  // Population means: logit / log ensure ranges between 0 and 1 / larger than 0
  real logit_alphaQ; // Asocial learning weight
  real log_betaQ; // Inverse temperature

  // Varying effects clustered on individuals for each parameter(vector), i.e.
  // for negative and positive rpe learning rates (across levels of resource abundance),
  // inverse temperature, and autocorrelation.
  // Individual offsets from population mean for each [parameter, individual] are
  // constructed by matrix multiplying uncorrelated z-values, the standard deviations
  // of parameters across individuals, and the cholesky factor. 
  // z %*% cholesky induces correlation among z-values, (z %*% cholesky) %*% sigma 
  // adds scaling.
  matrix[2, ID] z;           // z - values
  vector<lower=0>[2] sigma;       // SDs
  cholesky_factor_corr[2] cholesky;   // Cholesky factor matrix
  
}

transformed parameters{
  matrix[ID, 2] idoffset; // Matrix of varying effects for each individual
  idoffset = ( diag_pre_multiply( sigma , cholesky ) * z )'; // constructed from z, sigma, cholesky
}

// Model
model{
  
  // Assign priors
  
  // Asocial reinforcement learning
  // Population means
  logit_alphaQ ~ normal(0, 1.5); 
  log_betaQ ~ normal(1.5, .5); 

  // Individual offsets
  to_vector(z) ~ normal(0,1); 
  sigma ~ exponential(2); 
  cholesky ~ lkj_corr_cholesky(2); // the higher the number, the more skeptical of extreme correlations



  // Declare local variables
  vector[DECISIONS] Q; // Value of each state
  vector[DECISIONS] p; // policy
  
  real idalphaQ; // individual-level learning rate 
  real idbetaQ; // individual-level inverse temp


  // Loop over observations
  for (observation in 1:OBSERVATIONS){

      // If new trial, initialize / reset Q values and choice history
      if(time[observation] == 0){
        Q = [0.5, 0.5]';
      }
      
      // Construct id specific inverse temp
      idbetaQ = exp(log_betaQ + idoffset[id[observation], 2]);

      // Compute choice probabilities
      p = softmax(idbetaQ * Q);

      // Sample decision
      decision[observation] ~ categorical(p);

      // Construct id specific learning rate
      idalphaQ = inv_logit(logit_alphaQ + idoffset[id[observation], 1]);

      
      // Update Q-values
      Q[decision[observation]] = Q[decision[observation]] + idalphaQ *(reward[observation] - Q[decision[observation]]); 
      
  }
}

generated quantities{
  
  // Transform population-level estimates to proper scale and compute individual-level estimates
  
  // Declare variables
  matrix[2, 2] Rho; // Correlation matrix

  real<lower = 0, upper = 1> alphaQ; 
  real<lower=0, upper=1> idalphaQ[ID];
  
  real<lower = 0> betaQ;
  real<lower=0> idbetaQ[ID];
  
  // Transform population-level estimates
  alphaQ = inv_logit(logit_alphaQ);
  betaQ = exp(log_betaQ);

  // Compute and transform individual-level estimates
  for(i in 1:ID){
    idalphaQ[i] = inv_logit(logit_alphaQ + idoffset[i, 1]);
    idbetaQ[i] = exp(log_betaQ + idoffset[i, 2]);
  }
  
  // Correlation matrix
   Rho = cholesky * cholesky';

   
   
  // Compute log likelihood
  vector[OBSERVATIONS] log_lik;
  
  // Declare local variables
  vector[DECISIONS] Q; // Value of each state
  vector[DECISIONS] p; // policy


  // Loop over observations
  for (observation in 1:OBSERVATIONS){
    

      if(time[observation] == 0){
        Q = [0.5, 0.5]';
      }
      
      // Compute choice probabilities
      p = softmax(idbetaQ[id[observation]] * Q);
        
      // Compute log likelyhood of data given policy
      if(time[observation] == 0){
        log_lik[observation] = 0;
      }else{
        log_lik[observation] = categorical_lpmf(decision[observation] | p); 
      }
        
      // Update Q-values. 
      Q[decision[observation]] = Q[decision[observation] ] + idalphaQ[id[observation]] * (reward[observation] - Q[decision[observation]]); 

      

  }



}

