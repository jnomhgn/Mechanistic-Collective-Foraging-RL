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
  real logit_alphaQN[MAXIMUM, RATIO]; // Different asocial learning weights for negative reward prediction errors and yield maximum x yield ratio
  real logit_alphaQP[MAXIMUM, RATIO]; // Different asocial learning weights for position rpes and yield maximum x yield ratio
  real log_betaQ; // Inverse temperature
  real betaC;            // Autocorrelation strength
  
  // Varying effects clustered on individuals for each parameter(matrix), i.e.
  // for negative and positive rpe learning rates (across environments),
  // inverse temperature, and autocorrelation.
  // Individual offsets from population mean for each [parameter, individual] are
  // constructed by matrix multiplying uncorrelated z-values, the standard deviations
  // of parameters across individuals, and the cholesky factor. 
  // z %*% cholesky induces correlation among z-values, (z %*% cholesky) %*% sigma 
  // adds scaling.
  matrix[4, ID] z;           // z - values
  vector<lower=0>[4] sigma;       // SDs
  cholesky_factor_corr[4] cholesky;   // Cholesky factor matrix
  
}

transformed parameters{
  matrix[ID, 4] idoffset; // Matrix of varying effects for each individual
  idoffset = ( diag_pre_multiply( sigma , cholesky ) * z )'; // constructed from z, sigma, cholesky
}

// Model
model{
  
  // Assign priors
  // Population means
  // Social reinforcement learning
  for(m in 1:MAXIMUM){
    for(r in 1:RATIO){
      logit_alphaQN[m, r] ~ normal(0, 1.5);
      logit_alphaQP[m, r] ~ normal(0, 1.5);
    }
  }
  log_betaQ ~ normal(1.5, .5); 
  betaC ~ normal(0, 2);

  // Individual offsets
  to_vector(z) ~ normal(0,1); 
  sigma ~ exponential(2); 
  cholesky ~ lkj_corr_cholesky(2); // the higher the number, the more skeptical of extreme correlations



  // Declare local variables
  vector[DECISIONS] Q; // Value of each state
  vector[DECISIONS] C; // Choice trace for each state
  vector[DECISIONS] p; // policy
  
  real idalphaQ; // individual-level learning rate (overwritten so same variable for both positive and negative rpes)
  real idbetaQ; // individual-level inverse temp
  real idbetaC; // individual-level autocorrelation


  // Loop over observations
  for (observation in 1:OBSERVATIONS){

      // If new trial, initialize / reset Q values and choice history
      if(time[observation] == 0){
        Q = [0.5, 0.5]';
        C = [0, 0]';
      }
      
      // Construct id specific inverse temp and autocorrelation
      idbetaQ = exp(log_betaQ + idoffset[id[observation], 3]);
      idbetaC = betaC + idoffset[id[observation], 4];
      
      // Compute choice probabilities
      p = softmax(idbetaQ * Q + idbetaC * C);

      // Sample decision
      decision[observation] ~ categorical(p);

      // Construct id specific learning rate
      if((reward[observation] - Q[decision[observation]]) < 0){
        idalphaQ = inv_logit(logit_alphaQN[maximum[observation], ratio[observation]] + idoffset[id[observation], 1]);
      }else{
        idalphaQ = inv_logit(logit_alphaQP[maximum[observation], ratio[observation]] + idoffset[id[observation], 2]);
      }
      
      // Update Q-values
      Q[decision[observation]] = Q[decision[observation]] + idalphaQ *(reward[observation] - Q[decision[observation]]); 


      // Update choice trace. Considers previous decision only
      C = [0, 0]';
      C[decision[observation]] = 1;
      
  }
}

generated quantities{
  
  // Transform population-level estimates to proper scale and compute individual-level estimates
  
  // Declare variables
  matrix[4, 4] Rho; // Correlation matrix

  real<lower = 0, upper = 1> alphaQN[MAXIMUM, RATIO]; 
  matrix<lower = 0, upper = 1>[MAXIMUM, RATIO] idalphaQN[ID]; // A vector in which each element contains (a matrix of) the learning rates of that individual

  real<lower = 0, upper = 1> alphaQP[MAXIMUM, RATIO]; 
  matrix<lower = 0, upper = 1>[MAXIMUM, RATIO] idalphaQP[ID]; // A vector in which each element contains (a matrix of) the learning rates of that individual
  
  real<lower = 0> betaQ;
  real<lower=0> idbetaQ[ID];
  
  real idbetaC[ID];

  // Transform population-level estimates
  betaQ = exp(log_betaQ);
  
  for(m in 1:MAXIMUM){
    for(r in 1:RATIO){
      alphaQN[m, r] = inv_logit(logit_alphaQN[m, r]);
      alphaQP[m, r] = inv_logit(logit_alphaQP[m, r]);
    }
  }
  
  // Compute and transform individual-level estimates
  for(i in 1:ID){
    
    idbetaQ[i] = exp(log_betaQ + idoffset[i, 3]);
    idbetaC[i] = betaC + idoffset[i, 4];
    
    for(m in 1:MAXIMUM){
      for(r in 1:RATIO){
        idalphaQN[i, m, r] = inv_logit(logit_alphaQN[m, r] + idoffset[i, 1]);
        idalphaQP[i, m, r] = inv_logit(logit_alphaQP[m, r] + idoffset[i, 2]);
      }
    }
  }
  
  // Correlation matrix
   Rho = cholesky * cholesky';

   
   
  // Compute log likelihood
  vector[OBSERVATIONS] log_lik;
  
  // Declare local variables
  vector[DECISIONS] Q; // Value of each state
  vector[DECISIONS] C; // Choice trace for each state
  vector[DECISIONS] p; // policy


  // Loop over observations
  for (observation in 1:OBSERVATIONS){
    

      if(time[observation] == 0){
        Q = [0.5, 0.5]';
        C = [0, 0]';
      }
      
      // Compute choice probabilities
      p = softmax(idbetaQ[id[observation]] * Q + idbetaC[id[observation]] * C);
        
      // Compute log likelyhood of data given policy
      if(time[observation] == 0){
        log_lik[observation] = 0;
      }else{
        log_lik[observation] = categorical_lpmf(decision[observation] | p); 
      }
        
      // Update Q-values. 
      if((reward[observation] - Q[decision[observation]]) < 0){
        Q[decision[observation]] = Q[decision[observation] ] + idalphaQN[id[observation], maximum[observation], ratio[observation]] * (reward[observation] - Q[decision[observation]]); // + 1 is for indexing
      }else{
        Q[decision[observation]] = Q[decision[observation]] + idalphaQP[id[observation], maximum[observation], ratio[observation]] * (reward[observation] - Q[decision[observation]]); // + 1 is for indexing
      }
      
      // Update choice trace. Considers previous decision only
      C = [0, 0]';
      C[decision[observation]] = 1;
      

  }



}

