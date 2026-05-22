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
  
  real<lower = 0> obsdec[OBSERVATIONS, DECISIONS]; // matrix of decision frequencies
 
  int<lower = 1> REWARDS; // Number of reward outcomes
  int<lower = 0, upper = REWARDS-1> reward[OBSERVATIONS]; // Vector of observed rewards coded as 0 / 1
}

// Parameters to estimate
parameters{
  

 // Population means: Retransforming logit / log ensure ranges between 0 and 1 / larger than 0
  real logit_alphaQN; // Different asocial learning weights for negative reward prediction errors 
  real logit_alphaQP; // Different asocial learning weights for position rpes 
  real log_betaQ; // Inverse temperature
  real betaC;            // Autocorrelation strength
  
  // Social reinforcement learning parameters
  real logit_alphaDBD[MAXIMUM, RATIO]; // Social learning weight for decision-based decision biasing
  
  // Varying effects clustered on individuals for each parameter(vector)
  matrix[5, ID] z;           // z - values
  vector<lower=0>[5] sigma;       // SDs
  cholesky_factor_corr[5] cholesky;   // Cholesky factor matrix
  
}

transformed parameters{
  matrix[ID, 5] idoffset; // Matrix of varying effects for each individual
  idoffset = ( diag_pre_multiply( sigma , cholesky ) * z )'; // constructed from z, sigma, cholesky
}

// Model
model{
  
  // Assign priors
  
  // Population means
  
  // Asocial reinforcement learning
  logit_alphaQN ~ normal(0, 1.5); 
  logit_alphaQP ~ normal(0, 1.5); 
  log_betaQ ~ normal(1.5, .5); 
  betaC ~ normal(0, 2);
  
  // Social reinforcement learning
  for(m in 1:MAXIMUM){
    for(r in 1:RATIO){
      logit_alphaDBD[m, r] ~ normal(0, 1.5);
    }
  }
  

  // Individual offsets
  to_vector(z) ~ normal(0,1); 
  sigma ~ exponential(2); 
  cholesky ~ lkj_corr_cholesky(2); // the higher the number, the more skeptical of extreme correlations


  // Declare local variables
  vector[DECISIONS] Q; // Value of each state
  vector[DECISIONS] C; // Choice trace for each state
  vector[DECISIONS] p; // individual policy (that gets updated)
  vector[DECISIONS] psoc; // social policy used for updating

  
  real idalphaQ; // individual-level asocial learning rate (overwritten so same variable for both positive and negative rpes)
  real idbetaQ; // individual-level inverse temp
  real idbetaC; // individual-level autocorrelation
  real idalphaDBD; // individual-level social learning rate


  // Loop over observations
  for (observation in 1:OBSERVATIONS){

      // If new trial, initialize / reset Q values
      if(time[observation] == 0){
        Q = [0.5, 0.5]';
        C = [0, 0]';
      }
      
      // Construct id specific inverse temp and autocorrelation
      idbetaQ = exp(log_betaQ + idoffset[id[observation], 3]);
      idbetaC = betaC + idoffset[id[observation], 4];
      
      // Compute choice probabilities
      p = softmax(idbetaQ * Q + idbetaC * C);

      // Decision biasing
      if(time[observation] != 0){ // No social info at first time step

        // Construct id specific social learning weight.
        idalphaDBD = inv_logit(logit_alphaDBD[maximum[observation], ratio[observation]] + idoffset[id[observation], 5]);

        // Update individual policy using social policy
        psoc = to_vector(obsdec[observation, ]);
        p = p + idalphaDBD * (psoc - p);
      }

      // Sample decision
      decision[observation] ~ categorical(p);

      // Construct id specific learning rate
      if((reward[observation] - Q[decision[observation]]) < 0){
        idalphaQ = inv_logit(logit_alphaQN + idoffset[id[observation], 1]);
      }else{
        idalphaQ = inv_logit(logit_alphaQP + idoffset[id[observation], 2]);
      }
      
      // Update Q-values
      Q[decision[observation]] = Q[decision[observation]] + idalphaQ *(reward[observation] - Q[decision[observation]]); 


      // Update choice trace. Considers previous decision only
      C = [0, 0]';
      C[decision[observation]] = 1;
      
  }
}
