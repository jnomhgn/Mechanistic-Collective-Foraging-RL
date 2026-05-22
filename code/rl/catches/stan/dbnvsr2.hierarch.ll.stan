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
  real<lower = 0> obsrew[OBSERVATIONS]; // vector of observed rewards

 
  int<lower = 1> REWARDS; // Number of reward outcomes
  int<lower = 0, upper = REWARDS-1> reward[OBSERVATIONS]; // Vector of observed rewards coded as 0 / 1
}

// Parameters to estimate
parameters{
  
  // Population means: logit / log ensure ranges between 0 and 1 / larger than 0
  
  // Asocial reinforcement learning parameters
  real logit_alphaQN; // Different asocial learning weights for negative reward prediction errors 
  real logit_alphaQP; // Different asocial learning weights for position rpes
  real log_betaQ; // Inverse temperature
  real betaC;     // Autocorrelation strength
  
  // Social reinforcement learning parameters
  real logit_alphaVSR[MAXIMUM, RATIO]; // Social learning weight for reward-based value shaping per environment
  real logit_alphaDBD[MAXIMUM, RATIO]; // Social learning weight for decision-based decision biasing per environment

  
  // Varying effects clustered on individuals for each parameter(vector)
  matrix[6, ID] z;           // z - values
  vector<lower=0>[6] sigma;       // SDs
  cholesky_factor_corr[6] cholesky;   // Cholesky factor matrix
  
}

transformed parameters{
  matrix[ID, 6] idoffset; // Matrix of varying effects for each individual
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
    // Social reinforcement learning
  for(m in 1:MAXIMUM){
    for(r in 1:RATIO){
      logit_alphaDBD[m, r] ~ normal(0, 1.5);
      logit_alphaVSR[m, r] ~ normal(0, 1.5);
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
  real Qsoc; // social value used for updating

  
  real idalphaQ; // individual-level asocial learning rate (overwritten so same variable for both positive and negative rpes)
  real idbetaQ; // individual-level inverse temp
  real idbetaC; // individual-level autocorrelation
  real idalphaVSR; // individual-level social learning rate
  real idalphaDBD; // individual-level social learning rate



  // Loop over observations
  for (observation in 1:OBSERVATIONS){

      // If new trial, initialize / reset Q values
      if(time[observation] == 0){
        Q = [0.5, 0.5]';
        C = [0, 0]';
      }else{ // Value shaping
        // Construct individual-level social learning weight
        idalphaVSR = inv_logit(logit_alphaVSR[maximum[observation], ratio[observation]] + idoffset[id[observation], 5]);

        // Compute social value of choice
        Qsoc = obsrew[observation]; // Computed outside of stan so that obsrew[observation] are the observed rewards for each option from the timestep observation-1
        
        // Shape values if somebody else at patch, i.e. obsrew != 100 (NA)
        if(obsrew[observation] != 100 ){
          Q[decision[observation-1]] = Q[decision[observation-1]] + idalphaVSR * (Qsoc - Q[decision[observation-1]]);
        }
	
        // Adjust other option
        // Q[3 - decision[observation-1]] = 1 - Q[3 - decision[observation-1]];
        
      }
      
      // Construct id specific inverse temp and autocorrelation
      idbetaQ = exp(log_betaQ + idoffset[id[observation], 3]);
      idbetaC = betaC + idoffset[id[observation], 4];
      
      // Compute choice probabilities
      p = softmax(idbetaQ * Q + idbetaC * C);
      
      // Decision biasing
      if(time[observation] != 0){ // No social info at first time step

        // Construct id specific social learning weight.
        idalphaDBD = inv_logit(logit_alphaDBD[maximum[observation], ratio[observation]] + idoffset[id[observation], 6]);

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

generated quantities{

  // Transform population-level estimates to proper scale and compute individual-level estimates

  // Declare variables
  matrix[6, 6] Rho; // Correlation matrix

  real<lower = 0, upper = 1> alphaQN;
  real<lower=0, upper=1> idalphaQN[ID];

  real<lower = 0, upper = 1> alphaQP;
  real<lower=0, upper=1> idalphaQP[ID];

  real<lower = 0> betaQ;
  real<lower=0> idbetaQ[ID];

  real idbetaC[ID];

  real<lower = 0, upper = 1> alphaVSR[MAXIMUM, RATIO];
  matrix[MAXIMUM, RATIO] idalphaVSR[ID]; // A vector in which each element contains (a matrix of) the learning rates of that individual

  real<lower = 0, upper = 1> alphaDBD[MAXIMUM, RATIO];
  matrix<lower = 0, upper = 1>[MAXIMUM, RATIO] idalphaDBD[ID]; // A vector in which each element contains (a matrix of) the learning rates of that individual

  // Transform population-level estimates
  betaQ = exp(log_betaQ);
  alphaQN = inv_logit(logit_alphaQN);
  alphaQP = inv_logit(logit_alphaQP);
  for(m in 1:MAXIMUM){
    for(r in 1:RATIO){
      alphaVSR[m, r] = inv_logit(logit_alphaVSR[m, r]);
      alphaDBD[m, r] = inv_logit(logit_alphaDBD[m, r]);
    }
  }

  // Compute and transform individual-level estimates
  for(i in 1:ID){

    idalphaQN[i] = inv_logit(logit_alphaQN + idoffset[i, 1]);
    idalphaQP[i] = inv_logit(logit_alphaQP + idoffset[i, 2]);
    idbetaQ[i] = exp(log_betaQ + idoffset[i, 3]);
    idbetaC[i] = betaC + idoffset[i, 4];
    for(m in 1:MAXIMUM){
      for(r in 1:RATIO){
        idalphaVSR[i, m, r] = inv_logit(logit_alphaVSR[m, r] + idoffset[i, 5]);
        idalphaDBD[i, m, r] = inv_logit(logit_alphaDBD[m, r] + idoffset[i, 6]);

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
  vector[DECISIONS] psoc; // social policy used for updating
  real Qsoc; // social value used for updating


  // Loop over observations
  for (observation in 1:OBSERVATIONS){


      if(time[observation] == 0){
        Q = [0.5, 0.5]';
        C = [0, 0]';
      }else{ // Value shaping
        // Compute social value of choice
        Qsoc = obsrew[observation]; // Computed outside of stan so that obsrew[observation] are the observed rewards for each option from the timestep observation-1
        
      // Shape values if somebody else at patch, i.e. obsrew != 100 (NA)
      if(obsrew[observation] != 100 ){
        Q[decision[observation-1]] = Q[decision[observation-1]] + idalphaVSR[id[observation], maximum[observation], ratio[observation]] * (Qsoc - Q[decision[observation-1]]);

      }

	      // Adjust other option
        // Q[3 - decision[observation-1]] = 1 - Q[3 - decision[observation-1]];
        
      }

      // Compute choice probabilities
      p = softmax(idbetaQ[id[observation]] * Q + idbetaC[id[observation]] * C);
      
      // Decision biasing
      if(time[observation] != 0){ // No social info at first time step

        // Update individual policy using social policy
        psoc = to_vector(obsdec[observation, ]);
        p = p + idalphaDBD[id[observation], maximum[observation], ratio[observation]] * (psoc - p);
      }

      // Compute log likelyhood of data given policy
      if(time[observation] == 0){
        log_lik[observation] = positive_infinity();
      }else{
        log_lik[observation] = categorical_lpmf(decision[observation] | p);
      }

      // Update Q-values.
      if((reward[observation] - Q[decision[observation]]) < 0){
        Q[decision[observation]] = Q[decision[observation] ] + idalphaQN[id[observation]] * (reward[observation] - Q[decision[observation]]); // + 1 is for indexing
      }else{
        Q[decision[observation]] = Q[decision[observation]] + idalphaQP[id[observation]] * (reward[observation] - Q[decision[observation]]); // + 1 is for indexing
      }

      // Update choice trace. Considers previous decision only
      C = [0, 0]';
      C[decision[observation]] = 1;


  }


}

