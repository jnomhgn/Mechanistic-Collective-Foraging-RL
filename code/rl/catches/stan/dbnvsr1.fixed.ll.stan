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
  real logit_alphaVSR; // Social learning weight for reward-based value shaping
  real logit_alphaDBD; // Social learning weight for decision-based decision biasing
  
}

// Model
model{
  
  // Assign priors
    
  // Asocial reinforcement learning
  logit_alphaQN ~ normal(0, 1.5); 
  logit_alphaQP ~ normal(0, 1.5); 
  log_betaQ ~ normal(1.5, .5); 
  betaC ~ normal(0, 2);
  
  // Social reinforcement learning
  logit_alphaVSR ~ normal(0, 1.5);
  logit_alphaDBD ~ normal(0, 1.5);

  // Declare local variables
  vector[DECISIONS] Q; // Value of each state
  vector[DECISIONS] C; // Choice trace for each state
  vector[DECISIONS] p; // individual policy (that gets updated)
  vector[DECISIONS] psoc; // social policy used for updating
  real Qsoc; // social value used for updating

  real alphaQ; 
  real betaQ; 
  real alphaVSR; 
  real alphaDBD; 

  // Loop over observations
  for (observation in 1:OBSERVATIONS){

      // If new trial, initialize / reset Q values
      if(time[observation] == 0){
        Q = [0.5, 0.5]';
        C = [0, 0]';
      }else{ // Value shaping
        // Construct individual-level social learning weight
        alphaVSR = inv_logit(logit_alphaVSR);

        // Compute social value of choice
        Qsoc = obsrew[observation]; // Computed outside of stan so that obsrew[observation] are the observed rewards for each option from the timestep observation-1
        
        // Shape values if somebody else at patch, i.e. obsrew != 100 (NA)
        if(obsrew[observation] != 100 ){
          Q[decision[observation-1]] = Q[decision[observation-1]] + alphaVSR * (Qsoc - Q[decision[observation-1]]);
        }
	
        // Adjust other option
        // Q[3 - decision[observation-1]] = 1 - Q[3 - decision[observation-1]];
        
      }
      
      // Construct id specific inverse temp and autocorrelation
      betaQ = exp(log_betaQ);
      
      // Compute choice probabilities
      p = softmax(betaQ * Q + betaC * C);
      
      // Decision biasing
      if(time[observation] != 0){ // No social info at first time step

        // Construct id specific social learning weight.
        alphaDBD = inv_logit(logit_alphaDBD);

        // Update individual policy using social policy
        psoc = to_vector(obsdec[observation, ]);
        p = p + alphaDBD * (psoc - p);
      }

      // Sample decision
      decision[observation] ~ categorical(p);

      // Construct id specific learning rate
      if((reward[observation] - Q[decision[observation]]) < 0){
        alphaQ = inv_logit(logit_alphaQN);
      }else{
        alphaQ = inv_logit(logit_alphaQP);
      }
      
      // Update Q-values
      Q[decision[observation]] = Q[decision[observation]] + alphaQ *(reward[observation] - Q[decision[observation]]); 


      // Update choice trace. Considers previous decision only
      C = [0, 0]';
      C[decision[observation]] = 1;
      
  }
}

generated quantities{

  // Transform population-level estimates to proper scale 
  real<lower = 0, upper = 1> alphaQN;
  real<lower = 0, upper = 1> alphaQP;
  real<lower = 0> betaQ;
  real<lower = 0, upper = 1> alphaVSR;  
  real<lower = 0, upper = 1> alphaDBD;

  // Transform population-level estimates
  betaQ = exp(log_betaQ);
  alphaQN = inv_logit(logit_alphaQN);
  alphaQP = inv_logit(logit_alphaQP);
  alphaVSR = inv_logit(logit_alphaVSR);
  alphaDBD = inv_logit(logit_alphaDBD);

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
		Q[decision[observation-1]] = Q[decision[observation-1]] + alphaVSR * (Qsoc - Q[decision[observation-1]]);
	}

	// Adjust other option
        // Q[3 - decision[observation-1]] = 1 - Q[3 - decision[observation-1]];
        
      }

      // Compute choice probabilities
      p = softmax(betaQ * Q + betaC * C);
      
      // Decision biasing
      if(time[observation] != 0){ // No social info at first time step

        // Update individual policy using social policy
        psoc = to_vector(obsdec[observation, ]);
        p = p + alphaDBD * (psoc - p);
      }

      // Compute log likelyhood of data given policy
      if(time[observation] == 0){
        log_lik[observation] = 0;
      }else{
        log_lik[observation] = categorical_lpmf(decision[observation] | p);
      }

      // Update Q-values.
      if((reward[observation] - Q[decision[observation]]) < 0){
        Q[decision[observation]] = Q[decision[observation]] + alphaQN * (reward[observation] - Q[decision[observation]]); // + 1 is for indexing
      }else{
        Q[decision[observation]] = Q[decision[observation]] + alphaQP * (reward[observation] - Q[decision[observation]]); // + 1 is for indexing
      }

      // Update choice trace. Considers previous decision only
      C = [0, 0]';
      C[decision[observation]] = 1;


  }


}

