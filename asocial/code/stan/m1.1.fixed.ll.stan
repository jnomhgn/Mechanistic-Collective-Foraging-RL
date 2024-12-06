// Data
data{
  int<lower = 1> OBSERVATIONS; // number of observations
  
  int<lower = 1> MAXIMUM; // Number of maximum catch probabilities
  int<lower = 1, upper = MAXIMUM> maximum[OBSERVATIONS]; // vector of maximum catch probabilities
  
  int<lower = 1> RATIO; // Number of maximum catch probabilities
  int<lower = 1, upper = RATIO> ratio[OBSERVATIONS]; // vector of maximum catch probabilities
  
  int<lower = 1> PLAYERS; // number of players
  
  int<lower = 0> TIMES; // max trial length
  int<lower = 0, upper = TIMES> time[OBSERVATIONS]; // vector of time steps

  int<lower = 1> DECISIONS; // number of decision outcomes 
  int<lower = 0, upper = DECISIONS> decision[OBSERVATIONS]; // vector of observed decisions 
 
  int<lower = 1> REWARDS; // number of reward outcomes
  int<lower = 0, upper = REWARDS-1> reward[OBSERVATIONS]; // vector of observed rewards
}

// Parameters to estimate
parameters{
  
  real<lower = 0, upper = 1> alphaQ;
  real<lower = 0> betaQ;

}

// Model
model{
  
  // Assign priors
  alphaQ ~ beta(2, 2); 
  betaQ ~ lognormal(1.5, .5);

  // Declare local variables
  vector[DECISIONS] Q; // Value of each state
  vector[DECISIONS] p; // policy

  // Loop over observations
  for (observation in 1:OBSERVATIONS){

      // If new trial, initialize / reset Q values
      if(time[observation] == 0){
        Q = [0.5, 0.5]';
      }
      
      // Compute choice probabilities
      p = softmax(betaQ * Q);

      // Sample decision
      decision[observation] ~ categorical(p);

      // Update Q-values. 
      Q[decision[observation]] = Q[decision[observation]] + alphaQ *(reward[observation] - Q[decision[observation]]); 

      
  }
}

generated quantities{
  
  // Create log-likelihood vector
  vector[OBSERVATIONS] log_lik;
  
  // Declare local variables
  vector[DECISIONS] Q; // Value of each state
  vector[DECISIONS] p; // policy

  // Loop over observations
  for (observation in 1:OBSERVATIONS){

      // If new trial, initialize / reset Q values
      if(time[observation] == 0){
        Q = [0.5, 0.5]';
      }
      
      // Compute choice probabilities
      p = softmax(betaQ * Q);

      // Compute log likelyhood of data given policy
      if(time[observation] == 0){
        log_lik[observation] = 0;
      }else{
        log_lik[observation] = categorical_lpmf(decision[observation] | p); 
      }

      // Update Q-values. (Players can receive rewards at t == 0)
      Q[decision[observation]] = Q[decision[observation]] + alphaQ *(reward[observation] - Q[decision[observation]]); 

      
  }
  
}

