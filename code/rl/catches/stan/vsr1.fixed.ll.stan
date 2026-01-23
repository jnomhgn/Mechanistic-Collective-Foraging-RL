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

  // Asocial RL pars
  real<lower = 0, upper = 1> alphaQN;
  real<lower = 0, upper = 1> alphaQP;
  real<lower = 0> betaQ;
  real betaC;        
  
  // Social RL pars
  real<lower = 0, upper = 1> alphaVSR;
  
}

// Model
model{
  
  // Assign priors

  // Asocial rl pars
  alphaQN ~ beta(2, 2); 
  alphaQP ~ beta(2, 2); 
  betaQ ~ lognormal(1.5, .5);
  betaC ~ normal(0, 2);
  
  // Social rl pars
  alphaVSR ~ beta(2,2);

  // Declare local variables
  vector[DECISIONS] Q; // Value of each state
  vector[DECISIONS] C; // Choice trace for each state
  vector[DECISIONS] p; // individual policy (that gets updated)
  real Qsoc; // social policy used for updating

  // Loop over observations
  for (observation in 1:OBSERVATIONS){

      // If new trial, initialize / reset Q values
      if(time[observation] == 0){
        Q = [0.5, 0.5]';
        C = [0, 0]';
      }else{ // No social info on first time step

        // Compute social value of choice
        Qsoc = obsrew[observation]; // Computed outside of stan so that obsrew[observation] are the observed rewards for each option from the timestep observation-1
        
        if(Qsoc != 100){
            // Shape values
            Q[decision[observation-1]] = Q[decision[observation-1]] + alphaVSR * (Qsoc - Q[decision[observation-1]]);
            // Adjust other option
            // Q[3 - decision[observation-1]] = 1 - Q[3 - decision[observation-1]];
            } 
      }
      
      // Compute choice probabilities
      p = softmax(betaQ * Q + betaC * C);

      // Sample decision
      decision[observation] ~ categorical(p);

      // Update Q-values. (Players can receive rewards at t == 0)
      if((reward[observation] - Q[decision[observation]]) < 0){
        Q[decision[observation]] = Q[decision[observation]] + alphaQN *(reward[observation] - Q[decision[observation]]); 
      }else{
        Q[decision[observation]] = Q[decision[observation]] + alphaQP *(reward[observation] - Q[decision[observation]]); 
      }

      // Update choice trace. Considers previous decision only
      C = [0, 0]';
      C[decision[observation]] = 1;
      
  }
}

generated quantities{

   // Create log-likelihood vector
  vector[OBSERVATIONS] log_lik;

  // Declare local variables
  vector[DECISIONS] Q; // Value of each state
  vector[DECISIONS] C; // Choice trace for each state
  vector[DECISIONS] p; // policy
  real Qsoc; // social policy used for updating


  // Loop over observations
  for (observation in 1:OBSERVATIONS){

      if(time[observation] == 0){
        Q = [0.5, 0.5]';
        C = [0, 0]';
      }else{ // No social info on first time step
        // Compute social value of choice
        Qsoc = obsrew[observation]; // Computed outside of stan so that obsrew[observation] are the observed rewards for each option from the timestep observation-1
        
        if(Qsoc != 100){
            // Shape values
            Q[decision[observation-1]] = Q[decision[observation-1]] + alphaVSR * (Qsoc - Q[decision[observation-1]]);
            // Adjust other option
            // Q[3 - decision[observation-1]] = 1 - Q[3 - decision[observation-1]];
            }
      }

      // Compute choice probabilities
      p = softmax(betaQ * Q + betaC * C);

      // Compute log likelyhood of data given policy
      if(time[observation] == 0){
        log_lik[observation] = positive_infinity();
      }else{
        log_lik[observation] = categorical_lpmf(decision[observation] | p);
      }

      // Update Q-values.
      if((reward[observation] - Q[decision[observation]]) < 0){
        Q[decision[observation]] = Q[decision[observation] ] + alphaQN * (reward[observation] - Q[decision[observation]]); // + 1 is for indexing
      }else{
        Q[decision[observation]] = Q[decision[observation]] + alphaQP * (reward[observation] - Q[decision[observation]]); // + 1 is for indexing
      }

      // Update choice trace. Considers previous decision only
      C = [0, 0]';
      C[decision[observation]] = 1;


  }


}

