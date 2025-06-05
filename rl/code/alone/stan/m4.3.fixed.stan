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
  
  real<lower = 0, upper = 1> alphaQN[MAXIMUM, RATIO];
  real<lower = 0, upper = 1> alphaQP[MAXIMUM, RATIO];
  real<lower = 0> betaQ;
  real betaC;                                
  
}

// Model
model{
  
  // Assign priors

  // Population means
  for(m in 1:MAXIMUM){
    for(r in 1:RATIO){
      alphaQN[m, r] ~ beta(2, 2);
      alphaQP[m, r] ~ beta(2, 2);
    }
  }
  betaQ ~ lognormal(1.5, .5);
  betaC ~ normal(0, 2);

  // Declare local variables
  vector[DECISIONS] Q; // Value of each state
  vector[DECISIONS] C; // Choice trace for each state
  vector[DECISIONS] p; // policy

  // Loop over observations
  for (observation in 1:OBSERVATIONS){

      // If new trial, initialize / reset Q values
      if(time[observation] == 0){
        Q = [0.5, 0.5]';
        C = [0, 0]';
      }
      
      // Compute choice probabilities
      p = softmax(betaQ * Q + betaC * C);

      // Sample decision
      decision[observation] ~ categorical(p);
      // In the data, the first decision is derived from the initial Q-values.
      // That is why we aren't sampling decision[observation + 1]

      // Update Q-values. (Players can receive rewards at t == 0)
      if((reward[observation] - Q[decision[observation]]) < 0){
        Q[decision[observation]] = Q[decision[observation]] + alphaQN[maximum[observation], ratio[observation]] *(reward[observation] - Q[decision[observation]]); 
      }else{
        Q[decision[observation]] = Q[decision[observation]] + alphaQP[maximum[observation], ratio[observation]] *(reward[observation] - Q[decision[observation]]); 
      }

      // Update choice trace. Considers previous decision only
      C = [0, 0]';
      C[decision[observation]] = 1;
      
  }
}

