data {
  int<lower=1> OBSERVATIONS;
  int<lower=1> MAXIMUM;
  array[OBSERVATIONS] int<lower=1, upper=MAXIMUM> maximum;
  int<lower=1> RATIO;
  array[OBSERVATIONS] int<lower=1, upper=RATIO> ratio;
  int<lower=1> PLAYERS;
  int<lower=1> ID;
  array[OBSERVATIONS] int<lower=1, upper=ID> id;
  int<lower=0> TIMES;
  array[OBSERVATIONS] int<lower=0, upper=TIMES> time;
  int<lower=1> DECISIONS;
  array[OBSERVATIONS] int<lower=0, upper=DECISIONS> decision;
  array[OBSERVATIONS, DECISIONS] real<lower=0> obsdec;
  array[OBSERVATIONS] real<lower=0> obsrew;
  int<lower=1> REWARDS;
  array[OBSERVATIONS] int<lower=0, upper=REWARDS - 1> reward;
}
parameters {
  real<lower=0, upper=1> alphaQN;
  real<lower=0, upper=1> alphaQP;
  real<lower=0> betaQ;
  real betaC;
  real<lower=0, upper=1> alphaDBR;
}
model {
  alphaQN ~ beta(2, 2);
  alphaQP ~ beta(2, 2);
  betaQ ~ lognormal(1.5, .5);
  betaC ~ normal(0, 2);
  alphaDBR ~ beta(2, 2);
  vector[DECISIONS] Q;
  vector[DECISIONS] C;
  vector[DECISIONS] p;
  vector[DECISIONS] psoc;
  for (observation in 1 : OBSERVATIONS) {
    if (time[observation] == 0) {
      Q = [0.5, 0.5]';
      C = [0, 0]';
    }
    p = softmax(betaQ * Q + betaC * C);
    if (time[observation] != 0) {
      if (obsrew[observation] != 100) {
        psoc[decision[observation - 1]] = obsrew[observation];
        psoc[3 - decision[observation - 1]] = 1 - obsrew[observation];
        p = p + alphaDBR * (psoc - p);
      }
      decision[observation] ~ categorical(p);
      if ((reward[observation] - Q[decision[observation]]) < 0) {
        Q[decision[observation]] = Q[decision[observation]]
                                   + alphaQN
                                     * (reward[observation]
                                        - Q[decision[observation]]);
      }
      else {
        Q[decision[observation]] = Q[decision[observation]]
                                   + alphaQP
                                     * (reward[observation]
                                        - Q[decision[observation]]);
      }
      C = [0, 0]';
      C[decision[observation]] = 1;
    }
  }
}

