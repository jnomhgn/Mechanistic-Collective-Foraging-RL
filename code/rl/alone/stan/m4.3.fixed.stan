data {
  int<lower=1> OBSERVATIONS;
  int<lower=1> MAXIMUM;
  array[OBSERVATIONS] int<lower=1, upper=MAXIMUM> maximum;
  int<lower=1> RATIO;
  array[OBSERVATIONS] int<lower=1, upper=RATIO> ratio;
  int<lower=1> PLAYERS;
  int<lower=0> TIMES;
  array[OBSERVATIONS] int<lower=0, upper=TIMES> time;
  int<lower=1> DECISIONS;
  array[OBSERVATIONS] int<lower=0, upper=DECISIONS> decision;
  int<lower=1> REWARDS;
  array[OBSERVATIONS] int<lower=0, upper=REWARDS - 1> reward;
}
parameters {
  array[MAXIMUM, RATIO] real<lower=0, upper=1> alphaQN;
  array[MAXIMUM, RATIO] real<lower=0, upper=1> alphaQP;
  real<lower=0> betaQ;
  real betaC;
}
model {
  for (m in 1 : MAXIMUM) {
    for (r in 1 : RATIO) {
      alphaQN[m, r] ~ beta(2, 2);
      alphaQP[m, r] ~ beta(2, 2);
    }
  }
  betaQ ~ lognormal(1.5, .5);
  betaC ~ normal(0, 2);
  vector[DECISIONS] Q;
  vector[DECISIONS] C;
  vector[DECISIONS] p;
  for (observation in 1 : OBSERVATIONS) {
    if (time[observation] == 0) {
      Q = [0.5, 0.5]';
      C = [0, 0]';
    }
    p = softmax(betaQ * Q + betaC * C);
    decision[observation] ~ categorical(p);
    if ((reward[observation] - Q[decision[observation]]) < 0) {
      Q[decision[observation]] = Q[decision[observation]]
                                 + alphaQN[maximum[observation], ratio[observation]]
                                   * (reward[observation]
                                      - Q[decision[observation]]);
    }
    else {
      Q[decision[observation]] = Q[decision[observation]]
                                 + alphaQP[maximum[observation], ratio[observation]]
                                   * (reward[observation]
                                      - Q[decision[observation]]);
    }
    C = [0, 0]';
    C[decision[observation]] = 1;
  }
}

