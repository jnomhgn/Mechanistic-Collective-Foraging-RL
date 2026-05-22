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
  real logit_alphaQN;
  real logit_alphaQP;
  real log_betaQ;
  real betaC;
  real logit_alphaVSR;
  real logit_alphaDBD;
}
model {
  logit_alphaQN ~ normal(0, 1.5);
  logit_alphaQP ~ normal(0, 1.5);
  log_betaQ ~ normal(1.5, .5);
  betaC ~ normal(0, 2);
  logit_alphaVSR ~ normal(0, 1.5);
  logit_alphaDBD ~ normal(0, 1.5);
  vector[DECISIONS] Q;
  vector[DECISIONS] C;
  vector[DECISIONS] p;
  vector[DECISIONS] psoc;
  real Qsoc;
  real alphaQ;
  real betaQ;
  real alphaVSR;
  real alphaDBD;
  for (observation in 1 : OBSERVATIONS) {
    if (time[observation] == 0) {
      Q = [0.5, 0.5]';
      C = [0, 0]';
    }
    else {
      alphaVSR = inv_logit(logit_alphaVSR);
      Qsoc = obsrew[observation];
      if (obsrew[observation] != 100) {
        Q[decision[observation - 1]] = Q[decision[observation - 1]]
                                       + alphaVSR
                                         * (Qsoc
                                            - Q[decision[observation - 1]]);
      }
    }
    betaQ = exp(log_betaQ);
    p = softmax(betaQ * Q + betaC * C);
    if (time[observation] != 0) {
      alphaDBD = inv_logit(logit_alphaDBD);
      psoc = to_vector(obsdec[observation,  : ]);
      p = p + alphaDBD * (psoc - p);
    }
    decision[observation] ~ categorical(p);
    if ((reward[observation] - Q[decision[observation]]) < 0) {
      alphaQ = inv_logit(logit_alphaQN);
    }
    else {
      alphaQ = inv_logit(logit_alphaQP);
    }
    Q[decision[observation]] = Q[decision[observation]]
                               + alphaQ
                                 * (reward[observation]
                                    - Q[decision[observation]]);
    C = [0, 0]';
    C[decision[observation]] = 1;
  }
}
generated quantities {
  real<lower=0, upper=1> alphaQN;
  real<lower=0, upper=1> alphaQP;
  real<lower=0> betaQ;
  real<lower=0, upper=1> alphaVSR;
  real<lower=0, upper=1> alphaDBD;
  betaQ = exp(log_betaQ);
  alphaQN = inv_logit(logit_alphaQN);
  alphaQP = inv_logit(logit_alphaQP);
  alphaVSR = inv_logit(logit_alphaVSR);
  alphaDBD = inv_logit(logit_alphaDBD);
  vector[OBSERVATIONS] log_lik;
  vector[DECISIONS] Q;
  vector[DECISIONS] C;
  vector[DECISIONS] p;
  vector[DECISIONS] psoc;
  real Qsoc;
  for (observation in 1 : OBSERVATIONS) {
    if (time[observation] == 0) {
      Q = [0.5, 0.5]';
      C = [0, 0]';
    }
    else {
      Qsoc = obsrew[observation];
      if (obsrew[observation] != 100) {
        Q[decision[observation - 1]] = Q[decision[observation - 1]]
                                       + alphaVSR
                                         * (Qsoc
                                            - Q[decision[observation - 1]]);
      }
    }
    p = softmax(betaQ * Q + betaC * C);
    if (time[observation] != 0) {
      psoc = to_vector(obsdec[observation,  : ]);
      p = p + alphaDBD * (psoc - p);
    }
    if (time[observation] == 0) {
      log_lik[observation] = positive_infinity();
    }
    else {
      log_lik[observation] = categorical_lpmf(decision[observation]| p);
    }
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

