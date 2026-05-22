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
  array[MAXIMUM, RATIO] real logit_alphaDBDR;
  real logit_sigmaDBDR;
}
model {
  logit_alphaQN ~ normal(0, 1.5);
  logit_alphaQP ~ normal(0, 1.5);
  log_betaQ ~ normal(1.5, .5);
  betaC ~ normal(0, 2);
  logit_sigmaDBDR ~ normal(0, 1.5);
  for (m in 1 : MAXIMUM) {
    for (r in 1 : RATIO) {
      logit_alphaDBDR[m, r] ~ normal(0, 1.5);
    }
  }
  vector[DECISIONS] Q;
  vector[DECISIONS] C;
  vector[DECISIONS] p;
  vector[DECISIONS] psoc;
  real alphaQ;
  real betaQ;
  real alphaDBDR;
  real sigmaDBDR;
  for (observation in 1 : OBSERVATIONS) {
    if (time[observation] == 0) {
      Q = [0.5, 0.5]';
      C = [0, 0]';
    }
    betaQ = exp(log_betaQ);
    p = softmax(betaQ * Q + betaC * C);
    if (time[observation] != 0) {
      alphaDBDR = inv_logit(logit_alphaDBDR[maximum[observation], ratio[observation]]);
      sigmaDBDR = inv_logit(logit_sigmaDBDR);
      if (obsrew[observation] == 100) {
        psoc = to_vector(obsdec[observation,  : ]);
      }
      else {
        psoc[decision[observation - 1]] = (1 - sigmaDBDR)
                                          * obsdec[observation, decision[
                                          observation - 1]]
                                          + sigmaDBDR * obsrew[observation];
        psoc[3 - decision[observation - 1]] = 1
                                              - psoc[decision[observation - 1]];
      }
      p = p + alphaDBDR * (psoc - p);
    }
    decision[observation] ~ categorical(p);
    if ((reward[observation] - Q[decision[observation]]) < 0) {
      alphaQ = inv_logit(logit_alphaQN);
      Q[decision[observation]] = Q[decision[observation]]
                                 + alphaQ
                                   * (reward[observation]
                                      - Q[decision[observation]]);
    }
    else {
      alphaQ = inv_logit(logit_alphaQP);
      Q[decision[observation]] = Q[decision[observation]]
                                 + alphaQ
                                   * (reward[observation]
                                      - Q[decision[observation]]);
    }
    C = [0, 0]';
    C[decision[observation]] = 1;
  }
}
generated quantities {
  real<lower=0, upper=1> alphaQN;
  real<lower=0, upper=1> alphaQP;
  real<lower=0> betaQ;
  array[MAXIMUM, RATIO] real<lower=0, upper=1> alphaDBDR;
  real<lower=0, upper=1> sigmaDBDR;
  alphaQN = inv_logit(logit_alphaQN);
  alphaQP = inv_logit(logit_alphaQP);
  betaQ = exp(log_betaQ);
  sigmaDBDR = inv_logit(logit_sigmaDBDR);
  for (m in 1 : MAXIMUM) {
    for (r in 1 : RATIO) {
      alphaDBDR[m, r] = inv_logit(logit_alphaDBDR[m, r]);
    }
  }
  vector[OBSERVATIONS] log_lik;
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
      if (obsrew[observation] == 100) {
        psoc = to_vector(obsdec[observation,  : ]);
      }
      else {
        psoc[decision[observation - 1]] = (1 - sigmaDBDR)
                                          * obsdec[observation, decision[
                                          observation - 1]]
                                          + sigmaDBDR * obsrew[observation];
        psoc[3 - decision[observation - 1]] = 1
                                              - psoc[decision[observation - 1]];
      }
      p = p + alphaDBDR[maximum[observation], ratio[observation]] * (psoc - p);
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