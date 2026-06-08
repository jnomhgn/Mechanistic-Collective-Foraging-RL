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
  array[MAXIMUM, RATIO] real logit_alphaVSD;
  array[MAXIMUM, RATIO] real logit_alphaDBR;
}
model {
  logit_alphaQN ~ normal(0, 1.5);
  logit_alphaQP ~ normal(0, 1.5);
  log_betaQ ~ normal(1.5, .5);
  betaC ~ normal(0, 2);
  for (m in 1 : MAXIMUM) {
    for (r in 1 : RATIO) {
      logit_alphaVSD[m, r] ~ normal(0, 1.5);
      logit_alphaDBR[m, r] ~ normal(0, 1.5);
    }
  }
  vector[DECISIONS] Q;
  vector[DECISIONS] C;
  vector[DECISIONS] p;
  vector[DECISIONS] Qsoc;
  vector[DECISIONS] psoc;
  real alphaQ;
  real betaQ;
  real alphaVSD;
  real alphaDBR;
  for (observation in 1 : OBSERVATIONS) {
    if (time[observation] == 0) {
      Q = [0.5, 0.5]';
      C = [0, 0]';
    }
    else {
      alphaVSD = inv_logit(logit_alphaVSD[maximum[observation], ratio[observation]]);
      Qsoc = to_vector(obsdec[observation,  : ]);
      Q = Q + alphaVSD * (Qsoc - Q);
    }
    betaQ = exp(log_betaQ);
    p = softmax(betaQ * Q + betaC * C);
    if (time[observation] != 0) {
      alphaDBR = inv_logit(logit_alphaDBR[maximum[observation], ratio[observation]]);
      if (obsrew[observation] != 100) {
        psoc[decision[observation - 1]] = obsrew[observation];
        psoc[3 - decision[observation - 1]] = 1 - obsrew[observation];
        p = p + alphaDBR * (psoc - p);
      }
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