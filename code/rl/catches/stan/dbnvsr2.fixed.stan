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
  array[MAXIMUM, RATIO] real logit_alphaVSR;
  array[MAXIMUM, RATIO] real logit_alphaDBD;
}
model {
  logit_alphaQN ~ normal(0, 1.5);
  logit_alphaQP ~ normal(0, 1.5);
  log_betaQ ~ normal(1.5, .5);
  betaC ~ normal(0, 2);
  for (m in 1 : MAXIMUM) {
    for (r in 1 : RATIO) {
      logit_alphaVSR[m, r] ~ normal(0, 1.5);
      logit_alphaDBD[m, r] ~ normal(0, 1.5);
    }
  }
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
      alphaVSR = inv_logit(logit_alphaVSR[maximum[observation], ratio[observation]]);
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
      alphaDBD = inv_logit(logit_alphaDBD[maximum[observation], ratio[observation]]);
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