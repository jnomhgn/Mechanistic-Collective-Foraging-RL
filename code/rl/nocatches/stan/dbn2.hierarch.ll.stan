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
  int<lower=1> REWARDS;
  array[OBSERVATIONS] int<lower=0, upper=REWARDS - 1> reward;
}
parameters {
  real logit_alphaQN;
  real logit_alphaQP;
  real log_betaQ;
  real betaC;
  array[MAXIMUM, RATIO] real logit_alphaDBD;
  matrix[5, ID] z;
  vector<lower=0>[5] sigma;
  cholesky_factor_corr[5] cholesky;
}
transformed parameters {
  matrix[ID, 5] idoffset;
  idoffset = (diag_pre_multiply(sigma, cholesky) * z)';
}
model {
  logit_alphaQN ~ normal(0, 1.5);
  logit_alphaQP ~ normal(0, 1.5);
  log_betaQ ~ normal(1.5, .5);
  betaC ~ normal(0, 2);
  for (m in 1 : MAXIMUM) {
    for (r in 1 : RATIO) {
      logit_alphaDBD[m, r] ~ normal(0, 1.5);
    }
  }
  to_vector(z) ~ normal(0, 1);
  sigma ~ exponential(2);
  cholesky ~ lkj_corr_cholesky(2);
  vector[DECISIONS] Q;
  vector[DECISIONS] C;
  vector[DECISIONS] p;
  vector[DECISIONS] psoc;
  real idalphaQ;
  real idbetaQ;
  real idbetaC;
  real idalphaDBD;
  for (observation in 1 : OBSERVATIONS) {
    if (time[observation] == 0) {
      Q = [0.5, 0.5]';
      C = [0, 0]';
    }
    idbetaQ = exp(log_betaQ + idoffset[id[observation], 3]);
    idbetaC = betaC + idoffset[id[observation], 4];
    p = softmax(idbetaQ * Q + idbetaC * C);
    if (time[observation] != 0) {
      idalphaDBD = inv_logit(logit_alphaDBD[maximum[observation], ratio[observation]]
                             + idoffset[id[observation], 5]);
      psoc = to_vector(obsdec[observation,  : ]);
      p = p + idalphaDBD * (psoc - p);
    }
    decision[observation] ~ categorical(p);
    if ((reward[observation] - Q[decision[observation]]) < 0) {
      idalphaQ = inv_logit(logit_alphaQN + idoffset[id[observation], 1]);
    }
    else {
      idalphaQ = inv_logit(logit_alphaQP + idoffset[id[observation], 2]);
    }
    Q[decision[observation]] = Q[decision[observation]]
                               + idalphaQ
                                 * (reward[observation]
                                    - Q[decision[observation]]);
    C = [0, 0]';
    C[decision[observation]] = 1;
  }
}
generated quantities {
  matrix[5, 5] Rho;
  real<lower=0, upper=1> alphaQN;
  array[ID] real<lower=0, upper=1> idalphaQN;
  real<lower=0, upper=1> alphaQP;
  array[ID] real<lower=0, upper=1> idalphaQP;
  real<lower=0> betaQ;
  array[ID] real<lower=0> idbetaQ;
  array[ID] real idbetaC;
  array[MAXIMUM, RATIO] real<lower=0, upper=1> alphaDBD;
  array[ID] matrix<lower=0, upper=1>[MAXIMUM, RATIO] idalphaDBD;
  for (i in 1 : ID) {
    idalphaQN[i] = inv_logit(logit_alphaQN + idoffset[i, 1]);
    idalphaQP[i] = inv_logit(logit_alphaQP + idoffset[i, 2]);
    idbetaQ[i] = exp(log_betaQ + idoffset[i, 3]);
    idbetaC[i] = betaC + idoffset[i, 4];
    for (m in 1 : MAXIMUM) {
      for (r in 1 : RATIO) {
        idalphaDBD[i, m, r] = inv_logit(logit_alphaDBD[m, r] + idoffset[i, 5]);
      }
    }
  }
  alphaQN = mean(idalphaQN);
  alphaQP = mean(idalphaQP);
  betaQ = mean(idbetaQ);
  for (m in 1 : MAXIMUM) {
    for (r in 1 : RATIO) {
      real accumulatedDBD = 0;
      for (i in 1 : ID) {
        accumulatedDBD += idalphaDBD[i, m, r];
      }
      alphaDBD[m, r] = accumulatedDBD / ID;
    }
  }
  Rho = cholesky * cholesky';
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
    p = softmax(idbetaQ[id[observation]] * Q + idbetaC[id[observation]] * C);
    if (time[observation] != 0) {
      psoc = to_vector(obsdec[observation,  : ]);
      p = p
          + idalphaDBD[id[observation], maximum[observation], ratio[observation]]
            * (psoc - p);
    }
    if (time[observation] == 0) {
      log_lik[observation] = positive_infinity();
    }
    else {
      log_lik[observation] = categorical_lpmf(decision[observation]| p);
    }
    if ((reward[observation] - Q[decision[observation]]) < 0) {
      Q[decision[observation]] = Q[decision[observation]]
                                 + idalphaQN[id[observation]]
                                   * (reward[observation]
                                      - Q[decision[observation]]);
    }
    else {
      Q[decision[observation]] = Q[decision[observation]]
                                 + idalphaQP[id[observation]]
                                   * (reward[observation]
                                      - Q[decision[observation]]);
    }
    C = [0, 0]';
    C[decision[observation]] = 1;
  }
}

