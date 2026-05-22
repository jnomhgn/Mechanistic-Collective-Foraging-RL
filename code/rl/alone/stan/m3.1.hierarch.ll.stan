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
  int<lower=1> REWARDS;
  array[OBSERVATIONS] int<lower=0, upper=REWARDS - 1> reward;
}
parameters {
  real logit_alphaQ;
  real log_betaQ;
  real betaC;
  matrix[3, ID] z;
  vector<lower=0>[3] sigma;
  cholesky_factor_corr[3] cholesky;
}
transformed parameters {
  matrix[ID, 3] idoffset;
  idoffset = (diag_pre_multiply(sigma, cholesky) * z)';
}
model {
  logit_alphaQ ~ normal(0, 1.5);
  log_betaQ ~ normal(1.5, .5);
  betaC ~ normal(0, 2);
  to_vector(z) ~ normal(0, 1);
  sigma ~ exponential(2);
  cholesky ~ lkj_corr_cholesky(2);
  vector[DECISIONS] Q;
  vector[DECISIONS] C;
  vector[DECISIONS] p;
  real idalphaQ;
  real idbetaQ;
  real idbetaC;
  for (observation in 1 : OBSERVATIONS) {
    if (time[observation] == 0) {
      Q = [0.5, 0.5]';
      C = [0, 0]';
    }
    idbetaQ = exp(log_betaQ + idoffset[id[observation], 2]);
    idbetaC = betaC + idoffset[id[observation], 3];
    p = softmax(idbetaQ * Q + idbetaC * C);
    decision[observation] ~ categorical(p);
    idalphaQ = inv_logit(logit_alphaQ + idoffset[id[observation], 1]);
    Q[decision[observation]] = Q[decision[observation]]
                               + idalphaQ
                                 * (reward[observation]
                                    - Q[decision[observation]]);
    C = [0, 0]';
    C[decision[observation]] = 1;
  }
}
generated quantities {
  matrix[3, 3] Rho;
  real<lower=0, upper=1> alphaQ;
  array[ID] real<lower=0, upper=1> idalphaQ;
  real<lower=0> betaQ;
  array[ID] real<lower=0> idbetaQ;
  array[ID] real idbetaC;
  alphaQ = inv_logit(logit_alphaQ);
  betaQ = exp(log_betaQ);
  for (i in 1 : ID) {
    idalphaQ[i] = inv_logit(logit_alphaQ + idoffset[i, 1]);
    idbetaQ[i] = exp(log_betaQ + idoffset[i, 2]);
    idbetaC[i] = betaC + idoffset[i, 3];
  }
  Rho = cholesky * cholesky';
  vector[OBSERVATIONS] log_lik;
  vector[DECISIONS] Q;
  vector[DECISIONS] C;
  vector[DECISIONS] p;
  for (observation in 1 : OBSERVATIONS) {
    if (time[observation] == 0) {
      Q = [0.5, 0.5]';
      C = [0, 0]';
    }
    p = softmax(idbetaQ[id[observation]] * Q + idbetaC[id[observation]] * C);
    if (time[observation] == 0) {
      log_lik[observation] = positive_infinity();
    }
    else {
      log_lik[observation] = categorical_lpmf(decision[observation]| p);
    }
    Q[decision[observation]] = Q[decision[observation]]
                               + idalphaQ[id[observation]]
                                 * (reward[observation]
                                    - Q[decision[observation]]);
    C = [0, 0]';
    C[decision[observation]] = 1;
  }
}

