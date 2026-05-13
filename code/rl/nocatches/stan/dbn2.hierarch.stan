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

