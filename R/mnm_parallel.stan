functions {
  real partial_sum(int[] slice_y,
                   int start, int end,
                   vector alpha, vector beta, vector gamma,
                   int[] jj, int[] kk,
                   int C) {
    real partial_log_lik = 0;
    array[C] real logit_prob;

    for (n in start:end) {
      for (c in 1:C) {
        logit_prob[c] = gamma[kk[n]] * (alpha[jj[n]] - beta[kk[n]]);
      }
      partial_log_lik += categorical_logit_lpmf(slice_y[n] | logit_prob);
    }
    return partial_log_lik;
  }
}

data {
  int<lower=1> J; // number of voters
  int<lower=1> K; // number of races
  int<lower=1> N; // number of observations
  int<lower=1> C; // number of candidates
  array[N] int<lower=1, upper=J> jj; // voter for observation n
  array[N] int<lower=1, upper=K> kk; // race for observation n
  array[N] int<lower=1, upper=C> y;  // candidate choice for observation n
  array[N] int slices_n; // Slices for parallelization
}

parameters {
  real mu_beta; // mean race difficulty
  vector[J] alpha; // ability for j-mean
  vector[K] beta; // difficulty for k
  vector<lower=0>[K] gamma; // discrimination of k
  real<lower=0> sigma_beta; // scale of difficulties
  real<lower=0> sigma_gamma; // scale of log discrimination
}

model {
  // Priors
  alpha ~ std_normal(); // set to N(0, 1) for identifiabiilty
  beta ~ normal(0, sigma_beta);
  gamma ~ lognormal(0, sigma_gamma);
  mu_beta ~ cauchy(0, 5);
  sigma_beta ~ cauchy(0, 5);
  sigma_gamma ~ cauchy(0, 5);
  
  int grainsize = 1;
  
  target += reduce_sum(partial_sum, y, grainsize, alpha, beta, gamma, jj, kk, C);
}
