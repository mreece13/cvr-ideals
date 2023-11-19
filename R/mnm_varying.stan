data {
  int<lower=1> J; // number of voters
  int<lower=1> K; // number of races
  int<lower=1> C; // number of candidates
  array[K, C] int<lower=0, upper=1> candidates; // candidate availability matrix for each race
  array[J, K] int<lower=0, upper=C> votes; // votes matrix (0 if not voting in that race)
  array[J, K] int<lower=0, upper=1> eligibility; // 1 if voter is eligible for race, 0 otherwise
}

parameters {
  real mu_beta; // mean race difficulty
  array[J] real alpha; // latent ability of voters
  array[K] real beta_raw;  // difficulty for each race
  // array[K] real<lower=0> gamma; // discrimination of each race
  real<lower=0> sigma_beta; // scale of race difficulties
  real<lower=0> sigma_gamma; // scale of log discrimination
  
  real<lower=0> gamma_1; // First discrimination parameter
  array[K-1] real<lower=0> gamma_rest_raw; // Raw values for remaining discrimination parameters
}

transformed parameters {
  array[K] real adjusted_beta;
  for (k in 1:K) {
    adjusted_beta[k] = mu_beta + sigma_beta * beta_raw[k]; // Adjusted race difficulties
  }
  
  array[K] real gamma; // Full gamma array
  gamma[1] = gamma_1; // Fix the first gamma parameter
  for (k in 2:K) {
    gamma[k] = exp(gamma_rest_raw[k-1]); // Reparameterize remaining gammas
  }
}

model {
  // Priors
  alpha ~ std_normal();
  beta_raw ~ std_normal();
  // gamma ~ lognormal(0, sigma_gamma);
  mu_beta ~ cauchy(0, 5);
  sigma_beta ~ cauchy(0, 5);
  // sigma_gamma ~ cauchy(0, 5);
  
  gamma_1 ~ normal(1, 0.1); // Informative prior for the first gamma
  gamma_rest_raw ~ std_normal(); // Standard normal prior for the raw gammas

  // Likelihood
  for (j in 1:J) {
    for (k in 1:K) {
      if (eligibility[j, k] == 1) { // Check if the voter is eligible for the race
        vector[C] logits;
        for (c in 1:C) {
          if (candidates[k, c] == 1) {
            logits[c] = gamma[k] * (alpha[j] - adjusted_beta[k]);
          } else {
            logits[c] = -1e6; // Large negative value for unavailable candidates
          }
        }
        // Calculate the likelihood only if the voter voted in the race
        if (votes[j, k] > 0) {
          target += categorical_logit_lpmf(votes[j, k] | logits);
        }
      }
    }
  }
}
