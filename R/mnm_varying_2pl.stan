// Multinomial 2PL IRT Model
// Accounts for differing choice sets for different voters 
// and different candidates available in different races

data {
  int<lower=1> J; // number of voters
  int<lower=1> K; // number of races
  int<lower=1> C; // number of candidates
  array[K, C] int<lower=0, upper=1> candidates; // candidate availability matrix for each race
  array[J, K] int<lower=0, upper=C> votes; // votes matrix (0 if not voting in that race)
  array[J, K] int<lower=0, upper=1> eligibility; // 1 if voter is eligible for race, 0 otherwise
}

parameters {
  // real delta; // mean latent ability
  real mu_beta; // mean question difficulty
  array[J] real alpha; // latent ability of voter j - mean latent ability
  array[K] real beta;  // difficulty for each race
  array[K] real<lower=0> gamma; // discrimination for each race
  real<lower=0> sigma_beta; // scale of difficulties
  real<lower=0> sigma_gamma; // scale of log discrimination
}

model {
  // Priors
  // delta ~ std_normal();
  mu_beta ~ cauchy(0, 5);
  alpha ~ std_normal(); // set to N(0, 1) for identification
  beta ~ normal(0, sigma_beta);
  gamma ~ lognormal(0, sigma_gamma);
  sigma_beta ~ cauchy(0, 5);
  sigma_gamma ~ cauchy(0, 5);

  // Likelihood
  for (j in 1:J) {
    for (k in 1:K) {
      if (eligibility[j, k] == 1) { // Check if the voter is eligible for the race
        vector[C] logits;
        int reference = 1;
        for (c in 1:C) {
          if (candidates[k, c] == 1) { // check if candidate could be voted for in this race
            if (reference == 1){ // set alpha and beta to 0 if reference
              logits[c] = 0;
              reference = 0;
            } else {
              logits[c] = gamma[k] .* (alpha[j] - (beta[k] + mu_beta)); // 2PL IRT model
            }
          } else {
            logits[c] = -1e8; // Large negative value for unavailable candidates
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
