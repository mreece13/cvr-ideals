// Multinomial 2PL IRT Model
// Accounts for differing choice sets for different voters 
// and different candidates available in different races
// vectorized and parallelized version

functions {
  real partial_sum(array[, ] int votes_slice,
                   int start, int end,
                   array[] real alpha,
                   array[] real beta,
                   array[] real gamma,
                   real mu_beta,
                   array[,] int candidates,
                   array[,] int eligibility) {
    
    int K = dims(candidates)[1];
    int C = dims(candidates)[2];
    int N_slice = end - start + 1;
    array[N_slice] real alpha_slice = alpha[start:end];
    array[N_slice, K] int eligibility_slice = eligibility[start:end, 1:K];
    
    real partial_log_lik = 0;
    
    for (j in 1:N_slice) {
      for (k in 1:K) {
        if (eligibility_slice[j, k] == 1) {
          vector[C] logits = rep_vector(-1e8, C); // Initialize logits
          int reference = 1;
          for (c in 1:C) {
            if (candidates[k, c] == 1) {
              if (reference == 1) {
                logits[c] = 0;
                reference = 0;
              } else {
                logits[c] = gamma[k] .* (alpha_slice[j] - (beta[k] + mu_beta));
              }
            }
          }
          partial_log_lik += categorical_logit_lpmf(votes_slice[j, k] | logits);
        }
      }
    }
    return partial_log_lik;
  }
}

data {
  int<lower=0, upper=1> parallelize; // should the code be run using within-chain threading?
  int<lower=1> J; // number of voters
  int<lower=1> K; // number of races
  int<lower=1> C; // number of candidates
  array[K, C] int<lower=0, upper=1> candidates; // candidate availability matrix for each race
  array[J, K] int<lower=0, upper=C> votes; // votes matrix (0 if not voting in that race)
  array[J, K] int<lower=0, upper=1> eligibility; // 1 if voter is eligible for race, 0 otherwise
}

parameters {
  real mu_beta; // mean question difficulty
  array[J] real alpha; // latent ability of voter j - mean latent ability
  array[K] real beta;  // difficulty for each race
  array[K] real<lower=0> gamma; // discrimination for each race
  real<lower=0> sigma_beta; // scale of difficulties
  real<lower=0> sigma_gamma; // scale of log discrimination
}

model {
  // Priors
  mu_beta ~ cauchy(0, 5);
  alpha ~ std_normal(); // set to N(0, 1) for identification
  beta ~ normal(0, sigma_beta);
  gamma ~ lognormal(0, sigma_gamma);
  sigma_beta ~ cauchy(0, 5);
  sigma_gamma ~ cauchy(0, 5);
  
  if (parallelize == 0){
    // Likelihood
    for (j in 1:J) {
      for (k in 1:K) {
        if (eligibility[j, k] == 1) { // Check if the voter is eligible for the race
          vector[C] logits = rep_vector(-1e8, C); // Initialize logits with large negative values
          int reference = 1;
          for (c in 1:C) {
            if (candidates[k, c] == 1) { // check if candidate could be voted for in this race
              if (reference == 1){ // set alpha and beta to 0 if reference
                logits[c] = 0;
                reference = 0;
              } else {
                logits[c] = gamma[k] .* (alpha[j] - (beta[k] + mu_beta)); // 2PL IRT model
              }
            } 
          }
          target += categorical_logit_lpmf(votes[j, k] | logits);
        }
      }
    }
  }
  if (parallelize == 1){
    int grainsize = 1;

    target += reduce_sum(partial_sum, votes, grainsize, alpha, beta, gamma, mu_beta, candidates, eligibility);
  }
}
