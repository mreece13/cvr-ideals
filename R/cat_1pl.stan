// Multinomial 1PL IRT Model
// Accounts for differing choice sets for different voters 
// and different candidates available in different races
// vectorized and parallelized version

functions {
  real partial_sum(array[, ] int votes_slice,
                   int start, int end,
                   array[] real alpha,
                   array[,] real beta,
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
                logits[c] = mu_beta;
                reference = 0;
              } else {
                logits[c] = alpha_slice[j] - (beta[k, c] + mu_beta);
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
  real mu_beta; // mean latent ability
  array[J] real alpha; // latent ability of voter j - mean latent ability
  array[K, C] real beta;  // difficulty for each race
  array[K] real<lower=0> sigma_beta; // hierarchical SD for beta, shared for each race
}

model {
  // Priors
  alpha ~ std_normal();
  mu_beta ~ cauchy(0, 5);
  sigma_beta ~ cauchy(0, 5);
  
  for (k in 1:K){
    beta[k,] ~ normal(0, sigma_beta[k]);
  }
  
  int grainsize = 1;

  target += reduce_sum(partial_sum, votes, grainsize, alpha, beta, mu_beta, candidates, eligibility);
}