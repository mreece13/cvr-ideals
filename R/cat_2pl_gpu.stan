// Multinomial 2PL IRT Model
// Accounts for differing choice sets for different voters 
// and different candidates available in different races
// vectorized and parallelized version

functions {
  real partial_sum(array[, ] int votes_slice,
                   int start, int end,
                   array[] real alpha,
                   array[,] real beta,
                   array[,] real gamma,
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
      matrix[K, C] logits = rep_matrix(-1e8, K, C); // Initialize logits
      for (k in 1:K) {
        if (eligibility_slice[j, k] == 1) {
          for (c in 1:C) {
            if (candidates[k, c] == 1) {
              logits[k, c] = gamma[k, c] * alpha_slice[j] - (beta[k, c] + mu_beta);
            }
          }
          partial_log_lik += categorical_logit_lpmf(votes_slice[j, k] | logits[k]');
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
  array[K, C] real beta_raw;  // difficulty for each race
  array[K, C] real gamma_raw; // discrimination for each race
  real<lower=0> sigma_beta; // scale of difficulties
  real<lower=0> sigma_gamma; // scale of log discrimination
  
  // fix trump parameter to be greater than 0, 
  // with biden set as reference this imposes ordering constraint
  array[1, 1] real<lower=0> gamma_trump; 
  array[1, 1] real<lower=0> beta_trump; 
}

transformed parameters {
  array[K, C] real beta = rep_array(0, K, C);  // difficulty for each race
  array[K, C] real gamma = rep_array(0, K, C); // discrimination for each race
  
  for (k in 1:K){
    int reference = 1;
    for (c in 1:C){
      if (candidates[k, c] == 1) {
        if (reference == 1){
          reference = 0;
        } else {
          if (k == 1 && c == 2){
            beta[k, c] = beta_trump[1, 1];
            gamma[k, c] = gamma_trump[1, 1];
          } else {
            beta[k, c] = beta_raw[k, c];
            gamma[k, c] = gamma_raw[k, c];
          }
          
        }
      }
    }
  }
}

model {
  // Priors
  mu_beta ~ student_t(3, 0, 2.5);
  alpha ~ std_normal(); // set to N(0, 1) for identification
  
  for (k in 1:K){
    beta_raw[k, ] ~ normal(0, sigma_beta);
    gamma_raw[k,] ~ normal(0, sigma_gamma);
  }
  
  sigma_beta ~ student_t(3, 0, 2.5);
  sigma_gamma ~ student_t(3, 0, 2.5);
  
  if (parallelize == 1){
    int grainsize = 1;

    target += reduce_sum(partial_sum, votes, grainsize, alpha, beta, gamma, mu_beta, candidates, eligibility);
  }
}
