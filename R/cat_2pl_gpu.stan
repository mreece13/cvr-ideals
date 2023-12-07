// Multinomial 2PL IRT Model
// Accounts for differing choice sets for different voters 
// and different candidates available in different races

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
          beta[k, c] = beta_raw[k, c];
          gamma[k, c] = gamma_raw[k, c];
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
  
  for (k in 1:K){
    
    array[J] int votes_k = to_array_1d(votes[, k]);
    vector[C] intercept = to_array_1d(-(beta[k,] + mu_beta));
    row_vector[] x = ;
    matrix[C] coefficients = gamma[k, ];
    
    target += categorical_logit_glm_lpmf(votes_k, x, intercept, coefficients);
    
  }
  
  target += categorical_logit_glm_lpmf(votes_reshaped, x, alpha_modified, beta_modified);
  
  
}
