// Categorical 2PL IRT Model
// Streamlined version
data {
  // int<lower=0, upper=1> parallelize; // should the code be run using within-chain threading?
  int<lower=1> J; // number of voters
  int<lower=1> K; // number of races
  int<lower=1> C; // number of candidates
  array[J, K] int<lower=0, upper=C> votes; // votes matrix (0 if not voting in that race)
  array[K] int<lower=0, upper=C> sizes; // number of candidates in each race
}

parameters {
  real mu_beta; // mean question difficulty
  vector[J] alpha; // latent ability of voter j - mean latent ability
  vector[C] beta_raw;  // difficulty for each race
  vector[C] gamma_raw; // discrimination for each race
  real<lower=0> sigma_beta; // scale of difficulties
  real<lower=0> sigma_gamma; // scale of log discrimination
}

transformed parameters {
  vector[C] beta = rep_vector(0, C);  // ragged difficulty for each race
  vector[C] gamma = rep_vector(0, C); // ragged discrimination for each race
  
  { 
    int pos_params = 0;
    
    for (k in 1:K){
      if (sizes[k] > 0){
        for (c in 2:sizes[k]){
          beta[pos_params + c] = beta_raw[pos_params + c] + mu_beta;
          gamma[pos_params + c] = gamma_raw[pos_params + c];
        }
      }
      pos_params = pos_params + sizes[k];
    }
  }
}

model {
  // Priors
  mu_beta ~ student_t(3, 0, 2.5);
  alpha ~ std_normal(); // set to N(0, 1) for identification
  sigma_beta ~ student_t(3, 0, 2.5);
  sigma_gamma ~ student_t(3, 0, 2.5);
  
  int pos_target = 1;

  for (k in 1:K) {
    int s = sizes[k];
    
    // move on to the next race if we don't use this one for whatever reason
    if (s == 0){
      continue;
    }
    
    // set hierarchical priors
    segment(beta_raw, pos_target, s) ~ normal(0, sigma_beta);
    segment(gamma_raw, pos_target, s) ~ normal(0, sigma_gamma);
    
    // likelihood
    for (j in 1:J){
      if (votes[j, k] > 0){
        votes[j, k] ~ categorical_logit(segment(gamma, pos_target, s) .* (alpha[j] - segment(beta, pos_target, s)));
      }
    }
    
    // iterate to next race
    pos_target += s;
  }
}
