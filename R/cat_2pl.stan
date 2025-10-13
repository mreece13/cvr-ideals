// Categorical 2PL IRT Model
// Streamlined version with threading
functions {
  real partial_sum(array[] int votes_slice,
                   int start,
                   int end,
                   vector alpha,
                   vector diff_s,
                   vector disc_s) {
    real partial_log_lik = 0;
    
    for (j in 1:(end - start + 1)) {
      int voter_idx = start + j - 1;
      if (votes_slice[j] > 0) {
        partial_log_lik += categorical_logit_lpmf(votes_slice[j] | disc_s .* (alpha[voter_idx] - diff_s));
      }
    }
    
    return partial_log_lik;
  }
}

data {
  int<lower=1> N_voters; // number of voters
  int<lower=1> N_contests; // number of contests
  int<lower=1> N_cands; // number of candidates
  array[N_voters, N_contests] int<lower=0, upper=N_cands> votes; // votes matrix (0 if not voting in that race)
  array[N_contests] int<lower=0, upper=N_cands> sizes; // number of candidates in each race
}

parameters {
  real mean_diff; // mean question difficulty
  vector[N_voters] alpha; // latent ability of voter j - mean latent ability
  vector[N_cands] diff_raw;  // difficulty for each race
  vector[N_cands] disc_raw; // discrimination for each race
  real<lower=0> sigma_diff; // scale of difficulties
  real<lower=0> sigma_disc; // scale of log discrimination
}

transformed parameters {
  vector[N_cands] diff = rep_vector(0, N_cands);  // ragged difficulty for each race
  vector[N_cands] disc = rep_vector(0, N_cands); // ragged discrimination for each race
  
  { 
    int pos_params = 0;
    
    for (k in 1:N_contests){
      for (c in 2:sizes[k]){
        diff[pos_params + c] = diff_raw[pos_params + c] + mean_diff;
        disc[pos_params + c] = disc_raw[pos_params + c];
      }
      
      pos_params += sizes[k];
    }
  }
}

model {
  // Priors
  mean_diff ~ student_t(3, 0, 2.5);
  alpha ~ std_normal(); // set to N(0, 1) for identification
  sigma_diff ~ student_t(3, 0, 2.5);
  sigma_disc ~ student_t(3, 0, 2.5);
  
  int pos_target = 1;
  
  for (k in 1:N_contests) {
    int s = sizes[k];
    
    // move on to the next race if we don't use this one for whatever reason
    if (s == 0){
      continue;
    }
    
    // set hierarchical priors
    segment(diff_raw, pos_target, s) ~ normal(0, sigma_diff);
    segment(disc_raw, pos_target, s) ~ normal(0, sigma_disc);
    
    vector[s] disc_s = segment(disc, pos_target, s);
    vector[s] diff_s = segment(diff, pos_target, s);
    
    // Threaded log-likelihood calculation using reduce_sum
    target += reduce_sum(
      partial_sum,
      votes[, k],  // slice over all voters for contest k
      1,           // grainsize: number of voters per thread (can tune this)
      alpha,       // full alpha vector
      diff_s,      // difficulty parameters for this contest
      disc_s       // discrimination parameters for this contest
    );
    
    // iterate to next race
    pos_target += s;
    
  }
}
