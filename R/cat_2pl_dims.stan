// Categorical 2PL IRT Model
// Streamlined version with threading and multiple dimensions
functions {
  matrix whiten(matrix XX) {
    matrix[rows(XX), cols(XX)] DM; // de-meaned XX
    matrix[cols(XX), cols(XX)] SS; // covariance of the columns of XX
    matrix[cols(XX), cols(XX)] PP; // inverse covariance (i.e., precision)
    matrix[cols(XX), cols(XX)] WW; // Cholesky decomposition of precision
    for (m in 1:cols(XX)) {
      DM[, m] = XX[, m] - mean(XX[, m]); // de-mean each column (i.e., factor)
    }
    SS = crossprod(DM) ./ (rows(XX) - 1.0);
    PP = inverse_spd(SS);                   
    WW = cholesky_decompose(PP);
    return DM * WW;             // de-meaned and whitened XX
  }
  real partial_sum(array[] int votes_slice,
                   int start,
                   int end,
                   matrix alpha,
                   vector diff_s,
                   matrix disc_s,
                   int N_dims) {
    real partial_log_lik = 0;
    
    for (j in 1:(end - start + 1)) {
      int voter_idx = start + j - 1;
      if (votes_slice[j] > 0) {
        // For multi-dimensional: linear predictor is disc' * alpha - diff
        // disc_s is [n_cands x N_dims], alpha[voter_idx] is [N_dims x 1]
        // Result is [n_cands x 1] vector of utilities
        vector[rows(disc_s)] utilities = disc_s * alpha[voter_idx]' - diff_s;
        partial_log_lik += categorical_logit_lpmf(votes_slice[j] | utilities);
      }
    }
    
    return partial_log_lik;
  }
}

data {
  int<lower=1> N_voters; // number of voters
  int<lower=1> N_contests; // number of contests
  int<lower=1> N_cands; // number of candidates
  int<lower=1> N_dims; // number of latent dimensions
  array[N_voters, N_contests] int<lower=0, upper=N_cands> votes; // votes matrix (0 if not voting in that race)
  array[N_contests] int<lower=0, upper=N_cands> sizes; // number of candidates in each race
}

parameters {
  real mean_diff; // mean question difficulty
  matrix[N_voters, N_dims] alpha_raw; // raw latent positions of voters (to be whitened)
  vector[N_cands] diff_raw;  // difficulty for each candidate
  matrix[N_cands, N_dims] disc_raw; // discrimination for each candidate (multi-dimensional)
  real<lower=0> sigma_diff; // scale of difficulties
  real<lower=0> sigma_disc; // scale of discriminations
}

transformed parameters {
  vector[N_cands] diff = rep_vector(0, N_cands);  // difficulty for each candidate
  matrix[N_cands, N_dims] disc = rep_matrix(0, N_cands, N_dims); // discrimination for each candidate
  
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
  to_vector(alpha_raw) ~ std_normal(); // set to N(0, 1) for identification
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
    to_vector(disc_raw[pos_target:(pos_target + s - 1)]) ~ normal(0, sigma_disc);
    
    matrix[s, N_dims] disc_s = disc[pos_target:(pos_target + s - 1)];
    vector[s] diff_s = segment(diff, pos_target, s);
    
    // Threaded log-likelihood calculation using reduce_sum
    target += reduce_sum(
      partial_sum,
      votes[, k],  // slice over all voters for contest k
      1,           // grainsize: number of voters per thread (can tune this)
      alpha_raw,   // full alpha matrix [N_voters x N_dims]
      diff_s,      // difficulty parameters for this contest
      disc_s,      // discrimination parameters for this contest [s x N_dims]
      N_dims       // number of dimensions
    );
    
    // iterate to next race
    pos_target += s;
    
  }
}

generated quantities {
  matrix[N_voters, N_dims] alpha = whiten(alpha_raw); // whitened voter positions
}
