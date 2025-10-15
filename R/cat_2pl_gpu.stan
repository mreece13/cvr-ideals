// Categorical 2PL IRT Model
// GPU-accelerated version using OpenCL
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
    
    // Prepare data for GPU-accelerated GLM
    // categorical_logit_glm expects: y | x, alpha, beta
    // where linear predictor for category c is: alpha[c] + x * beta[c]
    // Our model: disc_s[c] * (alpha[j] - diff_s[c]) 
    //          = disc_s[c] * alpha[j] - disc_s[c] * diff_s[c]
    // So: x = alpha (N_voters x 1 matrix), beta = disc_s (1 x s matrix), 
    //     intercept = -disc_s .* diff_s
    
    // Prepare covariate matrix (N_voters x 1)
    matrix[N_voters, 1] x_alpha = to_matrix(alpha, N_voters, 1);
    
    // Prepare coefficient matrix (1 x s) - each column is a category's coefficient
    matrix[1, s] beta_disc = to_matrix(disc_s', 1, s);
    
    // Prepare intercepts (s x 1) - one per category
    vector[s] alpha_intercept = -disc_s .* diff_s;
    
    // Extract votes for this contest
    array[N_voters] int votes_k = votes[, k];
    
    // Count voters who actually voted in this contest
    array[N_voters] int voter_indices;
    int n_voted = 0;
    for (j in 1:N_voters) {
      if (votes_k[j] > 0) {
        n_voted += 1;
        voter_indices[n_voted] = j;
      }
    }
    
    // Only process voters who actually voted
    if (n_voted > 0) {
      array[n_voted] int votes_voted;
      matrix[n_voted, 1] x_voted;
      
      for (i in 1:n_voted) {
        votes_voted[i] = votes_k[voter_indices[i]];
        x_voted[i] = x_alpha[voter_indices[i]];
      }
      
      // GPU-accelerated log-likelihood calculation
      votes_voted ~ categorical_logit_glm(x_voted, alpha_intercept, beta_disc);
    }
    
    // iterate to next race
    pos_target += s;
    
  }
}
