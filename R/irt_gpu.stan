// Categorical 2PL IRT Model -- GPU Version (Ragged Array)

data {
  int<lower=1> N_voters; // number of voters
  int<lower=1> N_contests; // number of contests
  int<lower=1> N_cands; // number of candidates
  int<lower=1> N_votes; // total number of actual votes cast
  array[N_contests] int<lower=0> n_votes_per_contest; // votes in each contest
  array[N_votes] int<lower=1, upper=N_voters> voter_id; // which voter (flat, organized by contest)
  array[N_votes] int<lower=0, upper=N_cands> vote; // which candidate (flat, organized by contest)
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
  
  profile("transformed_parameters") {
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
  profile("priors") {
    mean_diff ~ student_t(3, 0, 2.5);
    alpha ~ std_normal(); // set to N(0, 1) for identification
    sigma_diff ~ student_t(3, 0, 2.5);
    sigma_disc ~ student_t(3, 0, 2.5);
  }
  
  int pos_target = 1;  // Position in parameter arrays
  int pos_votes = 1;   // Position in vote arrays
  
  profile("contest_loop") {
    for (k in 1:N_contests) {
      int s = sizes[k];
      int n_v = n_votes_per_contest[k];  // Number of votes in this contest
      
      // move on to the next race if we don't use this one for whatever reason
      if (s == 0){
        pos_votes += n_v;  // Still need to advance vote position
        continue;
      }
      
      // set hierarchical priors
      segment(diff_raw, pos_target, s) ~ normal(0, sigma_diff);
      segment(disc_raw, pos_target, s) ~ normal(0, sigma_disc);
      
      vector[s] disc_s = segment(disc, pos_target, s);
      vector[s] diff_s = segment(diff, pos_target, s);
      
      // Get the votes for this contest
      array[n_v] int voter_ids_k = segment(voter_id, pos_votes, n_v);
      array[n_v] int votes_k = segment(vote, pos_votes, n_v);
      
      profile("construct_design_matrix") {
        // Construct design matrix for GLM form: X * beta
        // X[i, 1] = alpha[voter_ids_k[i]] (voter ability)
        // X[i, 2] = 1 (constant for difficulty/intercept)
        // Then beta[1, c] = disc_s[c], beta[2, c] = diff_s[c]
        // Linear predictor: disc_s[c] * alpha[j] - diff_s[c]
        
        matrix[n_v, 2] X = rep_matrix(0, n_v, 2);
        for (i in 1:n_v) {
          X[i, 1] = alpha[voter_ids_k[i]];  // voter ability
          X[i, 2] = 1;                       // constant column
        }
        
        // Coefficient matrix: beta[feature, category]
        matrix[2, s] beta = rep_matrix(0, 2, s);
        for (c in 1:s) {
          beta[1, c] = disc_s[c];   // discrimination coefficient for candidate c
          beta[2, c] = -diff_s[c];  // negative difficulty (offset)
        }
        
        profile("glm_likelihood") {
          // categorical_logit_glm_lpmf(y | X, alpha, beta)
          // Computes: X * beta for linear predictors, then categorical_logit
          target += categorical_logit_glm_lpmf(votes_k | X, rep_vector(0, s), beta);
        }
      }
      
      // iterate to next race
      pos_target += s;
      pos_votes += n_v;
    }
  }
}
