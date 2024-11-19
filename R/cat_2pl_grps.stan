// Categorical 2PL IRT Model
// Grouped voters
data {
  int<lower=1> N_groups; // number of voters
  int<lower=1> N_contests; // number of contests
  int<lower=1> N_cands; // number of candidates
  array[N_groups, N_contests] int<lower=0, upper=N_cands> votes; // votes matrix (0 if not voting in that race)
  array[N_contests] int<lower=0, upper=N_cands> sizes; // number of candidates in each race
  array[N_groups] int<lower=0> group_sizes; // number of voters in each group of voting patterns
}

parameters {
  real mean_diff; // mean question difficulty
  vector[N_groups] alpha; // latent ability of voter j - mean latent ability
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
    
    for (j in 1:N_groups){
      if (votes[j, k] > 0){
        votes[j, k] ~ categorical_logit((disc_s .* alpha[j] - diff_s) * group_sizes[j]);
      }
    }
    
    // iterate to next race
    pos_target += s;
    
  }
}
