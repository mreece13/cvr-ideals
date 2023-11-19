data {
  int<lower=1> J; // number of voters
  int<lower=1> K; // number of races
  int<lower=1> N; // number of observations
  int<lower=1> C; // number of candidates
  array[N] int<lower=1, upper=J> jj; // voter for observation n
  array[N] int<lower=1, upper=K> kk; // race for observation n
  array[N] int<lower=1, upper=C> y;  // candidate choice for observation n
}

parameters {
  real mu_beta; // mean race difficulty
  vector[J] alpha; // latent ability
  vector[K] beta; // difficulty for k
  vector<lower=0>[K] gamma; // discrimination of k
  real<lower=0> sigma_beta; // scale of difficulties
  real<lower=0> sigma_gamma; // scale of log discrimination
}
transformed parameters {
  vector[K] beta;
  beta = mu_beta + sigma_beta*beta_raw;
}

model {
  // Priors
  alpha ~ std_normal(); // set to N(0, 1) for identifiabiilty
  beta_raw ~ std_normal();
  gamma ~ lognormal(0, sigma_gamma);
  mu_beta ~ cauchy(0, 5);
  sigma_beta ~ cauchy(0, 5);
  sigma_gamma ~ cauchy(0, 5);
  
  y ~ categorical_logit(gamma[kk] .* (alpha[jj] - beta[kk]));
}

// loop over races instead to impose the constraint that you can only vote for one person in each race
  
  // y_jk = \gamma_kc (\alpha_j - \beta_kc), for c in 1...c_k-1, c_k in that race
