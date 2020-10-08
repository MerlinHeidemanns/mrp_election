data {
  int N;
  int T;
  int R;
  int r[N];
  int t[N];
  int N_respondents[N];
  int N_affirmative[N];
  real prior_mu_sigma;
  real prior_theta_sigma;
}

parameters {
  vector[T] mu;
  real<lower = 0> mu_sigma;
  real<lower = 0> sigma;
  matrix[T, R] theta;
  row_vector<lower = 0>[R] theta_sigma;
}

model {
  vector[N] logit_theta;
  for (n_ in 1:N) logit_theta[n_] = theta[t[n_], r[n_]];
  mu_sigma ~ normal(0, prior_mu_sigma);
  theta_sigma ~ normal(0, prior_theta_sigma);
  sigma ~ normal(0, 1);
  mu[1] ~ normal(0, 1);
  for (t_ in 2:T) mu[t_] ~ normal(mu[t_ - 1], mu_sigma);
  for (r_ in 1:R) theta[, r_] ~ normal(mu, sigma);
  for (t_ in 2:T) theta[t_] ~ normal(theta[t_ - 1], theta_sigma);
  target += binomial_logit_lpmf(N_affirmative | N_respondents, logit_theta);
}





