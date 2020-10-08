# libraries
library(tidyverse)
library(boot)
library(DirichletReg)
# fun
source("code/functions/functions.R")
# scope
T <- 50
R <- 4
R_sigma <- abs(rnorm(R, 0, 0.3))
mu <- rnorm(1, 0, 1)
mu_sigma <- abs(rnorm(1, 0, 0.5))
sigma <- abs(rnorm(1, 0, 0.5))
R_mu <- rnorm(R, mu, 0.5)
# containers
walk_mat <- matrix(NA, nrow = T, ncol = R + 1)
# walk
walk_mat[1,1:R] <- rnorm(R, R_mu, R_sigma)
walk_mat[1,R + 1] <- mu
for (t in 2:T) walk_mat[t, R + 1] <- rnorm(1, walk_mat[t - 1, R + 1], mu_sigma)
for (t in 2:T) walk_mat[t, 1:R] <- (rnorm(R, walk_mat[t - 1, ], R_sigma) + rnorm(R, walk_mat[t, R + 1], sigma))/2
# plot
df <- as.data.frame(walk_mat)
colnames(df) <- c("1", "2", "3", "4", "mu")
df <- df %>% 
  pivot_longer(cols = everything(), names_to = "group", values_to = "mean") %>%
  mutate(mean = inv.logit(mean)) %>%
  arrange(group) %>%
  add_column(t = rep(seq(1,T), R + 1))
ggplot(data = df, aes(x = t, y = mean, color = group)) + 
  geom_line() + 
  theme_bw() +
  labs(y = "Share", x = "Time")
# sim data
n_t <- 5
t <- sample(1:T, n_t, replace = FALSE)
N <- floor(runif(n_t, 10000, 15000))
N_r <- floor(rdirichlet(5, rep(10, 4)) * N)
df_data <- data.frame(cbind(N_r, t)) %>%
  pivot_longer(starts_with("V"), names_to = "group", values_to = "N_respondents") %>%
  mutate(group = gsub("V", "", group)) %>%
  inner_join(df, by = c("group", "t")) %>%
  mutate(N_affirmative = rbinom(nrow(.), N_respondents, mean))
# model
m1 <- rstan::stan_model("code/stan/mrp_rw_pp.stan")
data <- list(
  N = nrow(df_data),
  T = T,
  R = length(unique(df_data$group)),
  r = as.integer(df_data$group),
  t = df_data$t,
  N_respondents = df_data$N_respondents,
  N_affirmative = df_data$N_affirmative,
  prior_mu_sigma = 1,
  prior_theta_sigma = 1
)
fit <- rstan::sampling(m1, data = data, cores = 4)
# extract data
# mu
mu <- inv.logit(rstan::extract(fit, pars = "mu")[[1]])
mu_sum <- extract_quantiles_mean(mu, c(2), 50)
ggplot(data = mu_sum, aes(x = t, y = median)) + 
  geom_line(size = 1) + 
  geom_ribbon(aes(x = t, ymax = q25, ymin =  q75), alpha = 0.5) + 
  geom_ribbon(aes(x = t, ymax = q10, ymin = q90), alpha = 0.25) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(5, 45, 10)) +
  labs(x = "time", 
       y = "Share", 
       caption = "Line: Median
                  Inner band: 50%
                  Outer band: 80%")
# subgroups
# mu
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]])
theta_sum <- extract_quantiles_mean(mu, c(2,3), 50)
ggplot(data = mu_sum, aes(x = t, y = median)) + 
  geom_line(size = 1) + 
  geom_ribbon(aes(x = t, ymax = q25, ymin =  q75), alpha = 0.5) + 
  geom_ribbon(aes(x = t, ymax = q10, ymin = q90), alpha = 0.25) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(5, 45, 10)) +
  labs(x = "time", 
       y = "Share", 
       caption = "Line: Median
                  Inner band: 50%
                  Outer band: 80%")




