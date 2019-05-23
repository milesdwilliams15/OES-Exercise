
# Regress treatment on covariates
fit = lm(Zdesign ~ X4 + as.factor(block), df)

# Obtain observed heteroskedasticity-robust Wald statistic
# See Wooldridge (2010), p. 62
# Null hypothesis is that the slope coefficients are all zero, i.e.
#  R beta = 0
#  where beta is the k x 1 vector of coefficients, including the intercept
#  and R is the 6 x 7 matrix with all elements zero except
#   R[1,2] = R[2,3] = R[3,4] = R[4,5] = R[5,6] = R[6,7] = 1

Rbeta.hat = coef(fit)[-1]
RVR = vcovHC(fit, type = 'HC0')[-1,-1]
W_obs = as.numeric(Rbeta.hat %*% solve(RVR, Rbeta.hat))  # Wooldridge, equation (4.13)

# Compare to permutation distribution of W

sims <- 10000
W_sims <- numeric(sims)

for(i in 1:sims){
  Z_sim = (df %>%
             group_by(b) %>%
             mutate(Z_sim = sample(Zdesign)))$Z_sim
  fit_sim = lm(Z_sim ~ X4 + as.factor(block), df)
  
  Rbeta.hat = coef(fit_sim)[-1]
  RVR = vcovHC(fit_sim, type = 'HC0')[-1,-1]
  W_sims[i] = as.numeric(Rbeta.hat %*% solve(RVR, Rbeta.hat))
}

# Obtain p-value
pc <- mean(W_sims >= W_obs)
pc


# Regress treatment on covariates
fit = lm(Zdesign ~ X4 + as.factor(block), df)

# Obtain observed heteroskedasticity-robust Wald statistic
# See Wooldridge (2010), p. 62
# Null hypothesis is that the slope coefficients are all zero, i.e.
#  R beta = 0
#  where beta is the k x 1 vector of coefficients, including the intercept
#  and R is the 6 x 7 matrix with all elements zero except
#   R[1,2] = R[2,3] = R[3,4] = R[4,5] = R[5,6] = R[6,7] = 1

Rbeta.hat = coef(fit)[-1]
RVR = vcovHC(fit, type = 'HC0')[-1,-1]
W_obs = as.numeric(Rbeta.hat %*% solve(RVR, Rbeta.hat))  # Wooldridge, equation (4.13)

# Compare to permutation distribution of W

sims <- 10000
W_sims <- numeric(sims)

for(i in 1:sims){
  Z_sim = (df %>%
             group_by(b) %>%
             mutate(Z_sim = sample(Zdesign)))$Z_sim
  fit_sim = lm(Z_sim ~ X4 + as.factor(block), df)
  
  Rbeta.hat = coef(fit_sim)[-1]
  RVR = vcovHC(fit_sim, type = 'HC0')[-1,-1]
  W_sims[i] = as.numeric(Rbeta.hat %*% solve(RVR, Rbeta.hat))
}

# Obtain p-value
pb <- mean(W_sims >= W_obs)
pb
