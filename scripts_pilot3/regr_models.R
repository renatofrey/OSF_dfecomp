### This script runs the regression models
### (c) Renato Frey

set.seed(777)

library(lme4)
library(rstanarm)
library(BEST)
options(mc.cores = parallel::detectCores())

load("../data/pilot3/prepared.Rdata")

predictors <- "1 + gamble_ind + (1 + gamble_ind | participant)"
n_pred <- 2

# center trial on 0
stat_perdec$gamble_ind <- stat_perdec$gamble_ind - 1

for (i in 1:3) {
  
  if (i == 1) {
    DV_label <- "Sample size" 
    
    mod_rstan <- stan_glmer(
      chains = 3,
      iter = 2000,
      formula = paste("samples ~ ", predictors, sep=""),
      data = stat_perdec,
      family = poisson(link="identity"),
      prior_intercept = normal(location = 0, scale = 10),
      prior = normal(location = 0, scale = 2.5))
    # launch_shinystan(mod_rstan)
  }
  
  if (i == 2) {
    DV_label <- "Choice of H" 
    
    mod_rstan <- stan_glmer(
      chains = 3,
      iter = 2000,
      formula = paste("H ~ ", predictors, sep=""),
      data = stat_perdec,
      family = binomial(link="log"),
      prior_intercept = normal(location = 0, scale = 10),
      prior = normal(location = 0, scale = 2.5))
    # launch_shinystan(mod_rstan)
  }
  
  if (i == 3) {
    DV_label <- "Choice of Hexp" 
    
    mod_rstan <- stan_glmer(
      chains = 3,
      iter = 2000,
      formula = paste("Hexp ~ ", predictors, sep=""),
      data = stat_perdec,
      family = binomial(link="log"),
      prior_intercept = normal(location = 0, scale = 10),
      prior = normal(location = 0, scale = 2.5))
    # launch_shinystan(mod_rstan)
  }
  
  
  mcmc <- as.data.frame(mod_rstan)[,1:n]
  
  estimates <- NULL
  estimates_net <- NULL
  for (curr_cond in c("wick0.20")) {
    for (curr_mode in c("solo")) {
      
      curr_mcmc <- 0
      
      estimates_net <- rbind(estimates_net, data.frame(cond=curr_cond, mode=curr_mode, mean=mean(curr_mcmc), rbind(hdi(curr_mcmc)[1:2])))
      
      curr_mcmc <- curr_mcmc + mcmc[,"(Intercept)"]
      
      estimates <- rbind(estimates, data.frame(cond="wick0.20", mode="solo", mean=mean(curr_mcmc), rbind(hdi(curr_mcmc)[1:2])))
      
    }
  }
  
  if (i == 2 | i == 3) {
      estimates_net[,3:5] <- exp(estimates_net[,3:5])
      estimates[,3:5] <- exp(estimates[,3:5])
  }
  
  if (i == 1) save(mod_rstan, DV_label, mcmc, estimates_net, estimates, file="../objects/pilot3/model_samplesize.Rdata")
  if (i == 2) save(mod_rstan, DV_label, mcmc, estimates_net, estimates, file="../objects/pilot3/model_H.Rdata")
  if (i == 3) save(mod_rstan, DV_label, mcmc, estimates_net, estimates, file="../objects/pilot3/model_Hexp.Rdata")
  
  print(esimates)
  
}