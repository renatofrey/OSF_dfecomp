### This script runs the regression models
### (c) Renato Frey

set.seed(777)

library(lme4)
library(rstanarm)
library(BEST)
options(mc.cores = parallel::detectCores())

load("../data/pilot/prepared.Rdata")

# add study 1 / study 2
stat_perdec$exp <- NA
stat_perdec$exp[which(is.element(stat_perdec$cond, c("kind", "wick0.20")))] <- 1
stat_perdec$exp[which(is.element(stat_perdec$cond, c("wick0.15", "wick0.10")))] <- 2

#predictors <- "1 + cond + gamble_ind + mode + (1 + mode + gamble_ind | participant)"
#n <- 6

predictors <- "1 + cond * mode + gamble_ind + (1 + mode + gamble_ind | participant)"
n <- 9

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
  for (curr_cond in c("kind", "wick0.20", "wick0.15", "wick0.10")) {
    for (curr_mode in c("solo", "comp")) {
      
      curr_mcmc <- 0
      
      if (curr_mode == "comp") curr_mcmc <- curr_mcmc + mcmc[,"modecomp"]
      if (curr_cond != "kind") curr_mcmc <- curr_mcmc + mcmc[,paste("cond", curr_cond, sep="")]
      
      interact <- paste("cond", curr_cond, ":", "mode", curr_mode, sep="")
      if (is.element(interact, colnames(mcmc))) curr_mcmc <- curr_mcmc + mcmc[,interact]
      
      estimates_net <- rbind(estimates_net, data.frame(cond=curr_cond, mode=curr_mode, mean=mean(curr_mcmc), rbind(hdi(curr_mcmc)[1:2])))
      
      curr_mcmc <- curr_mcmc + mcmc[,"(Intercept)"]
      
      estimates <- rbind(estimates, data.frame(cond=curr_cond, mode=curr_mode, mean=mean(curr_mcmc), rbind(hdi(curr_mcmc)[1:2])))
      
    }
  }
  
  if (i == 2 | i == 3) {
      estimates_net[,3:5] <- exp(estimates_net[,3:5])
      estimates[,3:5] <- exp(estimates[,3:5])
  }
  
  if (i == 1) save(mod_rstan, DV_label, mcmc, estimates_net, estimates, file="../objects/pilot/model_samplesize.Rdata")
  if (i == 2) save(mod_rstan, DV_label, mcmc, estimates_net, estimates, file="../objects/pilot/model_H.Rdata")
  if (i == 3) save(mod_rstan, DV_label, mcmc, estimates_net, estimates, file="../objects/pilot/model_Hexp.Rdata")
  
}