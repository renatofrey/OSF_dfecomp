### This script runs the analyses regarding whether participants track their competitors' behavior in the previous trial, that is, whether they aim to become the "chooser" after having been the "receiver" in the previous trial
### Renato Frey

load("../data/main/prepared.Rdata")

c <- subset(decisions, mode == "comp")
c <- c[order(c$pid, c$gamble_ind),]

c$chooser <- NA
c$chooser[c$role == "chooser"] <- 1
c$chooser[c$role != "chooser"] <- 0

# add variable if participant hat the "passive" role in the previous trial (i.e., either "receiver" or "random assignment of options")
c$passiveprev <- NA
c$passiveprev[2:nrow(c)] <- abs(c$chooser[1:(nrow(c)-1)] - 1)
c$passiveprev[which(c$gamble_ind == 1)] <- NA

library(rstanarm)
library(HDInterval)
options(mc.cores = parallel::detectCores())

mod_rstan <- stan_glmer(
  chains = 3,
  iter = 2000,
  formula = "chooser ~ passiveprev + gamble_ind + (1 + gamble_ind | pid)",
  data = c,
  family = binomial(link="log"),
  #family = gaussian(link="identity"),
  prior_intercept = normal(location = 0, scale = 10),
  prior = normal(location = 0, scale = 2.5))

print(round(summary(mod_rstan)[1:5,], 2))
# launch_shinystan(mod_rstan)

r_prec <- 2
coefs <- round(summary(mod_rstan)[,c("mean", "2.5%", "97.5%")], r_prec)
coefs <- coefs[1:5, ]
coefs <- round(rbind("(Intercept)"=exp(coefs[1,]), exp(coefs[1,] + coefs[2:nrow(coefs),]) - exp(coefs[1,])), r_prec)
print(coefs)