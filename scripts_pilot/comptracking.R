### This script runs the analyses regarding whether participants track their competitors' behavior in previous trials, and whether they aim to become the "chooser" after being the "receiver" in the previous trial
### (c) Renato Frey

load("../data/pilot/prepared.Rdata")

c <- subset(decisions, mode == "comp")
c <- c[order(c$partid, c$gamble_ind),]

# check trials
tmp <- matrix(c$gamble_ind, ncol=4, byrow=T)
sum(tmp[,1] != 2)
sum(tmp[,2] != 4)
sum(tmp[,3] != 6)
sum(tmp[,4] != 8)

c$chooser <- NA
c$chooser[c$role == "chooser"] <- 1
c$chooser[c$role != "chooser"] <- 0

# add variable if participant hat the "passive" role in the previous trial (i.e., either "receiver" or "random assignment of options")
c$passiveprev <- abs(c$chooser - 1)

# count cumulative "passive roles" in previous trials
c$passivecum <- unlist(tapply(c$passiveprev, c$partid, cumsum))

c[which(c$gamble_ind == 8), c("passiveprev", "passivecum")] <- c[which(c$gamble_ind == 6), c("passiveprev", "passivecum")]
c[which(c$gamble_ind == 6), c("passiveprev", "passivecum")] <- c[which(c$gamble_ind == 4), c("passiveprev", "passivecum")]
c[which(c$gamble_ind == 4), c("passiveprev", "passivecum")] <- c[which(c$gamble_ind == 2), c("passiveprev", "passivecum")]
c[which(c$gamble_ind == 2), c("passiveprev", "passivecum")] <- NA


library(rstanarm)
library(BEST)
options(mc.cores = parallel::detectCores())

mod_rstan1 <- stan_glmer(
  chains = 3,
  iter = 2000,
  formula = "chooser ~ passiveprev + gamble_ind + (1 + gamble_ind | partid)",
  data = c,
  family = binomial(link="log"),
  #family = gaussian(link="identity"),
  prior_intercept = normal(location = 0, scale = 10),
  prior = normal(location = 0, scale = 2.5))

mod_rstan2 <- stan_glmer(
  chains = 3,
  iter = 2000,
  formula = "chooser ~ passiveprev + passivecum + gamble_ind + (1 + gamble_ind | partid)",
  data = c,
  family = binomial(link="log"),
  #family = gaussian(link="identity"),
  prior_intercept = normal(location = 0, scale = 10),
  prior = normal(location = 0, scale = 2.5))

print(round(summary(mod_rstan1)[1:5,], 2))
# launch_shinystan(mod_rstan1)


mod_rstan <- mod_rstan1

r_prec <- 2
coefs <- round(summary(mod_rstan)[,c("mean", "2.5%", "97.5%")], r_prec)
coefs <- coefs[1:5, ]
coefs <- round(rbind("(Intercept)"=exp(coefs[1,]), exp(coefs[1,] + coefs[2:nrow(coefs),]) - exp(coefs[1,])), r_prec)
print(coefs)