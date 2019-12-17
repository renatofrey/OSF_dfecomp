### This script plots the different motivations underlying the behavior (self-reports)
### Renato Frey

load("../data/main/prepared.Rdata")

library(viridis)
cols <- viridis(2, begin=0.3, end=.6)

library(beanplot)
library(rstanarm)
library(HDInterval)

p_bw <- 2

pdf(file="../output/main/motivation.pdf", width=8, height=3)
par(mar=c(3,4,2,1))
for (i in 1:4) {
 
  if (i == 1) dv <- "strat_evmax"
  if (i == 2) dv <- "strat_maxout"
  if (i == 3) dv <- "strat_befirst"
  if (i == 4) dv <- "strat_tradeoff"
  
  dat <- tapply(participants[,dv], list(participants$mode), list)
  lapply(dat, mean)
  
  # compare groups for first two questions
  if (i <= 2) {
    long <- data.frame(mode="comp", v=dat$comp) 
    long <- rbind(long, data.frame(mode="solo", v=dat$solo))
    long$mode <- as.factor(long$mode)
    long$mode <- relevel(long$mode, ref = "solo")

    mod_rstan <- stan_glm(
      chains = 3,
      iter = 2000,
      formula = "v ~ mode",
      data = long,
      family = gaussian(link = "identity"),
      prior_intercept = normal(location = 0, scale = 10),
      prior = normal(location = 0, scale = 2.5),
      cores = 3, 
      refresh = 0)
    # launch_shinystan(mod_rstan)
    mcmc <- as.data.frame(mod_rstan)
  
    m_solo <- mean(mcmc[,1])
    m_comp <- mean(mcmc[,1] + mcmc[,2])
    out <- round(c(m_solo=mean(mcmc[,1]), hdi(mcmc[,1]),
                   m_comp=mean(mcmc[,1]+mcmc[,2]), hdi(mcmc[,1]+mcmc[,2]),
                   meandiff=mean(mcmc[,2]), hdi(mcmc[,2])), 1)
  }
  
  # only model distribution with an intercept for last two questions
  if (i > 2) {
    long <- data.frame(mode="comp", v=dat$comp) 
    
    mod_rstan <- stan_glm(
      chains = 3,
      iter = 2000,
      formula = "v ~ 1",
      data = long,
      family = gaussian(link = "identity"),
      prior_intercept = normal(location = 0, scale = 10),
      prior = normal(location = 0, scale = 2.5),
      cores = 3,
      refresh = 0)
    # launch_shinystan(mod_rstan)
    mcmc <- as.data.frame(mod_rstan)
    
    m_comp <- mean(mcmc[,1])
    out <- round(c(m=m_comp, hdi(mcmc[,1])), 2)
  }
  
  if (i == 1) do_add = F else do_add = T
  
  b1 <- beanplot(at=i, dat[1], bw=p_bw, what=c(0,1,0,1), method="jitter", jitter=.1, boxwex=1, ll=.05, beanlinewd=1, border=0, ylab="Rating of importance", xlim=c(0.7,4.3), ylim=c(-10,110), col=cols[1], side="second", add=do_add, las=1, cex.axis=.8, cex.lab=.8)
  lines(c(i, i+.3), c(m_comp,m_comp), lwd=2, col="red")
  
  if (i <= 2) {
    b2 <- beanplot(at=i, dat[2], bw=p_bw, what=c(0,1,0,1), method="jitter", jitter=.1, boxwex=1, ll=.05, beanlinewd=1, border=0, yaxt="n", col=cols[2], side="first", add=T)
    lines(c(i, i-.3), c(m_solo,m_solo), lwd=2, col="red")
  }
  
  abline(v=i, lwd=.5)
 
  titles <- c("Choice of option with\nhighest avg. outcome",
              "Choice of option with\nhighest max. outcome",
              "Choice ahead \nof other player ",
              "\"Better\" option (low values)\nvs. choice ahead of other\nplayer (high values)")
  text(x=i, y=-33, xpd=T, titles[i], cex=.8)
   
}

legend(1.85, 140, xpd=T, c("Solitary", "Competitive"), pch=15, col=cols[2:1], horiz=T, box.lty=0)
dev.off()