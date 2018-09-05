### This script plots the relationship between sample size, choice sensitivity, and the proportion of H-choices at the participant level

load("../data/pilot/prepared.Rdata")

W <- subset(stat_perpart, cond != "kind")

pdf("../output/pilot/wicked_sens.pdf", width=10, height=5)

par(mfrow=c(1,3))

for (s in c(0.5, 0.75, 1)) {

  w <- subset(W, Hexp_solo == s)
  
  w$H_solo <- w$H_solo + rnorm(nrow(w), mean=0, sd=0.01)
  
  c <- cor(w$samples_solo, w$H_solo)
  m1 <- lm(w$H_solo ~ w$samples_solo)
  m2 <- lm(w$H_solo ~ poly(w$samples_solo, 2))
  
  x <- w$samples_solo 
  y <- w$H_solo 
  m2 <- lm(y ~ poly(x, 2))
  
  
  library(viridis)
  key <- seq(0, 1, by=0.025)
  cols <- inferno(length(key), begin=1, end=.2)
  names(cols) <- key
  w$c <- cols[match(as.character(w$Hexp_solo), as.character(key))]
  
  plot(w$samples_solo, w$H_solo, col=w$c, type="n", xlim=c(0,40), ylim=c(0,1), las=1, xlab="Sample size", ylab="Proportion of H-choices", main=paste("Choice sensitivity = ", s, "\nr = ", round(c, 2), " (N = ", nrow(w), ")", sep=""), cex.main=1.5, cex.axis=1.25, cex.lab=1.25)
  
  points(w$samples_solo, w$H_solo, col=w$c, cex=1.25, pch=16)
  
  
  l <- as.character(seq(0, 1, by=.25))

  abline(m1, lty=2)
  
  x2 <- data.frame(x=seq(0, 40, by=1))
  ext <- predict(m2, newdata=x2)
  
  abline(h=.5)

}

dev.off()