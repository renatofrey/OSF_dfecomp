### This script plots the choice options' EVs against their variances
### (c) Renato Frey

dps_k <- read.csv("../data/main/dps_p00.csv")
dps_w20 <- read.csv("../data/main/dps_p20.csv")
dps_w15 <- read.csv("../data/main/dps_p15.csv")
dps_w10 <- read.csv("../data/main/dps_p10.csv")

m <- rbind(apply(dps_k, 2, mean),
           apply(dps_w20, 2, mean),
           apply(dps_w15, 2, mean),
           apply(dps_w10, 2, mean))

v <- rbind(apply(dps_k, 2, var),
           apply(dps_w20, 2, var),
           apply(dps_w15, 2, var),
           apply(dps_w10, 2, var))

# v <- v[,as.vector(t(as.matrix(cbind(1:8, 9:16), byrow=T)))]
#barplot(t(v), beside=T)


pdf(file="../output/main/env_var.pdf", width=10)
par(mfrow=c(2,2)) #     mar=c(3.5,4,3,2), mgp=c(2.2,0.5,0))

for (i in 1:4) {
  
  if (i == 1) p_title <- "Kind"
  if (i == 2) p_title <- "Mildly Wicked" 
  if (i == 3) p_title <- "Moderately Wicked"
  if (i == 4) p_title <- "Extremely Wicked" 
  
  Am <- m[i,1:8]
  Av <- v[i,1:8]
  Bm <- m[i,9:16]
  Bv <- v[i,9:16]
  plot(Am, Av, las=1, pch=16, col="cyan", xlim=c(0,100), ylim=c(0, 500), xlab="Expected value (EV)", ylab="Variance", cex=1.5, main=p_title)
  points(Bm, Bv, pch=16, col="orange")
  for (j in 1:8) {
    lines(c(Am[j], Bm[j]), c(Av[j], Bv[j]), lwd=0.5)
  }
  
  if (i == 2) legend("topright", c("H-options", "L-options"), col=c("cyan", "orange"), pch=18, pt.cex=c(1.5, 1), box.lty=0)
  
}

dev.off()