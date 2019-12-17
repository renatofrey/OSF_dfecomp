### This script plots the expected (based on the pilot data) avg.-cost curves
### Renato Frey

load("../objects/main/sim_choices.Rdata")

library(plotrix)

# get these values from pilot studies
tps_solo <- 5
tps_comp <- 10

init_min <- 1

cols <- c("green", "orange", "red")

pdf(file=paste("../output/main/costs_expected.pdf", sep=""), width=8, height=5)

layout(matrix(rbind(c(2,4,6),
                    c(2,4,6),
                    c(2,4,6),
                    c(2,4,6),
                    c(3,5,7),
                    c(3,5,7),
                    c(3,5,7),
                    c(3,5,7),
                    c(1,1,1)), ncol=3))
par(mar=c(0,0,0,0))
frame()

legend("center", box.lty=0, c("Kind environment", "Moderately wicked env.", "Extremely wicked env."), lty=1, lwd=2, col=cols, cex=1.2, horiz=T)

par(mar=c(4,5,2,1), mgp=c(2.5,1,0))

for (sens in c(.8, .9, 1)) {
  
  ps <- mean_H[[paste("sens_", sens, sep="")]]
  
  s <- seq(2, 20, by=2)
  
  
  
  for (i in 1:2) {
    
    if (i == 1) c <- init_min*60 + s * mean(tps_solo)
    if (i == 2) c <- init_min*60 + s * mean(tps_comp)
    
    # p(EV) with 20 exp and sens = .9
    p0 <- ps[,"p0"]
    p4 <- ps[,"p0.4"]
    p2 <- ps[,"p0.2"]
    
    o0 <- p0 * 2
    o4 <- p4 * 40
    o2 <- p2 * 40
    
    ac0 <- c/o0
    ac4 <- c/o4
    ac2 <- c/o2
    
    best0 <- which.min(ac0)
    best4 <- which.min(ac4)
    best2 <- which.min(ac2)
    
    if (T) {
      ac0 <- log(ac0)
      ac4 <- log(ac4)
      ac2 <- log(ac2)
    }
    
    if (F) {
      o0 <- log(o0)
      o4 <- log(o4)
      o2 <- log(o2)
    }
    
    if (F) {
      plot(s, c, type="b", las=1, xaxt="n", lwd=1)
      axis(1, at=s, s, cex.axis=.7)
      
      plot(s, o0, type="b", las=1, xaxt="n", ylim=range(c(o0, o4, o2)), col="green", lwd=1)
      points(s, o4, type="b", col="blue", lwd=1)
      points(s, o2, type="b", col="red", lwd=1)
      axis(1, at=s, s, cex.axis=.7)
    }
    
    #xl <- range(c(o0, o4, o2))
    xl <- c(0, 40)
    yl <- range(c(ac0, ac4, ac2))
    #yl <- c(1,5)
    
    gmin <- max(c(ac4, ac2)) * 1.01 #2.2 # 
    gmax <- min(ac0) * 0.99 #3.5 # 
    grange <- gmax-gmin
    
    gap.plot(o0, ac0, type="l", las=1, xtics=0, xticlab=0, ytics=0,
             xlim=xl, ylim=yl, col=cols[1], lwd=1, xlab='Expected reward', ylab="Average costs (log)", breakcol="white",
             gap=c(gmin,gmax), gap.axis="y", las=2)
    
    gap.plot(o4, ac4, type="l",
             col=cols[2], lwd=1,
             gap=c(gmin, gmax), gap.axis="y", add=T)
    
    gap.plot(o2, ac2, type="l",
             col=cols[3], lwd=1,
             gap=c(gmin, gmax), gap.axis="y", add=T)
    
    axis(1, at=pretty(xl))
    
    yat <- c(1, 1.5, 2, 2.7, 3.2, 3.7)
    ylab <- round(ifelse(yat>gmin, yat+grange, yat), 1)
    #axis(2, at=yat, ylab, las=1)
    
    axis.break(2, gmin, style="slash")
    
    points(o0[best0], ac0[best0]-grange, pch=19, col=cols[1], cex=1.7)
    points(o4[best4], ac4[best4], pch=19, col=cols[2], cex=1.7)
    points(o2[best2], ac2[best2], pch=19, col=cols[3], cex=1.7)
    
    
    ind1 <- c(1, 2, 3, 4, 5, 6, 8, 10)
    ind2 <- c(1, 2, 3, 4, 5, 6, 8, 10)
    
    tcex <- .8
    text(o0[ind1], ac0[ind1]-grange, as.character(s[ind1]), cex=tcex)
    text(o4[ind2], ac4[ind2], as.character(s[ind2]), cex=tcex)
    text(o2[ind2], ac2[ind2], as.character(s[ind2]), cex=tcex)
    
    if (i == 1) title(paste("Sensitivity = ", sens, sep=""))
    if (i == 1) title("Solitary search", line=-1, cex.main=1)
    if (i == 2) title("Competitive search", line=-1, cex.main=1)
    
    
    
    
    
  }
  
}

dev.off()