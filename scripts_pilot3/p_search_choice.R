### This script plots the proportions of higher-EV choices as a function of sample size, and outputs some basic stats
### (c) Renato Frey

load("../data/pilot3/prepared.Rdata")

# rare event observations
round(prop.table(table(stat_perdec$n_rare > 0)), 2)
table(stat_perdec$n_rare)

# some correlations
cor(stat_perdec$samples, stat_perdec$n_rare)
cor(participants$samples, participants$H_solo)

# motivational measures
summary(participants$qual_payoff)
summary(participants$qual_effort)
summary(participants$qual_focused)



pdf("../output/pilot/pilot3.pdf", width=10)

library(viridis)
key <- seq(0, 1, by=0.025)
cols <- inferno(length(key), begin=1, end=.2)
names(cols) <- key
mycols <- cols[match(as.character(participants$Hexp_solo), as.character(key))]

# define semi-transparent colors for HDIs
r2 <- data.frame("red" = 255,
                 "green" = 0,
                 "blue" = 0,
                 "alpha" = 50)
r2 <- rgb(r2[,1], r2[,2], r2[,3], r2[,4], maxColorValue=255)

# define variables
v1 <- "samples_solo"
v2 <- "H_solo"

c <- cor(participants[,v1], participants[,v2])

plot(participants[,v1], participants[,v2], las=1, cex=1.5, xlab="Sample size", ylab="Proportion of H-choices", type="n", ylim=c(0,1))

if (T) {
  load("../objects/pilot3/model_samplesize.Rdata")
  print(round(estimates[c("mean", "lower", "upper")], 1))
  ex <- estimates
  #rect(ex[1,"lower"], -99, ex[1,"upper"], 99, col="red", density=10, border=NA)
  #abline(v=ex[1,"mean",1], lwd=2, col="darkred")
}

if (T) {
  load("../objects/pilot3/model_H.Rdata")
  print(round(estimates[c("mean", "lower", "upper")], 1))
  ey <- estimates
  #rect(-99, ey[1,"lower"], 99, ey[1,"upper"], col="lightgrey", density=30, border=NA)
  #abline(h=ey[1,"mean",1], lwd=2, col="darkred")
}

if (F) {
  load("../objects/pilot3/model_Hexp.Rdata")
  print(round(estimates[c("mean", "lower", "upper")], 1))
}

rect(ex[1,"lower"], ey[1,"lower"], ex[1,"upper"], ey[1,"upper"], col=r2, border=NA)
lines(x=c(ex[1,"lower"], ex[1,"upper"]), y=c(ey[1,"mean"], ey[1,"mean"]), col="darkred", lwd=2, lend=3)
lines(x=c(ex[1,"mean"], ex[1,"mean"]), y=c(ey[1,"lower"], ey[1,"upper"]), col="darkred", lwd=2, lend=3)


points(participants[,v1], participants[,v2], pch=16, cex=1.25, col=mycols)
abline(h=.5, lty=3, lwd=.5)


# fit quadratic relationship
x <- participants[,v1] 
y <- participants[,v2] 
m2 <- lm(y ~ poly(x, 2))

x2 <- data.frame(x=seq(0, 30, by=1))
ext <- predict(m2, newdata=x2)
points(x2$x, ext, type="l", lty=2)


x <- as.character(seq(0.5, 1, by=.1))
legend("bottomright", title="Choice sensitivity", title.adj=.5, x, pch=16, col=cols[x], box.lty=0, bg="transparent", horiz=T)

legend("topleft", paste("r = ", round(c, 2), " (N = ", nrow(participants), ")", sep=""), box.lty=0)


dev.off()