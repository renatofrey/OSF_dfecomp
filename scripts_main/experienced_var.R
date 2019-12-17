### This script analyzes and plots the experienced variance
### Renato Frey

load("../data/main/prepared.Rdata")

expvars <- tapply(sampling$sample_out, list(sampling$pid, sampling$gamble_lab), sd, na.rm=T)

expvar_per_pid <- apply(expvars, 1, mean, na.rm=T)

participants$expvar <- expvar_per_pid[match(participants$pid, names(expvar_per_pid))]

p_solo <- subset(participants, mode == "solo")
p_comp <- subset(participants, mode == "comp")
print(cor(p_solo$expvar, p_solo$samples, use="complete.obs"))
print(cor(p_comp$expvar, p_comp$samples, use="complete.obs"))


expvar_means <- tapply(participants$expvar, list(participants$mode, participants$cond), mean, na.rm=T)
expvar_means <- expvar_means[2:1, c(1,3,2)]

participants$mode <- relevel(participants$mode, ref = "solo")


library(viridis)
cols <- viridis(2, begin=0.3, end=.6)

library(beanplot)

pdf(file="../output/main/exp_variance.pdf", width=8, height=3)
par(mar=c(3,4,2,1))

p_bw <- .25

for (i in 1:3) {
  
  if (i == 1) dat <- subset(participants, cond == "p00")
  if (i == 2) dat <- subset(participants, cond == "p40")
  if (i == 3) dat <- subset(participants, cond == "p20")
  
  dat <- tapply(dat$expvar, list(dat$mode), list)
  
  if (i == 1) do_add = F else do_add = T
  
  b1 <- beanplot(at=i, dat$comp, bw=p_bw, what=c(0,1,1,1), method="jitter", jitter=.025, boxwex=.75, ll=.25, beanlinewd=2, border=0, ylab="Experienced variance", xlim=c(0.7,3.3), ylim=c(-.1,1.1), col=c(cols[1], "black", "black", "red"), side="second", add=do_add, las=1, cex.axis=.8, cex.lab=.8)

  b2 <- beanplot(at=i, dat$solo, bw=p_bw, what=c(0,1,1,1), method="jitter", jitter=.025, boxwex=.75, ll=.25, beanlinewd=2, border=0, yaxt="n", col=c(cols[2], "black", "black", "red"),side="first", add=T)

  abline(v=i, lwd=.5)
  
  titles <- c("Kind environment",
              "Moderately wicked env.",
              "Extremely wicked env.")
  text(x=i, y=-0.3, xpd=T, titles[i], cex=.8)
  
}

legend(1.55, 1.4, xpd=T, c("Solitary", "Competitive"), pch=15, col=cols[2:1], horiz=T, box.lty=0)
dev.off()