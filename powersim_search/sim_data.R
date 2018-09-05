### This script generates new reference and exerpimental samples based on the data of the first two pilot studies
### (c) Renato Frey

load("../data/pilot/prepared.Rdata")

# add study 1 / study 2
stat_perdec$exp <- NA
stat_perdec$exp[which(is.element(stat_perdec$cond, c("kind", "wick0.20")))] <- 1
stat_perdec$exp[which(is.element(stat_perdec$cond, c("wick0.15", "wick0.10")))] <- 2

effsize_eqw <- function(s1, s2) {
  x1 <- mean(s1, na.rm=TRUE)
  x2 <- mean(s2, na.rm=TRUE)
  var1 <- var(s1, na.rm=TRUE)
  var2 <- var(s2, na.rm=TRUE)
  
  d <- (x1 - x2) / sqrt( (var1 + var2) / 2)
  return(d)
}

simulate_group <- function(sel_n, sel_cond, sel_mode, sel_ndecs) {
  if (sel_mode == "between") sel_n <- sel_n * 2
  partids <- subset(participants, cond == sel_cond)$partid
  simpart <- 1
  curr_group <- NULL
  while (simpart <= sel_n) {
    sel_partid <- as.character(sample(partids, 1))
    curr_decs <- NULL
    
    if (mode == "solo" | mode == "comp") {
      sel_decs <- subset(stat_perdec, participant == sel_partid & cond == sel_cond & mode == sel_mode)
      if (length(row.names(sel_decs)) > 0) curr_decs <- stat_perdec[sample(row.names(sel_decs), sel_ndecs, replace=T),]
    }
    
    if (mode == "between") {
      if (simpart <= sel_n/2) sel_decs <- subset(stat_perdec, participant == sel_partid & cond == sel_cond & mode == "solo")
      if (simpart > sel_n/2)  sel_decs <- subset(stat_perdec, participant == sel_partid & cond == sel_cond & mode == "comp")
      if (length(row.names(sel_decs)) > 0) curr_decs <- stat_perdec[sample(row.names(sel_decs), sel_ndecs, replace=T),]
    }
      
    if (mode == "within") {
      sel_decs1 <- subset(stat_perdec, participant == sel_partid & cond == sel_cond & mode == "solo")
      sel_decs2 <- subset(stat_perdec, participant == sel_partid & cond == sel_cond & mode == "comp")
      
      if (length(row.names(sel_decs1)) > 0 & length(row.names(sel_decs2)) > 0) {
        curr_decs1 <- stat_perdec[sample(row.names(sel_decs1), sel_ndecs/2, replace=T),]
        curr_decs2 <- stat_perdec[sample(row.names(sel_decs2), sel_ndecs/2, replace=T),]
        curr_decs <- rbind(curr_decs1, curr_decs2)
      }
    }
    
    if (!is.null(curr_decs)) {  
      curr_decs$participant <- paste("simpart", simpart, sep="")
      curr_decs$gamble_ind <- 1:sel_ndecs
      curr_group <- rbind(curr_group, curr_decs)
      simpart <- simpart+1
    }
  }
  return(curr_group)
}


# randomly sample from existing experimental groups
if (is.na(d)) {
  d_real <- NA
  simdecs <- NULL
  for (cond in c("kind", "wick0.20", "wick0.15", "wick0.10")) {
    simdecs <- rbind(simdecs, simulate_group(n, cond, mode, decs))
  }
}


# simulate experimental group if an effect size is specified
if (!is.na(d)) {
  
  d_real <- -999
  while (d_real < (d - .02) | d_real > (d + .02)) {
    decs_solo <- subset(stat_perdec, mode == "solo")
    
    g1_participants <- subset(participants, cond == "kind")
    g1_partids <- as.character(sample(g1_participants$partid, size=n, replace=T))
    g1_ind <- as.vector(sapply(g1_partids, function(x) {which(decs_solo$participant == x)}))
    decs_g1 <- decs_solo[g1_ind,]
    decs_g1$participant <- rep(paste("simpart", 1:n, sep=""), each=nrow(decs_g1) / n)
    
    m1 <- mean(decs_g1$samples)
    v1 <- var(decs_g1$samples)
    
    v2_planned <- v1
    m2_planned <- m1 - (d * sqrt( (v1 + v2_planned) / 2))
    
    decs_g2 <- decs_g1
    decs_g2$mode <- "comp"
    decs_g2$samples <- round(decs_g2$samples + (m2_planned - m1))
    
    m2 <- mean(decs_g2$samples)
    v2 <- var(decs_g2$samples)
    
    d_real <- round(effsize_eqw(decs_g1$samples, decs_g2$samples), 2)
    
    simdecs <- rbind(decs_g1, decs_g2)
  }
}




if (F) {
  Sys.sleep(1)
  ylim=c(0, 100)
  sel_bw <- 2
  
  h1 <- hist(decs_g1$samples, breaks=seq(-100, 100, by=.25), xlim=c(0, 20), ylim=ylim, las=1, col="blue", border=0, xlab="Sample size", main=paste("Distributions of sample sizes, d =", d, " / ", d_real), xaxt="n")
  axis(1, 0:25)
  
  hist(decs_g2$samples + .2, breaks=seq(-100, 100, by=.25), xlim=c(0, 25), las=1, border=0, col="red", add=T)
  
  d1 <- density(decs_g1$samples, bw=sel_bw, from=0)
  d2 <- density(decs_g2$samples, bw=sel_bw, from=0)
  d1$y <- d1$y * max(ylim) * 7
  d2$y <- d2$y * max(ylim) * 7
  
  points(d1, type="l", col="blue", lwd=2)
  points(d2, type="l", col="red", lwd=2)
}