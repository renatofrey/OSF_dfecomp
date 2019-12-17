### This file reads in the raw files, removes sensitive information, and stores some intermediate indices for the subsequent analyses
### Renato Frey

# read raw files
participants <- read.csv("../data/raw/main/participants.csv", na.strings="NULL", row.names=1)
sampling <- read.csv("../data/raw/main/sampling.csv", na.strings="NULL", row.names=1)
decisions <- read.csv("../data/raw/main/decisions.csv", na.strings="NULL", row.names=1)

# subset participants who completed the study and got both IMC correct
participants <- subset(participants, status == "done" & imc1 == "correct" & imc2 == "correct")

# remove sensitivy information and unncessary columns
vars_sens <- c("feedback", "http_agent", "ip")
vars_rem <- c("annot", "role", "slot", "status", "code", "rand", "competitor", "gamble_ind")
rem <- c(vars_sens, vars_rem)
participants <- participants[,!is.element(names(participants), rem)]

# convert duration to minutes
participants$duration <- participants$duration/60

# drop levels
participants$pid <- droplevels(participants$pid)
participants$cond <- droplevels(participants$cond)
participants$mode <- droplevels(participants$mode)

# subset data
sampling <- subset(sampling, is.element(pid, participants$pid))
sampling$pid <- droplevels(sampling$pid)
decisions <- subset(decisions, is.element(pid, participants$pid))
decisions$pid <- droplevels(decisions$pid)

# read which pids exist
pids <- sort(levels(participants$pid))

# get sampling statistics
f_switches <- function(options) {
  counter <- 0
  for (i in 2:length(options)) {
    if (options[i] != options[i-1]) counter <- counter + 1
  }
  return(counter)
}

# prepare empty data.frame
stat_perdec <- NULL

# loop through all participants
for (participant in pids) {
  
  part_data <- subset(sampling, pid == participant)
  part_data$dps <- paste(part_data$mode, part_data$gamble_lab, sep="_")
  
  for (gamble in unique(part_data$dps)) {
    
    gamble_data <- subset(part_data, dps == gamble)
    gamble_lab <- gamble_data$gamble_lab[1]
    cond <- as.character(gamble_data$cond[1])
    mode <- as.character(gamble_data$mode[1])
    
    ind_H <- which(gamble_data$sample_opt == "H")
    ind_L <- which(gamble_data$sample_opt == "L")
    
    dps <- read.csv(paste("../data/raw/main/dps_", cond, ".csv", sep=""))
    p <- as.numeric(substr(cond, 2, 3)) / 100
      
    dp_H <- dps[,paste("X", gamble_lab, "H", sep="")]
    dp_L <- dps[,paste("X", gamble_lab, "L", sep="")]
    if (sd(dp_H) > sd(dp_L)) rare_events <- tail(dp_H, nrow(dps)*p)
    if (sd(dp_H) < sd(dp_L)) rare_events <- tail(dp_L, nrow(dps)*p)
    
    rare_events <- rare_events / 100
    
    
    gamble_ok <- FALSE
    gamble_ind <- NA
    samples <- NA
    switches <- NA
    swrate <- NA
    H_mean <- NA
    H_var <- NA
    H_sd <- NA
    H_nrare <- NA
    L_mean <- NA
    L_var <- NA
    L_sd <- NA
    L_nrare <- NA
    HL_ratio <- NA
    HL_diff <- NA
    decision <- NA
    role <- NA
    H <- NA
    Hexp <- NA
    R <- NA
    Rexp <- NA    
    rt_sample <- NA
    rt_decision <- NA
    
    if (nrow(gamble_data) > 0) {
      
      gamble_ok <- TRUE
      
      gamble_ind <- gamble_data$gamble_ind[1]
      samples <- max(gamble_data$sample_ind)
      
      H_mean <- round(mean(gamble_data$sample_out[ind_H]), 2)
      H_var <- round(var(gamble_data$sample_out[ind_H]), 4)
      H_sd <- round(sd(gamble_data$sample_out[ind_H]), 4)
      L_mean <- round(mean(gamble_data$sample_out[ind_L]), 2)
      L_var <- round(var(gamble_data$sample_out[ind_L]), 4)
      L_sd <- round(sd(gamble_data$sample_out[ind_L]), 4)
      HL_ratio <- round(H_mean/L_mean, 2)
      HL_diff <- round(H_mean - L_mean, 2)
      
      dec_data <- subset(decisions, pid == participant & gamble_lab == gamble_data$gamble_lab[1] & mode == as.character(gamble_data$mode[1]))
      
      if (nrow(dec_data) > 1) {
        if (length(unique(dec_data$gamble_ind)) != 1 |
            length(unique(dec_data$gamble_lab)) != 1 |
            length(unique(dec_data$opt)) != 1) {
          print("Warning:")
          print(dec_data)
          dec_data <- dec_data[1,]
        }
      }
      
      if (nrow(dec_data) == 1) {
        decision <- dec_data$opt
        role <- as.character(dec_data$role)
        # H and Hexp
        if (decision == "H") H <- 1 else H <- 0
        if (length(ind_H) > 0 & length(ind_L) > 0) {
          if ((decision == "H" & H_mean > L_mean) | (decision == "L" & H_mean < L_mean)) Hexp <- 1
          if ((decision == "L" & H_mean > L_mean) | (decision == "H" & H_mean < L_mean)) Hexp <- 0
          if (H_mean == L_mean) Hexp <- NA
        }
      }
    }
    
    if (nrow(gamble_data) > 1) {
      switches <- f_switches(as.character(gamble_data$sample_opt))
      swrate <- round(switches / (samples - 1), 2)
    }
    
    # check if rare events were encountered
    n_rare <- NA
    n_rare <- sum(is.element(gamble_data$sample_out, rare_events))
    
    if (gamble_ok == TRUE) {
      
      stat_perdec <- rbind(stat_perdec, data.frame(participant, cond, gamble_ind, mode, role, gamble_lab, samples, H, Hexp, H_mean, L_mean, HL_ratio, HL_diff, H_var, L_var, H_sd, L_sd, n_rare))
      
      stat_perdec$H_var[is.na(stat_perdec$H_var)] <- 0
      stat_perdec$L_var[is.na(stat_perdec$L_var)] <- 0
      
      stat_perdec$H_sd[is.na(stat_perdec$H_sd)] <- 0
      stat_perdec$L_sd[is.na(stat_perdec$L_sd)] <- 0
      
    }
  }
}

stat_perdec$chooser <-NA
stat_perdec$chooser[which(stat_perdec$role == "chooser")] <- 1
stat_perdec$chooser[which(stat_perdec$role == "receiver")] <- 0

# only retain competitive trials in which participant was chooser (for "stat_perdec" only)
stat_perdec <- subset(stat_perdec, mode == "solo" | chooser == 1)

# aggregate trials across participants
tmp1 <- tapply(stat_perdec$samples, list(stat_perdec$participant), mean, na.rm=T)
tmp2 <- tapply(stat_perdec$H, list(stat_perdec$participant), mean, na.rm=T)
tmp3 <- tapply(stat_perdec$Hexp, list(stat_perdec$participant), mean, na.rm=T)
tmp <- cbind(tmp1, tmp2, tmp3)
colnames(tmp) <- c("samples", "H", "Hexp")
stat_perpart <- cbind(participants[match(row.names(tmp), participants$pid), c("pid", "cond", "mode")], tmp)

# how often was participant the chooser?
stat_perpart$n_chooser <- tapply(stat_perdec$chooser, list(stat_perdec$participant), sum, na.rm=T)
participants <- cbind(participants, stat_perpart[match(participants$pid, stat_perpart$pid),])

# prepare decisions
decisions$H <- NA
decisions$H[which(decisions$opt == "H")] <- 1
decisions$H[which(decisions$opt == "L")] <- 0

# sampling stats
sstat <- tapply(sampling$sample_ind, list(sampling$pid, sampling$gamble_ind), max)
sstat <- sstat[match(participants$pid, row.names(sstat)),]

# add average time-per-sample (tps) for each participant
tot_seconds <- unlist(lapply(tapply(sampling$time, list(sampling$pid), range), diff))
tot_samples <- apply(tapply(sampling$sample_ind, list(sampling$pid, sampling$gamble_ind), max, na.rm=T), 1, sum)
tps <- round(tot_seconds/tot_samples, 2)
participants$tps <- tps[match(participants$pid, names(tps))]


# determine lowest point on participants' idiosyncratic average-cost curves
load("../objects/main/sim_choices.Rdata")

# function to get lowest point on individual average-cost curve
get_lowest <- function(row) {
  
  sens <- 1
  cond <- row["cond"]
  tps <- row["tps"]
  
  if (!is.na(tps)) {
    tps <- as.numeric(as.character(tps))
    
    ps <- mean_H[[paste("sens_", sens, sep="")]]
    colnames(ps)[which(colnames(ps) == "p0")] <- "p00"
    colnames(ps)[which(colnames(ps) == "p0.4")] <- "p40"
    colnames(ps)[which(colnames(ps) == "p0.2")] <- "p20"
    
    # samples
    s <- seq(2, 20, by=2)
    
    # costs (inital plus tps)
    c <- 1*60 + s * tps
    
    # probability of choosing higher-EV option as a function of sample size
    pH <- ps[[cond]]
    
    # relative payoff if better option is chosen (2 in the kind and 40 in the wicked env.)
    if (cond == "p00") oH <- pH * 2 else oH <- pH * 40
    
    # average costs for the relative expected outcomes
    ac <- c/oH
    
    best <- which.min(ac)
  } else best <- NA
  
  return(best) 
}

participants$best <- apply(participants, 1, get_lowest)
participants$samp_diff <- round(participants$samples - participants$best, 1)

stat_perdec$best <- participants[match(stat_perdec$participant, participants$pid),"best"]
stat_perdec$samp_diff <- stat_perdec$samples - stat_perdec$best



# write files
write.csv(participants, "../data/main/participants.csv", row.names=F)
write.csv(sampling, "../data/main/sampling.csv", row.names=F)
write.csv(decisions, "../data/main/decisions.csv", row.names=F)
save(participants, pids, sampling, decisions, stat_perdec, stat_perpart, file="../data/main/prepared.Rdata")