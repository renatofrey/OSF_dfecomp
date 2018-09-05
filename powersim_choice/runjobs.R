### This script creates separate jobs for the regression models (to be run on a Linux cluster with SLURM queuing system)
### (c) Renato Frey

task <- "powersim_choice"

dir_out <- "~/Data/dfe_comp/powersim_choice"
dir_log <- "~/Data/dfe_comp/powersim_choice_log"

# remove and (re)-create directories
if (file.exists(dir_out) == T) system(paste("rm -r ", dir_out, sep=""))
if (file.exists(dir_log) == T) system(paste("rm -r ", dir_log, sep=""))
system(paste("mkdir -p ", dir_out, sep=""))
system(paste("mkdir -p ", dir_log, sep=""))

joblist <- expand.grid(sim=1:250, n=c(30,40,50))

# loop through jobs
for (job in 1:nrow(joblist)) {

  print(paste("Submitting job ", task, job, sep=""))

  job_args <- paste("--args task='", task, "' job='", job, "' sim='", joblist[job,"sim"], "' n='", joblist[job,"n"], "'", sep="") 

  job_string <- paste("#!/bin/bash 
#SBATCH --job-name=", task, formatC(job, width=3, flag="0"), "
#SBATCH --output=/dev/null
#SBATCH --cpus-per-task=3
#SBATCH --time=24:00:00
#SBATCH --qos=1day
module load R
module load JAGS
R CMD BATCH --no-save --no-restore --slave \"", job_args, "\" fit_rstanarm.R ", dir_log, "/Rout", formatC(job, width=3, flag="0"), ".txt", sep="")

  write(job_string, file="slurm_script") 
  system("sbatch slurm_script")

  
}
