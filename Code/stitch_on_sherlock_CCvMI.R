


# run this interactively in ml load R or via:
#   sbatch -p qsu,owners,normal /home/groups/manishad/CCvMI/job_stitch.sbatch
# scontrol show job 33834701
# look at its out file:
# cd /home/groups/manishad/CCvMI
# cd /home/users/mmathur
# less rm_stitch.out

# for non-huge simulations, can often run this script interactively in a higher-memory
#  Sherlock session:
# ml load R/4.1.2
# srun --mem=32G --time=3:00:00 --pty bash
# R


# to be run by stitch.sbatch or manually
# To quickly run this script in high-mem interactive session:
# setwd("/home/groups/manishad/CCvMI"); source("stitch_on_sherlock_CCvMI.R")

# # load command line arguments
# args = commandArgs(trailingOnly = TRUE)
# start.num = as.numeric( args[1] )  # starting results number to stitch
# stop.num = as.numeric( args[2] )  # stopping results number to stitch



path = "/home/groups/manishad/CCvMI"
setwd(path)
source("helper_CCvMI.R")
source("analyze_sims_helper_CCvMI.R")

# PRELIMINARIES ----------------------------------------------

library(data.table)
library(dplyr)
library(testthat)
# s = stitch_files(.results.singles.path = "/home/groups/manishad/CCvMI/sim_results/long",
#                  .results.stitched.write.path = "/home/groups/manishad/CCvMI/sim_results/overall_stitched",
#                  .name.prefix = "long_results",
#                  .stitch.file.name="stitched.csv")

.results.singles.path = "/home/groups/manishad/CCvMI/long_results"
.results.stitched.write.path = "/home/groups/manishad/CCvMI/stitched_results"
.name.prefix = "long_results"
.stitch.file.name="stitched.csv"


# MAKE STITCHED DATA ----------------------------------------------

# get list of all files in folder
all.files = list.files(.results.singles.path, full.names=TRUE)

# we only want the ones whose name includes .name.prefix
keepers = all.files[ grep( .name.prefix, all.files ) ]
length(keepers)

# grab variable names from first file
names = names( read.csv(keepers[1] ) )

# read in and rbind the keepers
tables <- lapply( keepers, function(x) read.csv(x, header= TRUE) )

# sanity check: do all files have the same names?
# if not, could be because some jobs were killed early so didn't get doParallelTime
#  variable added at the end
#  can be verified by looking at out-file for a job without name "doParallelTime"
allNames = lapply( tables, names )
# # find out which jobs had wrong number of names
# lapply( allNames, function(x) all.equal(x, names ) )
# allNames[[1]][ !allNames[[1]] %in% allNames[[111]] ]

# bind_rows works even if datasets have different names
#  will fill in NAs
s <- do.call(bind_rows, tables)

names(s) = names( read.csv(keepers[1], header= TRUE) )

if( is.na(s[1,1]) ) s = s[-1,]  # delete annoying NA row
# write.csv(s, paste(.results.stitched.write.path, .stitch.file.name, sep="/") )

cat("\n\n nrow(s) =", nrow(s))
cat("\n nuni(s$scen.name) =", nuni(s$scen.name) )

# debugging help:
# if there's only 1 unique scen, you might have set
#  interactive.cluster.run = TRUE in doParallel


# ~ Check for Bad Column Names ---------------------------

# not sure why this is needed - has NA columns at end
names(s)
any(is.na(names(s)))

if ( any(is.na(names(s))) ) {
  NA.names = which( is.na(names(s) ) )
  s = s[ , -NA.names ]
  
}

s = s %>% filter(!is.na(scen.name))

# ~ Write stitched.csv ---------------------------

setwd(.results.stitched.write.path)
fwrite(s, .stitch.file.name)

# also make a zipped version
string = paste("zip -m stitched.zip", .stitch.file.name)
system(string)




# MAKE AGG DATA ----------------------------------------------

path = "/home/groups/manishad/CCvMI"
setwd(path)
source("helper_CCvMI.R")
source("analyze_sims_helper_CCvMI.R")

# if this says "problem with column OptimConverged", 
#  you just need to comment out the optim columns in make_agg_data
#  because you didn't run those methods
agg = make_agg_data(s)

setwd(.results.stitched.write.path)
fwrite(agg, "agg.csv")


table(agg$method)

cat("\n\n nrow(agg) =", nrow(agg))
cat("\n nuni(agg$scen.name) =", nuni(agg$scen.name) )

# # OPTIONAL: shorter version of agg that's nicer to look at
# agg.short = agg %>% select(prob.hacked, prob.conf, method, Mhat, MhatBias, MhatCover, MhatWidth,
#                            #MhatEstFail, MhatCIFail,
#                            sancheck.dp.k.nonaffirm.unhacked, sancheck.dp.k.nonaffirm.hacked) %>%
#   filter( !( method %in% c("rtma-adj-pmed", "rtma-adj-pmean") ) ) %>%
#   mutate_if(is.numeric, function(x) round(x,2))
#   
# setwd(.results.stitched.write.path)
# fwrite(agg.short, "agg_short.csv")




# look again at failures
t = agg %>% group_by(method) %>%
  summarise( mean( is.na(bhat) ) )
as.data.frame(t)




##### Move to Local #####

# # stitched and agg -> local directory
# scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/CCvMI/stitched_results/* /Users/mmathur/Dropbox/Personal computer/Independent studies/*Inchoate/2022/CCvMI/Linked to OSF (CCvMI)/Simulation study/Data

# scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/CCvMI/stitched_results/* /Users/mmathur/Dropbox/Personal\ computer/Independent\ studies/\*Inchoate/2022/CCvMI/Linked\ to\ OSF\ \(CCvMI\)/Simulation\ study/Data




# LOOK FOR MISSED JOBS ----------------------------------------------

path = "/home/groups/manishad/CCvMI"
setwd(path)
source("helper_CCvMI.R")
source("analyze_sims_helper_CCvMI.R")

# look for missed jobs
missed.nums = sbatch_not_run( "/home/groups/manishad/CCvMI/long_results",
                              "/home/groups/manishad/CCvMI/long_results",
                              .name.prefix = "long",
                              .max.sbatch.num = 108)

# run any missed jobs
setwd( paste(path, "/sbatch_files", sep="") )
for (i in missed.nums) {
  system( paste("sbatch -p qsu,owners,normal /home/groups/manishad/CCvMI/sbatch_files/", i, ".sbatch", sep="") )
}

