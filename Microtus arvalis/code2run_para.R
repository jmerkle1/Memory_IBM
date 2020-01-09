# This code runs the information transfer IBM (info.transfer.IBM.R) and the plots the results
# Written by Zach Bell and Jerod Merkle
# June 2019


#source the function
source("C:/Users/Zach/Documents/GitHub/Memory_IBM/Memory_IBM/info.transfer.IBM.R")
source("C:/Users/Zach/Documents/GitHub/Memory_IBM/Memory_IBM/plotInfoTransferIBM.R")
args <- list(0.20, #increase in probability of death for uninformed
             c(0.01, .05, .10, .15, .20), # naive learning probability of the oldest animals (i.e., the ones that have the highest naive learning)
             2, # maximum mean (i.e., lambda of poison distribution) number of interactions per pair (if animal has 1 bold, it interacts with an animal with 1 boldness, and population is at or above K, this is the lambda of the interaction distributions)
             0.03, # given an interaction, what is the probability that information is transfered (min=0, max=1)
             2000, # carrying capacity
             200, # starting number of individuals
             25, # how many years should the simulation run for?
             0.46, #what is the sex ratio of of the population/births?
             1.5, # lambda value for starting age distribution based on poison distribution
             ".5 5", # starting probability distribution of knowing information; beta distribution ranges from 0 to 1 (vector of 2 values: shape1 and shape2)
             "2 5", # starting probability distribution of being bold, beta distribution (vector of 2 values: shape1 and shape2)
             "C:/Users/Zach/Documents/GitHub/Memory_IBM/Memory_IBM/Microtus arvalis/ageClass_MicroArv.csv", #dataframe of age based birth and death rate for FEMALES only. The columns should be age, ageClass, birthRate, and survivalRate, in that order.
             "C:/Users/Zach/Desktop/results/MicroArv", #an empty folder where results will be saved.
             FALSE, # want to make results reproducible? Then set as TRUE
             TRUE, #should all results be written to file at each time step?
             1, # When giving birth, should your information status be given to your offspring? 0 if false, 1 if true (i.e., is there vertical transmission of information?)
             0, #density dependence of interactions, set to 1 for positive density dependence, 0 for none, -1 for negative density dependence
             .4)

args <- expand.grid(args)
time1 <- Sys.time()
results.folders <- paste("C:/Users/Zach/Desktop/results/MicroArv", 1:nrow(args), sep = "/")
require(snowfall)
sfInit(parallel = TRUE, cpus = 2)
sfLibrary(igraph, Matrix, stringr)
sfExport("args", "info.tansfer.IBM")
sfClusterApplyLB(1:nrow(args), info.transfer.IBM)
sfExport("results.folders", "plotInfoTransferIBM")
sfClusterApplyLB(1:length(results.folders), plotInfoTransferIBM)
time2 <- Sys.time()
Sys.sleep(.001) 
print(time2 - time1)
