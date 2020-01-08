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
for(i in 1:nrow(args)){
  info.transfer.IBM(h = args[i, 1], #increase in probability of death for uninformed
                  nl = args[i, 2], # naive learning probability of the oldest animals (i.e., the ones that have the highest naive learning)
                  si = args[i, 3], # maximum mean (i.e., lambda of poison distribution) number of interactions per pair (if animal has 1 bold, it interacts with an animal with 1 boldness, and population is at or above K, this is the lambda of the interaction distributions)
                  infotransfer = args[i, 4], # given an interaction, what is the probability that information is transfered (min=0, max=1)
                  K = args[i, 5], # carrying capacity
                  N0 = args[i, 6], # starting number of individuals
                  t = args[i, 7], # how many years should the simulation run for?
                  sex.ratio = args[i, 8], #what is the sex ratio of of the population/births?
                  age.distr.lamba = args[i, 9], # lambda value for starting age distribution based on poison distribution
                  informed.distr.beta = as.numeric(unlist(strsplit(as.character(args[i, 10]), " "))), # starting probability distribution of knowing information; beta distribution ranges from 0 to 1 (vector of 2 values: shape1 and shape2)
                  bold.distr.beta = as.numeric(unlist(strsplit(as.character(args[i, 11]), " "))), # starting probability distribution of being bold, beta distribution (vector of 2 values: shape1 and shape2)
                  birthdeath.file = as.character(args[i, 12]), #dataframe of age based birth and death rate for FEMALES only. The columns should be age, ageClass, birthRate, and survivalRate, in that order.
                  result.folder = paste(as.character(args[i, 13]), i, sep = "/"), #an empty folder where results will be saved.
                  set_seed = args[i, 14], # want to make results reproducible? Then set as TRUE
                  save_at_each_iter = args[i, 15], #should all results be written to file at each time step?
                  vertTransmission = args[i, 16], # When giving birth, should your information status be given to your offspring? 0 if false, 1 if true (i.e., is there vertical transmission of information?)
                  densityDependType = args[i, 17], #density dependence of interactions, set to 1 for positive density dependence, 0 for none, -1 for negative density dependence
                  familiarBias = args[i, 18]) 

  # You can simply run ALL of the following code, and then check your results folder for results
  
  plotSocialIBMResults(result.folder=paste("C:/Users/Zach/Desktop/results/MicroArv", i, sep = "/"))

}
time2 <- Sys.time()
Sys.sleep(.001) 
print(time2 - time1)
