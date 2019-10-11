# This code runs the information transfer IBM (info.transfer.IBM.R) and the plots the results
# Written by Zach Bell and Jerod Merkle
# June 2019


#source the function
source("C:/Users/Zach/Documents/GitHub/Memory_IBM/Memory_IBM/info.transfer.IBM.R")

info.transfer.IBM(h=0.20, #increase in probability of death for uninformed
                  nl=0.01, # naive learning probability of the oldest animals (i.e., the ones that have the highest naive learning)
                  si=5, # maximum mean (i.e., lambda of poison distribution) number of interactions per pair (if animal has 1 bold, it interacts with an animal with 1 boldness, and population is at or above K, this is the lambda of the interaction distributions)
                  infotransfer=0.03, # given an interaction, what is the probability that information is transfered (min=0, max=1)
                  K=3100, # carrying capacity
                  N0=300, # starting number of individuals
                  t=25, # how many years should the simulation run for?
                  sex.ratio=0.5, #what is the sex ratio of of the population/births?
                  age.distr.lamba=3.8, # lambda value for starting age distribution based on poison distribution
                  informed.distr.beta=c(.5, 5), # starting probability distribution of knowing information; beta distribution ranges from 0 to 1 (vector of 2 values: shape1 and shape2)
                  bold.distr.beta=c(2, 5), # starting probability distribution of being bold, beta distribution (vector of 2 values: shape1 and shape2)
                  birthdeath.file="C:/Users/Zach/Documents/GitHub/Memory_IBM/Memory_IBM/Myotis bechsteinii/ageClass_MyotisBe.csv", #dataframe of age based birth and death rate for FEMALES only. The columns should be age, ageClass, birthRate, and survivalRate, in that order.
                  result.folder="C:/Users/Zach/Desktop/results/MyotisBe", #an empty folder where results will be saved.
                  set_seed=FALSE, # want to make results reproducible? Then set as TRUE
                  save_at_each_iter=TRUE, #should all results be written to file at each time step?
                  vertTransmission=1, # When giving birth, should your information status be given to your offspring? 0 if false, 1 if true (i.e., is there vertical transmission of information?)
                  densityDependType = 0, #density dependence of interactions, set to 1 for positive density dependence, 0 for none, -1 for negative density dependence
                  familiarBias = .4) 

# You can simply run ALL of the following code, and then check your results folder for results
source("C:/Users/Zach/Documents/GitHub/Memory_IBM/Memory_IBM/plotInfoTransferIBM.R")
plotSocialIBMResults(result.folder="C:/Users/Zach/Desktop/results/MyotisBe")
