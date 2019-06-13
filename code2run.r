


#source the functions you will need
source("C:/Users/jmerkle/Documents/GitHub/Memory_IBM/info.transfer.IBM.R")


h=0.10 #increase in probability of death for uninformed
nl=0.01 # naive learning probability of the oldest animals (i.e., the ones that have the highest naive learning)
si=5 # maximum mean (i.e., lambda of poison distribution) number of interactions per pair (if animal has 1 bold, it interacts with an animal with 1 boldness, and population is at or above K, this is the lambda of the interaction distributions)
infotransfer=0.6 # given an interaction, what is the probability that information is transfered (min=0, max=1)
K=100 # carrying capacity
N0=50 # starting number of individuals
t=10 # time of simulation
sex.ratio=0.5 #what is the sex ratio of of the population/births?
age.distr.lamba=5 # lambda value for starting age distribution based on poison distribution
informed.distr.beta=c(.5, 1) # probability of knowing information, beta distribution ranges from 0 to 1 (vector of 2 values: shape1 and shape2)
bold.distr.beta=c(2, 2) # probability of being bold, beta distribution (vector of 2 values: shape1 and shape2)
birthdeath.file="C:/Users/jmerkle/Documents/GitHub/Memory_IBM/ageClass_Test.csv" #dataframe of age based birth and death rate. The columns should be age, ageClass, birthRate, and survivalRate, in that order.
result.folder="C:/Users/jmerkle/Desktop/results" #an empty folder where results will be saved.
set_seed=FALSE # want to make results reproducible? Then set as TRUE
save_at_each_iter=TRUE #should all results be written to file at each time step?
vertTransmission=0


info.transfer.IBM(h=0.10, #increase in probability of death for uninformed
                  nl=0.01, # naive learning probability of the oldest animals (i.e., the ones that have the highest naive learning)
                  si=5, # maximum mean (i.e., lambda of poison distribution) number of interactions per pair (if animal has 1 bold, it interacts with an animal with 1 boldness, and population is at or above K, this is the lambda of the interaction distributions)
                  infotransfer=0.6, # given an interaction, what is the probability that information is transfered (min=0, max=1)
                  K=100, # carrying capacity
                  N0=50, # starting number of individuals
                  t=10, # time of simulation
                  sex.ratio=0.5, #what is the sex ratio of of the population/births?
                  age.distr.lamba=5, # lambda value for starting age distribution based on poison distribution
                  informed.distr.beta=c(.5, 1), # probability of knowing information, beta distribution ranges from 0 to 1 (vector of 2 values: shape1 and shape2)
                  bold.distr.beta=c(2, 2), # probability of being bold, beta distribution (vector of 2 values: shape1 and shape2)
                  birthdeath.file="C:/Users/jmerkle/Documents/GitHub/Memory_IBM/ageClass_Test.csv", #dataframe of age based birth and death rate. The columns should be age, ageClass, birthRate, and survivalRate, in that order.
                  result.folder="C:/Users/jmerkle/Desktop/results", #an empty folder where results will be saved.
                  set_seed=FALSE, # want to make results reproducible? Then set as TRUE
                  save_at_each_iter=TRUE, #should all results be written to file at each time step?
                  vertTransmission=0) #0 if false, 1 if true, vertical transmission of info status


# load up results files
result.folder="C:/Users/jmerkle/Desktop/results"
load(paste0(result.folder,"/interaction_matricies.RData"))
load(paste0(result.folder,"/population_data.RData"))  #don't really need this if ind data is written out
load(paste0(result.folder,"/individual_data.RData"))
results <- read.csv(paste0(result.folder,"/population_stats.csv"))
head(results)

# png(paste0(result.folder,"/summary_stats.png"), width=4, height=6, units="in", res=400)
par(mfrow=c(3,2), mai=c(.3,.3,.03,.03), mgp=c(1,.1,0),
    tck=-0.02, cex.axis=.8)
plot(results$t, results$pop.size, type="l",ylab="N", xlab="Time step", lwd=3)
plot(results$t, results$births, type="l",ylab="Births", xlab="Time step", lwd=3)
plot(results$t, results$deaths, type="l",ylab="Deaths", xlab="Time step", lwd=3)
plot(results$t, results$med.age, type="l",ylab="Median age", xlab="Time step", lwd=3)
plot(results$t, results$frac.informed, type="l", 
     ylim=c(0,1),ylab="Proportion", xlab="Time step", lwd=3)
lines(results$t, 1-results$frac.informed, col="grey",lwd=3)
legend("right", c("Informed","Uninformed"), lty=1, col=c("black","grey"), inset=0.02)
# dev.off()

#plot of one yrs interactions
interactGraph <- graph_from_incidence_matrix(interactions[[2]], multiple = FALSE, mode="all", weighted = TRUE)
plot(interactGraph, vertex.size=10, vertex.label=NA)

#histogram of number of individuals with a number of interactions, interactions are binned in intervals of 5
sumIndInteracts <- rowSums(interactions[[2]])
hist(sumIndInteracts)

