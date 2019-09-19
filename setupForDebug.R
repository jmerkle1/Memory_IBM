h=0.01 #increase in probability of death for uninformed
nl=0.01 # naive learning probability of the oldest animals (i.e. the ones that have the highest naive learning)
si=5 # maximum mean (i.e., lambda of poison distribution) number of interactions per pair (if animal has 1 bold, it interacts with an animal with 1 boldness, and population is at or above K, this is the lambda of the interaction distributions)
infotransfer=0.03 # given an interaction, what is the probability that information is transfered (min=0, max=1)
K=200 # carrying capacity
N0=50 # starting number of individuals
t=25 # how many years should the simulation run for?
sex.ratio=0.5 #what is the sex ratio of of the population/births?
age.distr.lamba=4 # lambda value for starting age distribution based on poison distribution
informed.distr.beta=c(.5, 5) # starting probability distribution of knowing information; beta distribution ranges from 0 to 1 (vector of 2 values: shape1 and shape2)
bold.distr.beta=c(2, 5) # starting probability distribution of being bold, beta distribution (vector of 2 values: shape1 and shape2)
birthdeath.file="C:/Users/Yankee/Documents/GitHub/Memory_IBM/ageClass_Test.csv" #dataframe of age based birth and death rate for FEMALES only. The columns should be age, ageClass, birthRate, and survivalRate, in that order.
result.folder="C:/Users/Yankee/Desktop/results" #an empty folder where results will be saved.
set_seed=FALSE # want to make results reproducible? Then set as TRUE
save_at_each_iter=TRUE #should all results be written to file at each time step?
vertTransmission=1 # When giving birth, should your information status be given to your offspring? 0 if false, 1 if true (i.e., is there vertical transmission of information?) 
densityDependType = 0 #density dependence of interactions, set to 1 for positive density dependence, 0 for none, -1 for negative density dependence
familiarBias = .4
density = seq(0,2,.001)

if(all(c("igraph","Matrix") %in% installed.packages()[,1])==FALSE)
  stop("You must install the following packages: igraph and Matrix.")
require(igraph)
require(Matrix)

#identify initial time
t1 <- Sys.time()
print(paste0("start time: ",t1,"."))

#check the empty directories
if(dir.exists(result.folder)==FALSE){
  dir.create(result.folder)
}
if(length(dir(result.folder))> 0)
  print("Warning! Your result.folder has something in it. Those files will be overwritten!")

# bring in the age and birth and death rate data
if(file.exists(birthdeath.file)==FALSE)
  stop("You didn't provide an existing file for birthdeath.file!")
d <- read.csv(file = birthdeath.file, colClasses = c("numeric", "numeric", "numeric", "numeric")) 
if(ncol(d)!= 4)
  stop("Your birthdeath.file does not have 4 columns. The columns should be age, ageClass, birthRate, and survivalRate, in that order.")
colnames(d) <- c("age", "ageClass", "birthRate", "survivalRate") #set column names of the age class, vitals data
maxAgeClass=max(d$ageClass) #max age class in order to get a proportion of age class for age class based rates of naive learning

source(locationLearnFunction)

#create starting individual w attributes ("alive", "age", "informed"), initial population
print("Creating initial individual database.")
if(set_seed){
  set.seed(1)
}

ind <- vector(mode="list", N0) 
for(i in seq(ind)){
  ind[[i]]$alive <- 1 #0 if false, 1 if true
  ind[[i]]$sex <- rbinom(1, 1, sex.ratio) #coinflip for sex, 0 = male, 1 = female  
  ind[[i]]$age <- rpois(1, age.distr.lamba) #poisson distribution of age centered around 5
  if(ind[[i]]$age==0){   #can't have a 0
    ind[[i]]$age <- 1
  }
  informedProb <- rbeta(1, informed.distr.beta[1], informed.distr.beta[2]) #probability of knowing information, beta distribution ranges from 0 to 1
  ageClass <- d$ageClass[which(d$age == ind[[i]]$age)]
  ind[[i]]$informed <- round((informedProb + informedProb * (ageClass/maxAgeClass))/2) #0 if false, 1 if true, modified by age class proportion (we standardize so the values range from 0 to 1, based on the range could be 0 to 2)
  ind[[i]]$boldness <- rbeta(1, bold.distr.beta[1], bold.distr.beta[2]) #beta distribution, ranges from 0 to 1
  ind[[i]]$mother <- 0
  ind[[i]]$birthYr <- 0
  #ind[[i]]$maternalSurvivalMod <- rbeta(1, .8, 1)
}
returnedList <- list()
interactionMatrix <- Matrix(data = 0,
                            nrow = length(seq(ind)),
                            ncol = length(seq(ind)), sparse = TRUE)

#make empty vectors to record population statistics for each time step
time <- seq(t+1)

pop <- list() # population size
pop[[1]] <- which(sapply(ind, function(x) x$alive) == 1)

frac.informed <- NaN * time # fraction of population that is informed
info <- sapply(ind, function(x) x$informed)   
frac.informed[1] <- mean(info)

med.age <- NaN * time
ages <- sapply(ind, function(x) x$age)
med.age[1] <- median(ages)

interactions <- list()
interactions[[1]] <- interactionMatrix

#this dataframe will be saved out at every t iteration no matter what.
tosave <- data.frame(time.stamp=Sys.time(), t=0, pop.size=length(pop[[1]]),births=NA, deaths=NA,
                     frac.informed=frac.informed[1], med.age=med.age[1])

density <- (exp(density) - (density + 1))/2
density <- ifelse(density > 2, 2, density)
x <- seq(0,2,.001)

#simulation starts here
print(paste0("Looping through the ", t, " years."))

