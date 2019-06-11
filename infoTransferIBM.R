library(igraph)
library(Matrix)

h <- .10 #increase in probability of death for uninformed
nl <- 0.01 # naive learning probability
si <- 10 # social interaction probability
K <- 100 # carrying capacity
N0 <- 50 # starting number of individuals
t <- 100 # time of simulation
d <- read.csv(file = "C:/Users/Zach/Documents/GitHub/Social-Learning-Pop-Crash-IBM/ageClass_Test.csv", colClasses = c("numeric", "numeric", "numeric", "numeric")) #vector of age based birth and death rate
d <- read.csv(file = "C:/Users/jmerkle/Documents/GitHub/Memory_IBM/ageClass_Test.csv", colClasses = c("numeric", "numeric", "numeric", "numeric")) #vector of age based birth and death rate
colnames(d) <- c("age", "ageClass", "birthRate", "survivalRate") #set column names of the age class, vitals data
maxAgeClass <- max(d$ageClass) #max age class in order to get a proportion of age class for age class based rates of naive learning
vertTransmission <- 0 #0 if false, 1 if true, vertical transmission of interactions

#create starting individual w attributes ("alive", "age", "informed"), initial population
set.seed(1)
ind <- vector(mode="list", N0) 
for(i in seq(ind)){
  ind[[i]]$alive <- 1 #0 if false, 1 if true
  ind[[i]]$sex <- round(rnorm(1, .5, .25)) #coinflip for sex, 0 if false, 1 if true
  ind[[i]]$age <- rpois(1, 5) #poisson distribution of age centered around 5
  informedProb <- rbeta(1, .5, 1) #probability of knowing information, beta distribution ranges from 0 to 1
  ageClass <- d$ageClass[which(d$age == ind[[i]]$age)]
  ind[[i]]$informed <- round(informedProb + informedProb * (ageClass/maxAgeClass)) #0 if false, 1 if true, modified by age class proportion 
  ind[[i]]$boldness <- rbeta(1, 2, 2) #beta distribution, ranges from 0 to 1
  ind[[i]]$mother <- 0
  ind[[i]]$birthYr <- 0
  #ind[[i]]$maternalSurvivalMod <- rbeta(1, .8, 1)
}

interactionMatrix <- Matrix(data = 0,
                   nrow = length(seq(ind)),
                   ncol = length(seq(ind)), sparse = TRUE)

# g <- graph_from_adjacency_matrix(interactionMatrix, mode="undirected")
# plot(g, vertex.size=10, vertex.label=NA)

#make empty vectors to record population statistics for each time step
time <- seq(t+1)

pop <- list() # population size
pop[[1]] <- which(sapply(ind, function(x) x$alive) == 1)

frac.informed <- NaN * time # fraction of population that is informed
info <- sapply(ind, function(x) x$informed)
frac.informed[1] <- sum(info  == 1) / length(info)

med.age <- NaN * time
ages <- sapply(ind, function(x) x$age)
med.age[1] <- median(ages)

interactions <- list()
interactions[[1]] <- interactionMatrix


#simulation
#save.alive.only <- TRUE # optional cropping of "ind" to include alive individuals only 
t1 <- Sys.time()
for(i in seq(t)){ # loop for each time increment
  
  
  is.alive <- which(sapply(ind, function(x) x$alive) == 1) #just alive individuals from total list
  boldness <- sapply(ind[is.alive], function(x) x$boldness) #boldness of alive individuals
  interactionMatrix <- Matrix(data = 0,
                              nrow = length(seq(is.alive)),
                              ncol = length(seq(is.alive)), sparse = TRUE)
  for(j in is.alive){ #loop for each alive individual
    
    curIndividual <- ind[[j]] #assigns current individual
    indexJ <- which(is.alive == j)
    ageClass <- d$ageClass[which(d$age == curIndividual$age)] #age class of current
    birthRate <- d$birthRate[which(d$age == curIndividual$age)] #birth rate of current age class
    survivalRate <- d$survivalRate[which(d$age == curIndividual$age)] #survival rate of current age class
    
    naiveLearn <- runif(1) <= (nl * ageClass/maxAgeClass * (1-curIndividual$informed)) # calculate a naive learning probability, depends on age class
    if(naiveLearn){
      curIndividual$informed <- 1
    }
    
    socialPool <- data.frame(is.alive[-j], boldness[-j]) #pool of available individuals to socialize with
    socialPool$numInteractions <- rpois(nrow(socialPool), (si * curIndividual$boldness * (length(is.alive)/K) * socialPool$boldness))# calculate a social interaction probability for each individual that is alive
    colnames(socialPool) <- c("is.alive", "boldness", "numInteractions")
    for(g in 1:nrow(socialPool)){ #loop through interaction individuals
      indexG <- which(is.alive == socialPool$is.alive[g])
      interIndividual <- ind[[socialPool$is.alive[g]]] #current interaction individual
      numInterInd <- socialPool$numInteractions[g]
        if((((1 - interIndividual$informed)+(1 - curIndividual$informed) > 0))){ # if current ind or current interaction ind is informed, make them informed
          curIndividual$informed <- 1
          interIndividual$informed <- 1
        }
        interactionMatrix[indexG, indexJ] <- numInterInd #update interaction matrix
        ind[[indexG]] <- interIndividual #update current interaction individual in total individuals dataset
    }
    birth <- runif(1) <= (birthRate * (1 - length(is.alive)/K)) # calculate a birth probability for each individual that is alive
    if(birth && (ind[[j]]$sex == 1)){ #checks for succesful birth and female sex
      #create new individual
      len.ind <- length(ind)
      informedProb <- rbeta(1, .5, 1)
      ind[[len.ind+1]] <- list(alive=1, age=1, sex = round(rnorm(1, .5, .25)), informed= round(vertTransmission * informedProb + informedProb * (1/maxAgeClass)), boldness = rbeta(1, 2, 2), mother = j, birthYr = i) # create offspring, inherits informed status of parent
      #makes new row in interaction matrix for new individual with inheirited interactions
      inheiritedConnections <- vector(nrow(interactionMatrix), mode = "numeric")
      inheiritedConnections[which(interactionMatrix[indexJ,] <= 1)] <- 1
      interactionMatrix <- rbind(interactionMatrix, inheiritedConnections)
      #makes new col in interaction matrix for new individual with inheirited interactions
      inheiritedConnections <- vector(ncol(interactionMatrix)+1, mode = "numeric")
      inheiritedConnections[which(interactionMatrix[indexJ,] <= 1)] <- 1
      interactionMatrix <- cbind(interactionMatrix, inheiritedConnections)
    }
    #death decided by survival Rate, density, and increased uniformed mortality rate
    death <- runif(1) <= survivalRate*(1 + (length(is.alive)/K)) + ((1 - curIndividual$informed) * h) # calculate a death probability for each individual 
    if(death){
      curIndividual$alive <- 0 # if death, reset alive = 0
    } else { #else, advance age + 1
      curIndividual$age <- curIndividual$age + 1 # advance age of parent
    }
    ind[[j]] <- curIndividual #update current individual in the total ind list
  }
  
  #optional cropping of list "ind"
  # if(save.alive.only){
  #   is.dead <- which(sapply(ind, function(x) x$alive) == 0)
  #   if(length(is.dead) > 0) ind <- ind[-is.dead]
  # }
  
  # saving Population stats for the time step
  is.alive <- which(sapply(ind, function(x) x$alive) == 1)
  pop[[i+1]] <- is.alive 
  
  info <- sapply(ind, function(x) x$informed)
  frac.informed[i+1] <- sum(info[is.alive]  == 1) / length(is.alive)
  
  ages <- sapply(ind, function(x) x$age)
  med.age[i+1] <- median(ages[is.alive])
  
  interactions[[i+1]] <- interactionMatrix 
  
  #simulation progress
  Sys.sleep(.1)
  print(paste(i, "of", t, "finished", "[", i/t*100, "%]"))
  
}
#time to run simulation
t2 <- Sys.time()
dt <- t2-t1
dt


#plot fractions of informed and uninformed populations over time
#png("pops_vs_time.png", width=6, height=4, units="in", res=400)
par(mar=c(4,4,1,1))
pop.info <- length(pop) * frac.informed
pop.uninfo <- length(pop) * (1-frac.informed)
ylim=range(c(pop.info, pop.uninfo))
plot(time, pop.info, t="l", lwd=2, col="red", ylim=ylim, ylab="Population size")
lines(time, pop.uninfo, lwd=2, col="blue")
legend("topleft", legend=c("informed pop.", "uniformed pop."), lwd=2, col=c("red","blue"), bty="n")
dev.off()

#plot median age
#png("med_age_vs_time.png", width=6, height=4, units="in", res=400)
par(mar=c(4,4,1,1))
plot(time, med.age, t="l", lwd=2, ylab="Median age")
dev.off()

#plot of one yrs interactions
interactGraph <- graph_from_incidence_matrix(interactions[[2]], multiple = FALSE, mode="all", weighted = TRUE)
plot(interactGraph, vertex.size=10, vertex.label=NA)

#histogram of number of individuals with a number of interactions, interactions are binned in intervals of 5
sumIndInteracts <- rowSums(interactions[[2]])
hist(sumIndInteracts)
