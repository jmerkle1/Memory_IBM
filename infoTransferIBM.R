library(igraph)
library(Matrix)

this is a test

#sexRatio <- 
h <- .10 #increase in probability of death for uninformed
nl <- 0.001 # naive learning probability
si <- 10 # social interaction probability
K <- 100 # carrying capacity
N0 <- 50 # starting number of individuals
t <- 500 # time of simulation
d <- read.csv(file = "C:/Users/Zach/Documents/infoTransferIBM/ageClassData.csv", col.names = c("age_Class", "ages", "birth_Rate", "survival_Rate"), colClasses = c("factor", "character", "numeric", "numeric")) #vector of age based birth and death rate
colnames(d) <- c("age_Class", "ages", "birth_Rate", "survival_Rate")

maxAge <- 12
vertTransmission <- 0 #0 if false, 1 if true #change to distribution?
totalInitialEdges <- 0

#create starting individual w attributes ("alive", "age", "informed"), initial population
set.seed(1)
ind <- vector(mode="list", N0)
for(i in seq(ind)){
  ind[[i]]$alive <- 1
  ind[[i]]$sex <- runif(1) <= rnorm(1, .5, .2)
  ind[[i]]$age <- rnorm(1, 5, 3)
  informedProb <- rbeta(1, .5, 1)
  ind[[i]]$informed <- runif(1) <= round(informedProb + informedProb * ind[[i]]$age)
  ind[[i]]$boldness <- rbeta(1, 2, 2)
  ind[[i]]$initialEdges <- round(rgamma(1, 9, .5))
  totalInitialEdges <- totalInitialEdges + ind[[i]]$initialEdges
  #ind[[i]]$maternalSurvivalMod <- rbeta(1, .8, 1)
}

edgeMatrix <- Matrix(data = 0,
                   nrow = length(seq(ind)),
                   ncol = length(seq(ind)), sparse = TRUE)

availableInd <- seq(ind)
availableInd[unlist(lapply(ind, function(x){x$initialEdges}))==0] <- NA
availableInd <- availableInd[order(unlist(lapply(ind, function(x){x$initialEdges})))]
hist(unlist(lapply(ind, function(x){x$initialEdges})))
availableInd <- na.omit(availableInd)


for (i in availableInd[1:(length(availableInd) - 1)]) {
  if (i %in% availableInd == FALSE)
    next
  if (sum(edgeMatrix[i,]) < ind[[i]]$initialEdges) {
    numb2sample <-
      ifelse(ind[[i]]$initialEdges > length(na.omit(availableInd[which(availableInd == i) +
                                                                   1:length(availableInd)])),
             length(na.omit(availableInd[which(availableInd == i) +
                                           1:length(availableInd)])),
             ind[[i]]$initialEdges)
    if (numb2sample == 0)
      next
    interactInd <-
      sample(na.omit(availableInd[which(availableInd == i) + 1:length(availableInd)]), numb2sample)
    
    for (e in 1:length(interactInd)) {
    }
    if (round((ind[[i]]$boldness * ind[[interactInd[e]]]$boldness) + si) > 1) {
      edgeMatrix[i, interactInd[e]] <- 1
    }
    if (sum(edgeMatrix[, interactInd[e]]) == ind[[interactInd[e]]]$initialEdges) {
      availableInd[availableInd == interactInd[e]] <- NA
    }
    availableInd[i] <- NA
  }
}


sum(edgeMatrix)
meanEdges <- sum(edgeMatrix)/nrow(edgeMatrix)
length(which(edgeMatrix == 1))

g <- graph_from_adjacency_matrix(edgeMatrix, mode="undirected")
plot(g, vertex.size=10, vertex.label=NA)

#make empty vectors to record population statistics
time <- seq(t+1)



pop <- NaN * time # population size
pop[1] <- N0

frac.informed <- NaN * time # fraction of population that is informed
info <- sapply(ind, function(x) x$informed)
frac.informed[1] <- sum(info  == 1) / length(info)

med.age <- NaN * time
ages <- sapply(ind, function(x) x$age)
med.age[1] <- median(ages)

edges <- vector("list", length = length(time))
edges[[1]] <- edgeMatrix


#simulation
save.alive.only <- TRUE # optional cropping of "ind" to include alive individuals only 
t1 <- Sys.time()
for(i in seq(t)){ # loop for each time increment
  
  is.alive <- which(sapply(ind, function(x) x$alive) == 1)
  for(j in is.alive){ #loop for each alive individual
    
    naiveLearn <- runif(1) <= (nl * age/maxAge * 1-ind[[j]]$informed) # calculate a naive learning probability for each individual that is alive
    if(naiveLearn){
      ind[[j]]$informed <- 1
    }
    
    numOfInteract <- rpois(1, sum(edgeMatrix[j,]))
    for(k in 1:numOfInteract){
    socialInter <- runif(1) <= (si * ind[[j]]$boldness * length(is.alive)/K) # calculate a social interaction probability for each individual that is alive
    if(socialInter){
      uniform <- dunif(1:length(is.alive), min = 0, max =  length(is.alive))
      edgesOfInd <- length(which(edgeMatrix[j,] == 1)) #number of connected individuals
      edgesOfIndMod <- .05 #modifier to add to each connected individual's selection chance
      edgesOfIndModTotal <- edgesOfInd * edgesOfIndMod #total percentage
      uniform <- uniform - edgesOfIndModTotal/length(uniform) #subtracting the total percentage of connected individuals from each element in uniform equally so that after addition of modifier, sum of pdf = 1
      uniform[which(edgeMatrix[j,] == 1)] <- uniform[which(edgeMatrix[j,] == 1)] + edgesOfIndMod #adds the modifier to specified individuals
      interIndividual <- round(runif(1, min = 0, max =  length(is.alive))) #record individual that is interacted with
      if(round(socialInter * ind[[interIndividual]]$boldness)){
      if(edgeMatrix[j,interIndividual] == 1 || edgeMatrix[interIndividual, j] == 1){
      if(ind[[interIndividual]]$informed && !ind[[j]]$informed){
        ind[[j]]$informed <- 1
        } 
      if(!ind[[interIndividual]]$informed && ind[[j]]$informed){
        ind[[interIndividual]]$informed <- 1
      }
      }
      
      else{
        edgeMatrix[j,interIndividual] == 1
        edgeMatrix[interIndividual, j] == 1
        if(ind[[interIndividual]]$informed && !ind[[j]]$informed){
          ind[[j]]$informed <- 1
        } 
        if(!ind[[interIndividual]]$informed && ind[[j]]$informed){
          ind[[interIndividual]]$informed <- 1
        }
      }
      }
    }
    }
    birth <- runif(1) <= (b * (1 - length(is.alive)/K)) # calculate a birth probability for each individual that is alive
    if(birth){
      len.ind <- length(ind)
      ind[[len.ind+1]] <- list(alive=1, age=0, informed= runif(1) <= round(vertTransmission * rnorm(1, ind[[j]]$informed, .25 * (1 - ind[[j]]$recruitmentMod))), boldness = ind[[j]]$boldness * ind[[j]]$recruitmentMod, recruitmentMod = round(vertTransmission * rnorm(1, ind[[j]]$recruitmentMod, rbeta(1, .6, 3)))) # create offspring, inherits informed status of parent
      edgeMatrix <- edgeMatrix[nrow(edgeMatrix) + 1, ncol(edgeMatrix) + 1]
      numinheritedConnections <- ind[[j]]$recruitmentMod * sum(edgeMatrix[j,])
      inheritedConnections <- sample(numinheritedConnections, which(edgeMatrix[j,] == 1))
      edgeMatrix[nrow(edgeMatrix), inheritedConnections] <- 1
      }
    death <- runif(1) <= ind[[j]]$age/maxAge * (1 - length(is.alive)/K) + if(ind[[j]]$informed == 0)h # calculate a death probability for each individual 
    if(death){
      ind[[j]]$alive <- 0 # if death, reset alive = 0
      edgeMatrix[j,] <- 0
      edgeMatrix[,j] <- 0
    } else { #else, advance age + 1
      ind[[j]]$age <- ind[[j]]$age + 1 # advance age of parent
    }
  }
  
  #optional cropping of list "ind"
  if(save.alive.only){
    is.dead <- which(sapply(ind, function(x) x$alive) == 0)
    if(length(is.dead) > 0) ind <- ind[-is.dead]
  }
  
  #Population stats
  is.alive <- which(sapply(ind, function(x) x$alive) == 1)
  pop[i+1] <- length(is.alive) 
  
  info <- sapply(ind, function(x) x$informed)
  frac.informed[i+1] <- sum(info[is.alive]  == "informed") / length(is.alive)
  
  ages <- sapply(ind, function(x) x$age)
  med.age[i+1] <- median(ages[is.alive])
  
  edges[i+1] <- edgeMatrix 
  
  print(paste(i, "of", t, "finished", "[", round(1/t*100), "%]"))
}
t2 <- Sys.time()
dt <- t2-t1
dt


#plot populations
png("pops_vs_time.png", width=6, height=4, units="in", res=400)
par(mar=c(4,4,1,1))
pop.info <- pop * frac.informed
pop.uninfo <- pop * (1-frac.informed)
ylim=range(c(pop.info, pop.uninfo))
plot(time, pop.info, t="l", lwd=2, col=4, ylim=ylim, ylab="Population size")
lines(time, pop.uninfo, lwd=2, col=2)
legend("topleft", legend=c("informed pop.", "uniformed pop."), lwd=2, col=c(4,2), bty="n")
dev.off()

#plot median age
png("med_age_vs_time.png", width=6, height=4, units="in", res=400)
par(mar=c(4,4,1,1))
plot(time, med.age, t="l", lwd=2, ylab="Median age")
dev.off()