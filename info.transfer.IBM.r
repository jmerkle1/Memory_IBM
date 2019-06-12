# Function to run information transfer IBM
# developed by Zach Bell and Jerod Merkle
# last update: June 2019


info.transfer.IBM <- function(h=0.10, #increase in probability of death for uninformed
                              nl=0.01, # naive learning probability
                              si=10, # social interaction probability
                              K=100, # carrying capacity
                              N0=50, # starting number of individuals
                              t=100, # time of simulation
                              sex.ratio=0.5, #what is the sex ratio of of the population/births?
                              age.distr.lamba=5, # lambda value for starting age distribution based on poison distribution
                              informed.distr.beta=c(.5, 1), # probability of knowing information, beta distribution ranges from 0 to 1 (vector of 2 values: shape1 and shape2)
                              bold.distr.beta=c(2, 2), # probability of being bold, beta distribution (vector of 2 values: shape1 and shape2)
                              birthdeath.file="C:/Users/jmerkle/Documents/GitHub/Memory_IBM/ageClass_Test.csv", #dataframe of age based birth and death rate
                              result.folder="C:/Users/jmerkle/Desktop/results", #an empty folder where results will be saved.
                              set_seed=TRUE, # want to make results reproducible? Then set as TRUE
                              save_at_each_iter=TRUE, #should it write all results to file at each time step?
                              vertTransmission=0){ #0 if false, 1 if true, vertical transmission of interactions

  #manage packages
  if(all(c("igraph","Matrix") %in% installed.packages()[,1])==FALSE)
    stop("You must install the following packages: igraph and Matrix.")
  require(igraph)
  require(Matrix)

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
    informedProb <- rbeta(1, informed.distr.beta[1], informed.distr.beta[2]) #probability of knowing information, beta distribution ranges from 0 to 1
    ageClass <- d$ageClass[which(d$age == ind[[i]]$age)]
    ind[[i]]$informed <- round(informedProb + informedProb * (ageClass/maxAgeClass)) #0 if false, 1 if true, modified by age class proportion 
    ind[[i]]$boldness <- rbeta(1, bold.distr.beta[1], bold.distr.beta[2]) #beta distribution, ranges from 0 to 1
    ind[[i]]$mother <- 0
    ind[[i]]$birthYr <- 0
    #ind[[i]]$maternalSurvivalMod <- rbeta(1, .8, 1)
  }
  
  interactionMatrix <- Matrix(data = 0,
                              nrow = length(seq(ind)),
                              ncol = length(seq(ind)), sparse = TRUE)

  #make empty vectors to record population statistics for each time step
  time <- seq(t+1)
  
  pop <- list() # population size
  pop[[1]] <- which(sapply(ind, function(x) x$alive) == 1)
  
  frac.informed <- NaN * time # fraction of population that is informed
  info <- sapply(ind, function(x) x$informed)   #NOTE: once we get the informed values fixed so they are only 0 or 1, we can simply do a mean here.
  frac.informed[1] <- sum(info  == 1) / length(info)
  
  med.age <- NaN * time
  ages <- sapply(ind, function(x) x$age)
  med.age[1] <- median(ages)
  
  interactions <- list()
  interactions[[1]] <- interactionMatrix
  
  tosave <- data.frame(time.stamp=Sys.time(), t=0, pop.size=length(pop[[1]]),births=NA, deaths=NA,
                       frac.informed=frac.informed[1], med.age=med.age[1])
  
  #simulation starts here
  print(paste0("Looping through the ", t, " years."))
  for(i in seq(t)){ # loop for each time increment
    #prep that years data
    is.alive <- which(sapply(ind, function(x) x$alive) == 1) #just alive individuals from total list
    boldness <- sapply(ind[is.alive], function(x) x$boldness) #boldness of alive individuals
    interactionMatrix <- Matrix(data = 0,   #build a new interaction matrix for this time step!
                                nrow = length(seq(is.alive)),
                                ncol = length(seq(is.alive)), sparse = TRUE)
    for(j in is.alive){ #loop for each alive individual
      
      curIndividual <- ind[[j]] #assigns current individual
      indexJ <- which(is.alive == j)    #NOTE: Is indexJ aways the same value as j?
      ageClass <- d$ageClass[which(d$age == curIndividual$age)] #age class of current
      birthRate <- d$birthRate[which(d$age == curIndividual$age)] #birth rate of current age class
      survivalRate <- d$survivalRate[which(d$age == curIndividual$age)] #survival rate of current age class
      
      naiveLearn <- runif(1) <= (nl * ageClass/maxAgeClass * (1-curIndividual$informed)) # calculate a naive learning probability, depends on age class
      if(naiveLearn){   #if this animal has knowledge, then add a 1 to informed column
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
        informedProb <- rbeta(1, informed.distr.beta[1], informed.distr.beta[2])
        ind[[len.ind+1]] <- list(alive=1, age=1, sex = round(rnorm(1, .5, .25)), 
                                 informed=round(vertTransmission * informedProb + informedProb * (1/maxAgeClass)), 
                                 boldness = rbeta(1, bold.distr.beta[1], bold.distr.beta[2]), 
                                 mother = j, birthYr = i) # create offspring, inherits informed status of parent
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
    
    # saving Population stats for final output
    numb.died <- sum(is.alive %in% which(sapply(ind, function(x) x$alive) == 1) == FALSE)
    
    is.alive <- which(sapply(ind, function(x) x$alive) == 1)
    pop[[i+1]] <- is.alive 
    
    info <- sapply(ind, function(x) x$informed)
    frac.informed[i+1] <- sum(info[is.alive]  == 1) / length(is.alive)
    
    ages <- sapply(ind, function(x) x$age)
    med.age[i+1] <- median(ages[is.alive])
    
    interactions[[i+1]] <- interactionMatrix 
    
    tosave <- rbind(tosave, data.frame(time.stamp=Sys.time(), t=i, pop.size=length(pop[[i+1]]),frac.informed=frac.informed[i+1],
                                       births=length(which(sapply(ind, function(x) x$birthYr) == i)), 
                                       deaths=numb.died, med.age=med.age[i+1]))
    write.csv(tosave, paste0(result.folder,"/population_stats.csv"), row.names = FALSE)
    
    #save out all the main files?
    if(save_at_each_iter == TRUE){
      save(interactions, file = paste0(result.folder,"/interaction_matricies.RData"))
      save(pop, file = paste0(result.folder,"/population_data.RData"))  #don't really need this if ind data is written out
      save(ind, file = paste0(result.folder,"/individual_data.RData"))
    }
  
    #simulation progress
    Sys.sleep(.1)
    print(paste0(i, " of ", t, " finished ", "[", round(i/t*100,0), "%]"))
    
  } # end of loop through t
  
  #write out final files
  write.csv(tosave, paste0(result.folder,"/population_stats.csv"), row.names = FALSE)
  save(interactions, file = paste0(result.folder,"/interaction_matricies.RData"))
  save(pop, file = paste0(result.folder,"/population_data.RData"))  #don't really need this if ind data is written out
  save(ind, file = paste0(result.folder,"/individual_data.RData"))
  
  #time to run simulation
  return(paste0("Completed in ", round((as.numeric(Sys.time())-as.numeric(t1))/60,2), " mins! Check your result.folder for results."))
}


