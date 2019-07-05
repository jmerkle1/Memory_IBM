learning <- 
function(curIndividual, ageClass, maxAgeClass = 4, is.alive, boldness, interactionMatrix, ind, densityDependType = 0, indexJ, birth = 0){
  curIndividual$informed <- rbinom(1, 1, nl * ageClass/maxAgeClass * (1-curIndividual$informed)) # calculate a naive learning probability, depends on age class
  if(birth == 1){
    socialPool <- data.frame(is.alive, boldness)
  }
  else{
    socialPool <- data.frame(is.alive[-indexJ], boldness[-indexJ]) #pool of available individuals to socialize with
  }
  if(densityDependType == 1){
    socialPool$numInteractions <- rpois(nrow(socialPool), (si * curIndividual$boldness * length(is.alive)/K * socialPool$boldness))# calculate a social interaction probability for each individual that is alive
  }
  if(densityDependType == 0){
    socialPool$numInteractions <- rpois(nrow(socialPool), (si * curIndividual$boldness * socialPool$boldness))# calculate a social interaction probability for each individual that is alive
  }
  if(densityDependType == -1){
    socialPool$numInteractions <- rpois(nrow(socialPool), (si * curIndividual$boldness * ifelse(length(is.alive)>=K, 1, 1+(1-length(is.alive)/K)) * socialPool$boldness))# calculate a social interaction probability for each individual that is alive
  }
  colnames(socialPool) <- c("is.alive", "boldness", "numInteractions")
  
  socialPool$intIDinformed <- sapply(ind[socialPool$is.alive], function(x) x$informed)
  if(birth == 0){
    interactionMatrix[-indexJ,indexJ] <- socialPool$numInteractions #update interaction matrix
    interactionMatrix[indexJ,-indexJ] <- socialPool$numInteractions #update interaction matrix
  }
  if(birth == 1){
    interactionMatrix <- rbind(interactionMatrix, socialPool$numInteractions) #update interaction matrix
    interactionMatrix <- cbind(interactionMatrix, socialPool$numInteractions)#update interaction matrix
  }
  socialPool$calc <- ifelse(curIndividual$informed==0 & socialPool$intIDinformed == 0, 0, 1)
  socialPool$infotransfer <- do.call(c, lapply(1:nrow(socialPool), function(ii){
    return(ifelse(sum(rbinom(socialPool$numInteractions[ii], 1, infotransfer))>0,1,0))
  }))
  socialPool$infotransfer <- socialPool$infotransfer*socialPool$calc
  if(curIndividual$informed==0 & sum(socialPool$infotransfer)>0){   #if status is 0 or there was an interaction wher info was transfered, then put a 1 in there
    curIndividual$informed <- 1
  }
  
  socialPool$infotransfer <- ifelse(socialPool$intIDinformed+socialPool$infotransfer>0,1,0)  # now we need to update the interacting individuals and their info status
  for(g in 1:nrow(socialPool)){ #loop through interaction individuals
    indexG <- socialPool$is.alive[g]
    ind[[indexG]]$informed <- socialPool$infotransfer[g] #update current interaction individual in total individuals dataset
  }
  returnList <- list(curIndividual, ind, interactionMatrix)
  return(returnList)
}