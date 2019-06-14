# This code runs the information transfer IBM (info.transfer.IBM.R) and the plots the results
# Written by Zach Bell and Jerod Merkle
# June 2019


#source the function
source("C:/Users/jmerkle/Documents/GitHub/Memory_IBM/info.transfer.IBM.R")


info.transfer.IBM(h=0.20, #increase in probability of death for uninformed
                  nl=0.01, # naive learning probability of the oldest animals (i.e., the ones that have the highest naive learning)
                  si=5, # maximum mean (i.e., lambda of poison distribution) number of interactions per pair (if animal has 1 bold, it interacts with an animal with 1 boldness, and population is at or above K, this is the lambda of the interaction distributions)
                  infotransfer=0.03, # given an interaction, what is the probability that information is transfered (min=0, max=1)
                  K=250, # carrying capacity
                  N0=50, # starting number of individuals
                  t=30, # time of simulation
                  sex.ratio=0.5, #what is the sex ratio of of the population/births?
                  age.distr.lamba=4, # lambda value for starting age distribution based on poison distribution
                  informed.distr.beta=c(.5, 5), # starting probability of knowing information, beta distribution ranges from 0 to 1 (vector of 2 values: shape1 and shape2)
                  bold.distr.beta=c(2, 5), # probability of being bold, beta distribution (vector of 2 values: shape1 and shape2)
                  birthdeath.file="C:/Users/jmerkle/Documents/GitHub/Memory_IBM/ageClass_Test.csv", #dataframe of age based birth and death rate. The columns should be age, ageClass, birthRate, and survivalRate, in that order.
                  result.folder="C:/Users/jmerkle/Desktop/results", #an empty folder where results will be saved.
                  set_seed=FALSE, # want to make results reproducible? Then set as TRUE
                  save_at_each_iter=TRUE, #should all results be written to file at each time step?
                  vertTransmission=0) #0 if false, 1 if true, vertical transmission of info status


# You can simply run ALL of the following code, and then check your results folder for results
library(stringr)

# load up results files
result.folder="C:/Users/jmerkle/Desktop/results"
load(paste0(result.folder,"/interaction_matricies.RData"))
load(paste0(result.folder,"/population_data.RData"))  #don't really need this if ind data is written out
load(paste0(result.folder,"/individual_data.RData"))
results <- read.csv(paste0(result.folder,"/population_stats.csv"))
head(results)

png(paste0(result.folder,"/summary_stats.png"), width=4, height=6, units="in", res=400)
par(mfrow=c(3,2), mai=c(.3,.3,.03,.03), mgp=c(1,.1,0),
    tck=-0.02, cex.axis=.8)
plot(results$t, results$pop.size, type="l",ylab="N", xlab="Time step", lwd=3,ylim=c(0,max(results$pop.size)))
plot(results$t, results$births, type="l",ylab="Births", xlab="Time step", lwd=3,ylim=c(0,max(c(results$births,results$deaths),na.rm=TRUE)))
plot(results$t, results$deaths, type="l",ylab="Deaths", xlab="Time step", lwd=3,ylim=c(0,max(c(results$births,results$deaths),na.rm=TRUE)))
plot(results$t, results$med.age, type="l",ylab="Median age", xlab="Time step", lwd=3, ylim=c(0,max(results$med.age)))
plot(results$t, results$frac.informed, type="l", 
     ylim=c(0,1),ylab="Proportion", xlab="Time step", lwd=3)
lines(results$t, 1-results$frac.informed, col="grey",lwd=3)
legend("right", c("Informed","Uninformed"), lty=1, col=c("black","grey"), inset=0.02)
dev.off()    #check your results folder for this figure!!!!!

# build a results table of network stats and save out graph figures
results.graph <- do.call(rbind, lapply(2:length(interactions), function(i){
  #create the graph for the given time step
  tmp <- interactions[[i]]
  g <- graph_from_adjacency_matrix(tmp, diag = FALSE, mode="undirected", weighted = TRUE)
  
  #manage the folders where the graphs will go
  dr <- paste0(result.folder,"/graphs")
  
  if(i == 2){
    unlink(dr, recursive=TRUE)
    dir.create(dr)
  }

  # save out the plot of the graph
  E(g)$width <- E(g)$weight*2
  lo <- layout_nicely(g)
  png(paste0(dr,"/graph",str_pad(i-1,3,pad="0"),".png"), width=5, height=5, units="in", res=400)
  par(mai=c(.03,.03,.03,.03))
  plot(g, layout=lo, vertex.label.cex=.4, vertex.size=7, edge.curved=.1, vertex.label=pop[[i]])
  dev.off()
  
  #write out dataframe with dereived metrics from the graph
  return(data.frame(t=i-1, no_ids=length(V(g)),
                    edge_density=edge_density(g), #The proportion of present edges from all possible edges in the network.
                    diameter=diameter(g, directed=F, weights=NULL),   #  length of the longest geodesic (or longest distance)
                    mean_distance=mean_distance(g),            # mean length of geodesics
                    degree=mean(degree(g, normalized=TRUE)),
                    betweenness=mean(betweenness(g, normalized=TRUE)),    #the mean number of geodesics (shortest paths) going through a vertex or an edge.
                    eigen_centrality=eigen_centrality(g, weights=E(g)$weight, scale=TRUE)$value,    # mean centrality proportional to the sum of connection centralities
                    authority=hub.score(g, weights=E(g)$weight, scale=TRUE)$value,   #hub or authority score 
                    transitivity=transitivity(g, type="global", weights=E(g)$weight)))   #probability that the adjacent vertices of a vertex are connected. This is sometimes also called the clustering coefficient.
  
}))
head(results.graph)

# main plotting of graph results over time
png(paste0(result.folder,"/summary_graph_stats.png"), width=5, height=5, units="in", res=400)
par(mfrow=c(3,3), mai=c(.3,.3,.03,.03), mgp=c(1,.1,0),
    tck=-0.02, cex.axis=.8)
for(i in 2:ncol(results.graph)){
  plot(results.graph[,1], results.graph[,i], type="l",xlab="Time step", ylab=names(results.graph)[i], lwd=3)
}
dev.off()    #check your results folder for this and other figures!!!!!

