# This is the code to run the information transfer IBM (info.transfer.IBM.R)
# Written by Zach Bell and Jerod Merkle
# June 2019


#source the function
source("C:/Users/jmerkle/Documents/GitHub/Memory_IBM/info.transfer.IBM.R")

info.transfer.IBM(h=0.10, #increase in probability of death for uninformed
                  nl=0.01, # naive learning probability of the oldest animals (i.e., the ones that have the highest naive learning)
                  si=5, # maximum mean (i.e., lambda of poison distribution) number of interactions per pair (if animal has 1 bold, it interacts with an animal with 1 boldness, and population is at or above K, this is the lambda of the interaction distributions)
                  infotransfer=0.2, # given an interaction, what is the probability that information is transfered (min=0, max=1)
                  K=250, # carrying capacity
                  N0=50, # starting number of individuals
                  t=20, # time of simulation
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

png(paste0(result.folder,"/summary_stats.png"), width=4, height=6, units="in", res=400)
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
dev.off()    #check your results folder for this figure!!!!!

# add network matricies to results table
results.graph <- do.call(rbind, lapply(2:length(interactions), function(i){
  tmp <- interactions[[i]]
  g <- graph_from_adjacency_matrix(tmp, diag = FALSE, mode="undirected", weighted = TRUE)
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

#plot of one yrs interactions
interactGraph <- graph_from_adjacency_matrix(interactions[[2]], diag = FALSE, mode="undirected", weighted = TRUE)
length(E(interactGraph)$weight)   #this is the number of edges
hist(E(interactGraph)$weight)    #this is the distribution of interactions or weights among the number of edges
E(interactGraph)   # Here are the edges
length(V(interactGraph)) # this is the number of vertices or individuals in teh graph
#plot 1 year's graph
E(interactGraph)$width <- E(interactGraph)$weight*2
lo <- layout_nicely(interactGraph)
par(mai=c(.1,.1,.1,.1))
plot(interactGraph, layout=lo, vertex.label.cex=.6, vertex.size=10, edge.curved=.2)

# main plotting of graph results over time
png(paste0(result.folder,"/summary_graph_stats.png"), width=5, height=5, units="in", res=400)
par(mfrow=c(3,3), mai=c(.3,.3,.03,.03), mgp=c(1,.1,0),
    tck=-0.02, cex.axis=.8)
for(i in 2:ncol(results.graph)){
  plot(results.graph[,1], results.graph[,i], type="l",xlab="Time step", ylab=names(results.graph)[i], lwd=3)
}
dev.off()    #check your results folder for this figure!!!!!

