


#source the functions you will need
source("C:/Users/jmerkle/Documents/GitHub/Memory_IBM/info.transfer.IBM.R")


h=0.10 #increase in probability of death for uninformed
nl=0.01 # naive learning probability
si=10 # social interaction probability
K=100 # carrying capacity
N0=50 # starting number of individuals
t=100 # time of simulation
sex.ratio=0.5 #what is the sex ratio of of the population/births?
age.distr.lamba=5 # lambda value for starting age distribution based on poison distribution
birthdeath.file="C:/Users/jmerkle/Documents/GitHub/Memory_IBM/ageClass_Test.csv" #dataframe of age based birth and death rate
vertTransmission=0


results <- info.transfer.IBM()



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

