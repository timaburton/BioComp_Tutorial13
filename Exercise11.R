# set carrying capacity at 1 million
K=1000000
# set time we will collect data for
times <- 1:800

# set up the matrix for N that will contain the time and population of non-mutant cells
N <- matrix(data=NA,nrow=length(times),ncol=2)
N[1,2]=2
N[,1]=times
# set up the matrix for M that will contain the time and population of mutant cells
M <- matrix(data=NA,nrow=length(times),ncol=2)
M[1:99,2]=0
M[100,2]=1 #first mutation at time=100
M[,1]=times

# loop to generate the model between time 2 and 250
# r=0.1 because the drug has not been added yet
for(i in 2:250){
 N[i,2]=N[(i-1),2] + (0.1)*N[(i-1),2]*(1-(N[(i-1),2]+M[(i-1),2])/K)
  if(i>100){
    M[i,2]=M[(i-1),2] + (0.1)*M[(i-1),2]*(1-(N[(i-1),2]+M[(i-1),2])/K)
  }
}

# new loop to simulate addition of the drug
for(i in 251:800){
  N[i,2]=N[(i-1),2] + (-0.1)*N[(i-1),2]*(1-(N[(i-1),2]+M[(i-1),2])/K)
  M[i,2]=M[(i-1),2] + (0.05)*M[(i-1),2]*(1-(N[(i-1),2]+M[(i-1),2])/K)
}

#turn matrix into data frame so can be used for plotting
NDF <- data.frame(time=N[,1],population=N[,2])
MDF <- data.frame(time=M[,1],population=M[,2])

#combine data frames into one data frame for plotting
CellPopulations <- data.frame(time=NDF[,1], populationN=NDF$population, populationM=MDF$population)

#load ggplot
library(ggplot2)

#plot the growth of the two populations
ggplot(CellPopulations)+
  geom_line(aes(x=time, y=populationN), color="blue")+
  geom_line(aes(x=time, y=populationM), color="red")+
  xlab("Time")+
  ylab("Population")
