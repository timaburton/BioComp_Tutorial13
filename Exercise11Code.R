# Exercise 11
# Set timesteps
timesteps = 500
# Dataframe for non-mutant cells
Ns = data.frame(time = 1:timesteps, pop=rep(0,timesteps))
# Start simulation when there are 100 total cells, 99 normal and 1 mutant
Ns[1,2] = 99

# Dataframe for mutant cells
Ms = data.frame(time = 1:timesteps, pop=rep(0,timesteps))
# A Single mutant cell
Ms[1,2] = 1


# Set parameters
r = 0.1
K = 1000000
rd = -0.1

# Loop for simulation before drug treatment
for (t in 2:200){
  Ns$pop[t] <- Ns$pop[t-1]+r*Ns$pop[t-1]*(1-(Ns$pop[t-1]+Ms$pop[t-1])/K)
  Ms$pop[t] <- Ms$pop[t-1]+r*Ms$pop[t-1]*(1-(Ns$pop[t-1]+Ms$pop[t-1])/K)
}

# Loop for simulation after drug treatment
for (t in 201:timesteps){
  Ns$pop[t] <- Ns$pop[t-1]+rd*Ns$pop[t-1]*(1-(Ns$pop[t-1]+Ms$pop[t-1])/K)
  Ms$pop[t] <- Ms$pop[t-1]+r/2*Ms$pop[t-1]*(1-(Ns$pop[t-1]+Ms$pop[t-1])/K)
}

# Combine into one dataframe
populations <- data.frame(time = c(Ns$time, Ms$time), populationN = Ns$pop, populationM = Ms$pop)

# Graph
library(ggplot2)
ggplot(populations)+
  geom_line(aes(x=time, y=populationN), color ="light blue")+
  geom_line(aes(x=time, y=populationM), color ="pink")+
  xlab("Time")+
  ylab("Population")
  theme_classic()
  