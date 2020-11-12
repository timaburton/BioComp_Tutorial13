### two simulations in a single for loop for the noral and the mutant cells.
N0=100
rN=0.1
rM=-0.1
K=1000000
timesteps=1000

# create dataframe to store N's and set initial N
#For normal cells
Ns=data.frame(time=1:timesteps,sim1=rep(0,timesteps),sim2=rep(0,timesteps))
#For the mutant cells
Ms=data.frame(time=1:timesteps,sim1=rep(0,timesteps),sim2=rep(0,timesteps))
#Set initial Ns and Ms Values
Ns$sim1[1]=N0
Ns$sim2[1]=N0
Ms$sim1[1]=N0
Ms$sim2[1]=N0

# simulate
for(t in 2:timesteps){
  Ns$sim1[t+1] <- Ns$sim1[t]+rN*Ns$sim1*(1-(Ns$sim1+Ms$sim1)/K)
  Ms$sim1[t+1] <- Ms$sim1[t]+rM*Ms$sim1*(1-(Ns$sim1+Ms$sim1)/K)
  
}

# convert wide data to long form
Ns2<-data.frame(time=c(Ns$time,Ms$time),N=c(Ns$sim1,Ns$sim2,Ms$sim1,Ms$sim2),each=timesteps))

# plot simulation
library(ggplot2)
ggplot(data=Ns2,aes(x=time,y=N,color=sim)) + 
  geom_line() +
  theme_classic()