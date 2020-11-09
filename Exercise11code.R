#Drug treatment at 175 timesteps 
#Create 2 different subpopulations, see how growth responds after drug treatment at time t=175
#Original number of cells in non-mutant population
N0=100
#Original number of cells in mutant population
N1=1

r=0.1
K=1000000
timesteps=500

Ns=data.frame(time=1:timesteps,
              sim1=rep(0,timesteps))
Ns$sim1[1]=N0


Ms=data.frame(time=1:timesteps,
              sim1=rep(0,timesteps))
Ms$sim1[1]=N1


# simulate
# rate of growth modified after drug treatment at t=175. Rates change differently
#for mutant and nonmutant populations 
for(t in 2:timesteps){
  if (t>=175){
  Ns$sim1[t] <- Ns$sim1[t-1]+r*-1*Ns$sim1[t-1]*(1-(Ns$sim1[t-1]+Ms$sim1[t-1])/K)
  Ms$sim1[t] <- Ms$sim1[t-1]+r*.5*Ms$sim1[t-1]*(1-(Ns$sim1[t-1]+Ms$sim1[t-1])/K)
} else {
  Ns$sim1[t] <- Ns$sim1[t-1]+r*Ns$sim1[t-1]*(1-(Ns$sim1[t-1]+Ms$sim1[t-1])/K)
  Ms$sim1[t] <- Ms$sim1[t-1]+r*Ms$sim1[t-1]*(1-(Ns$sim1[t-1]+Ms$sim1[t-1])/K)
}
  }

# convert wide data to long form
Ns2<-data.frame(time=c(Ns$time,Ms$time),N=c(Ns$sim1,Ms$sim1),sim=rep(c("nonmutant", "mutant"),each=timesteps))

#plot 
library(ggplot2)
ggplot(data=Ns2,aes(x=time,y=N,color=sim)) + 
  ylab("Number cells") +
  xlab("Time")+
  geom_line() +
  theme_classic()
