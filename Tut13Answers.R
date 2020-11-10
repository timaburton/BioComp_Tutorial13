setwd("C:/Users/matth/Desktop/Intro_to_Biocomputing/BioComp_Tutorial13")

#Set conditions
time <- 1:600
rnn <- .1
rnt <- -.1
rmn <- .1
rmt <- .05
K=1000000
No=99

pop <- matrix(data=NA, nrow=length(time),ncol=3) #pre-allocate matrix
pop[,1] <- 1:length(time) #Set generations
pop[1,2] <- No #Set initial non-resistant population size
pop[1,3] <- 1 #Set initial resistant population size

#For some reason I could only get it to work if I did it without treatment first
for (i in 2:length(time)){ #For each generation
  pop[i,2] <- pop[i-1,2]+(rnn*pop[i-1,2]*(1-(pop[i-1,2]+pop[i-1,3])/K)) #nonres
  pop[i,3] <- pop[i-1,3]+(rmn*pop[i-1,3]*(1-(pop[i-1,2]+pop[i-1,3])/K)) #res
}


for (i in 2:length(time)){
  if(i < which(K - (pop[,2]+pop[,3])<100)[1]){#Equilibrium reached within 
    #100 cells of carrying capacity
    #This is considered pre-treatment
    pop[i,2] <- pop[i-1,2]+(rnn*pop[i-1,2]*(1-(pop[i-1,2]+pop[i-1,3])/K))
    pop[i,3] <- pop[i-1,3]+(rmn*pop[i-1,3]*(1-(pop[i-1,2]+pop[i-1,3])/K))
  }
  else{ #This is post-treatment
    pop[i,2] <- pop[i-1,2]+(rnt*pop[i-1,2]*(1-(pop[i-1,2]+pop[i-1,3])/K))
    pop[i,3] <- pop[i-1,3]+(rmt*pop[i-1,3]*(1-(pop[i-1,2]+pop[i-1,3])/K))  
  }
}
  
#Need a new df to graph it
numbers <- data.frame(Generation=pop[,1],NumberNot=pop[,2],NumberAre=pop[,3])
#Have to put df in "long mode"
newnumbers <- data.frame(generations=numbers[,1],size=unlist(c(numbers[,2],numbers[,3]),use.names=FALSE))
newnumbers$population <- rep(c("NonResistant","Resistant"),each=nrow(numbers)) 

#Make the graph!
library(ggplot2)
ggplot(data=newnumbers,aes(x=generations,y=size,color=population))+
  geom_line(method="loess",size=1.5)

#Viola! Have a great day!
