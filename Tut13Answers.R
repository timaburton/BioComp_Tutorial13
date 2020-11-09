setwd("C:/Users/matth/Desktop/Intro_to_Biocomputing/BioComp_Tutorial13")
time <- 1:10000
rnn <- .1
rnt <- -.1
rmn <- .1
rmt <- .05

K=1000000
No=99
pop <- matrix(data=NA, nrow=length(time),ncol=3)
pop[,1] <- 1:length(time)
pop[1,2] <- No
pop[1,3] <- 1

for (i in 2:length(time)){
  pop[i,2] <- pop[i-1,2]+(rnn*pop[i-1,2]*(1-(pop[i-1,2]+pop[i-1,3])/K))
  pop[i,3] <- pop[i-1,3]+(rmn*pop[i-1,3]*(1-(pop[i-1,2]+pop[i-1,3])/K))
}

for (i in 2:length(time)){
  if(i < which(K - (pop[,2]+pop[,3])<100)[1]){
    pop[i,2] <- pop[i-1,2]+(rnn*pop[i-1,2]*(1-(pop[i-1,2]+pop[i-1,3])/K))
    pop[i,3] <- pop[i-1,3]+(rmn*pop[i-1,3]*(1-(pop[i-1,2]+pop[i-1,3])/K))
  }
  else{
    pop[i,2] <- pop[i-1,2]+(rnt*pop[i-1,2]*(1-(pop[i-1,2]+pop[i-1,3])/K))
    pop[i,3] <- pop[i-1,3]+(rmt*pop[i-1,3]*(1-(pop[i-1,2]+pop[i-1,3])/K))  
  }
}
  


rm(pop)
