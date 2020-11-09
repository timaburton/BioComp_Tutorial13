#Excercise 11
#Anoop Sunkara
#population model to investigate evolution of drug resistance in tumors
#Imagine a cancer cell in a tumor that spontaneously exhibited a mutation that confers drug resistance
#The mutation does not have any positive or negative effects on growth rate of that sub-population when the cancer drug is absent
#However, when the cancer drug is present the mutant sub-population grows at 50% of its growth rate in the absence of the drug 
#and the non-mutant sub-population declines rapidly


#setting original r value for growth rate of normal and mutant cells
Rorig = .1
#carrying capacity
K = 1000000
#growth rate of normal cells when drug has been administered
Rdrug = -.1

#number of timesteps in timeline
timesteps = 500

#creating empty vectors of the length of the number of timesteps
Ncount = numeric(length = timesteps)
Mcount = numeric(length = timesteps)


#setting original N values
#Mutant cell was supposed to appear early so began with one mutant cell
#mutant cell appears when 100 normal cells are present
Ncount[1] = 10
Mcount[1] = 0

#check for when normal cell count is close to 100
onlyonce = TRUE

#check for drug administration level
druglevel = FALSE

#for loop to fill empty vector from above
#recursive function setup used to calculate next value based on 
#current normal and mutant population levels
for(t in 1:(timesteps-1)){
  #when total cell count is above 960000 (very close to carrying capacity), drug is administered
  if((Ncount[t] + Mcount[t]) > 996000 & druglevel == FALSE){
    
    druglevel = TRUE
  }
  #add in mutant cell when normal cell count is within 5 of 100
  if(Ncount[t]>95 & Ncount[t]<105 & onlyonce == TRUE){
    Mcount[t+1] = Mcount[t] + 1
    Ncount[t+1] <- Ncount[t]+Rorig*Ncount[t]*(1-(Ncount[t]+Mcount[t])/K)  
    onlyonce = FALSE
  }else{
    
  if(druglevel == FALSE){
    Ncount[t+1] <- Ncount[t]+Rorig*Ncount[t]*(1-(Ncount[t]+Mcount[t])/K)  
  Mcount[t+1] <- Mcount[t]+Rorig*Mcount[t]*(1-(Ncount[t]+Mcount[t])/K)  
  }
  #drug administered after carrying capacity is reached
  if(druglevel == TRUE){
    Ncount[t+1] <- Ncount[t]+Rdrug*Ncount[t]*(1-(Ncount[t]+Mcount[t])/K)  
    Mcount[t+1] <- Mcount[t]+(Rorig/2)*Mcount[t]*(1-(Ncount[t]+Mcount[t])/K)  
    
  }
  }
}

#concatenating two different vectors (wide form) into one Tumorgrowth vector (long form)
#first column is for timesteps
#second column is for the Ncount and Mcount vectors
#third column is to demarcate which data is for the normal and mutant cell counts (graphing purposes)
Tumorgrowth<-data.frame(time=c(1:timesteps, 1:timesteps),pop=c(Ncount, Mcount),type = rep(c("normal", "mutant"), each = timesteps))

#must access library before calling ggplot
library(ggplot2)

#ggplot call
#plotting cell count over time with demarcation of mutant or normal cells
#creating trend liine
ggplot(data=Tumorgrowth,aes(x=time,y=pop,color=type)) + 
geom_line() +
theme_classic()+
  xlab("Time") + 
  ylab("Population of Cells")


