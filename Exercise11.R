##mutant strain grows at .5 the rate of normal growth
##mutant: M(t+1)=M(t)+r*M(t)(1-((N(t)+M(t))/K))
##normal: N(t+1)=N(t)+r*N(t)(1-((N(t)+M(t))/K))
##curent growth rate is .1 per day
##K= 1 million cells
##a single cell becomes mutated when there are 100 total cells
#drug treatment of non-mutant results in growth rate of -.1

##generate a script that simulates the growth of the two subpopulations
##to equilibrum followed by drug treatment

##start with 99 normal cella and 1 mutant cell
NCn=99
NCm=1
NTotal=0
rn=0.1
rm=0.1
rndrug=-0.1
rmdrug=0.05
K=1000000
total <- c()
iteration <- c()
i=1

##set an initial guess of iterations at 1000, then see where the curve flattens out and readjust graph
##for this case, it reached the carrying capacity at 323 iterations


##expansion with no presence of drug
while(NTotal<=K && i<=323){
  NCn=NCn+rn*NCn*(1-((NCn+NCm)/K))
  NCm=NCm+rm*NCm*(1-((NCn+NCm)/K))
  NTotal=NCn+NCm
  total[i]=NTotal
  iteration[i]=i
  i=i+1
}

##contraction with the presence of drug
##keep the same vector variables so they can be continuously graphed
##set an initial guess of iterations at 1000, then see where the curve flattens out and readjust graph
##for this case, it reached carrying capacity at 1000 iterations (677 iterations with drug present)

while(NTotal<=K && i<1000){
  NCn=NCn+rndrug*NCn*(1-((NCn+NCm)/K))
  NCm=NCm+rmdrug*NCm*(1-((NCn+NCm)/K))
  NTotal=NCn+NCm
  total[i]=NTotal
  iteration[i]=i
  i=i+1
}


library(ggplot2)
sim <- data.frame(step=iteration, result=total)
ggplot(data=sim, aes(x=step, y=result))+
  labs(title="Tumor Cell Population Progression")+
  xlab("Iteration")+
  ylab("Population Size")+
  geom_line()+
  theme_classic()

##The graph makes sense because the tumors will initially grow, with the mutant form growing a lot
##more slowly than the normal cells (when the initial grow curve flattens out at carrying capacity
##the normal cells are 100 times more prevalent than the mutant form).
##However, when the drug is introduced, the normal cells will begin to die, while the mutant cells
##will just grow a little slower. So the big drop in the graph is all the normal cells dying, however
##you can see it doesn't go all the way down to zero, because the mutants are still there. And after
##all the normal cells are dead, the mutants will grow at a rate that is half of the growth rate of 
##the normal cells, which is why the slope of the second increase is about half the first. 

##This shows how mutant cells can keep a tumor alive, and bring it to the full carrying capacity
##even when exposed to drugs that kill all the normal tumor cells.

