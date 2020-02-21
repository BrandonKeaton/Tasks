setwd("C:\\Users\\Brandon\\Desktop\\Tasks\\Task_06")
library("learnPopGen")
?coalescent.plot()
coalescent.plot(n=10, ngen=20, colors=NULL)
coalescent.plot(n=25, ngen=30, colors=NULL)
coalescent.plot(n=5, ngen=20, colors=NULL)
coalescent.plot(n=3, ngen=20, colors=NULL)
coalescent.plot(n=100, ngen=50, colors=NULL)
#The number of alleles depends on what number you set n equal to.
##It depends on the number alleles, for 5 alleles it seems to be about 5 generations. The number of alleles and the number of generations it takes to become fixated seem to be about the same. There is definitley a correlation between the population size initially and how many generations it takes for an allele to be driven to fixation.
###Each individual can have 0-3 offspring however the average number of offspring is always 1.
#### Fitness does not play a role in this situation, drift and chance are the only two factors that play a role in driving an allele into fixation.
##### No, the most common recent ancestor can be tracked back to a narrow spot in the plot and by following the lines you can see if its traits were passed on, if not it wouldn't be the most recent acommon ancestor. 
#install.packages("coala")
#install.packages("phytools")
#install.packages("ape")
#install.packages("maps")

library("coala")
library("phytools")

model<- coal_model(sample_size=5, loci_number=10, loci_length=500, ploidy=2)+
    feat_mutation(10)+
    feat_recombination(10)+
    sumstat_trees()+
    sumstat_nucleotide_div()

stats <- simulate(model, nsim=1)

Diversity<- stats$pi
#
Nloci <- length(stats$trees)
t1<- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()

## The number doesn't match because the simulation was set to diploidy so each organism has 2 alleles. 
Agel<- max(nodeHeights(t1))
t2<- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()

### They do not match, the first figure's most common ancestor is 0.8 away while the second figures MRCA is 0.3 away. The numbers will vary but they will almost alwats be different.
par(mfrow=c(1,3))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()

compare.chronograms(t1,t2)

t1_1<- read.tree(text=stats$trees[[1]][1])
t1_2<- read.tree(text=stats$trees[[1]][2])

par(mfrow=c(1,2))
compare.chronograms(t1_1, t1_2)

x <- ape:::c.phylo(t1, t2)
densityTree(x)

for(locus in 1:Nloci) {
  ntrees <- length(stats$trees[[locus]])
  for(n in 1:ntrees) { 
    if(locus==1&& n==1) {
      outPhy <- read.tree(text=stats$trees[[locus]][n])
    }
    else {
      outPhy <- ape:::c.phylo( outPhy, read.tree(text=stats$trees[[locus]][n]))
    }
  }
}
par(mfrow=c(1,1))
densityTree(outPhy, type = "cladogram")


model3 <-coal_model(10, 50)+
feat_mutation(par_prior("theta", sample.int(100, 1)))+
sumstat_nucleotide_div()
stats <- simulate(model3, nsim=40)
mean_pi<- sapply(stats, function(x) mean(x$pi))
theta <- sapply(stats, function(x) x$pars[["theta"]])
plot(x=mean_pi, y=theta)
abline(lm(mean_pi~theta))
