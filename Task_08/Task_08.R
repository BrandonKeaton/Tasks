setwd("C:\\Users\\Brandon\\Desktop\\Tasks\\Task_08")
#Potenial Mistake
install.packages("phylotools")
###Potential Fix
install.packages("phytools")
install.packages("ape")
library(phylotools)
library(ape)
library(phytools)
text.string <- "(((((((cow, pig), whale),(bat,(lemur, human))),(robin, iguana)), coelacanth), (gold_fish, trout)),shark);"
vert.tree <- read.tree(text=text.string)
plot(vert.tree, edge.width=2)
nodelabels(frame="circle", bg='white', cex=1)
##Question 1: The shark is more closely realted to a human than a goldfish
vert.tree
##Question 2 : No, there are not branch lengths in this tree.
str(vert.tree)
tree<-read.tree(text="(((A,B),(C,D)),E);")

plotTree(tree ,offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)
tree$tip.label
tree$edge
## Lizards
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1) 
hist(AnolisTree$edge.length, col='black', border='white', main="", xlab="edge lengths for the Anolis tree", ylim=c(0,50), xlim=c(0, 6))
tipEdges <- which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths)[which(Lengths ==min(Lengths))]
plot(AnolisTree, cex=0.25)      
Labs <-sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
###Question 3 :
?plot.phylo
plot(vert.tree, edge.width=2, show.tip.label=FALSE)
#####Question 4
plot(vert.tree, edge.width=2, show.tip.label=FALSE, type = "unrooted")
##########Question 5
plot(vert.tree, edge.width=2, show.tip.label=TRUE, type = "unrooted", tip.color = "red")
###############Question 6
text.string <- "(((((((cow, pig), whale),(bat,(lemur, human))),(robin, iguana)), coelacanth), (gold_fish, trout)),shark);"
vert.tree <- read.tree(text=text.string)
plot(vert.tree, edge.width=2)
nodelabels(frame="circle", bg='white', cex=1)
edgelabels(tree$edge.length)
tree$tip.label
########################### The Iguana and the Robin  has the shortest edge length at 0.014
#########################Question 7
vert.tree<-(drop.tip("robin, iguana"))

############################################ Question 10
ltt(AnolisTree)
abline(0, 1, lwd=2, col='red', lty=2)
#The line never goes does down because even if a species goes extinct their lineage still exists, therefore it will go up continually. The slope of the line does decrease even though it remains positive, it tells you that the rate of speciation is slowing.
fit.bd(AnolisTree, rho=0.2)
############################################## Birth rate of species= 0.8031 Deathrate of species=0 