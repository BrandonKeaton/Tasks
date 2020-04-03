setwd ("C:\\Users\\Brandon\\Desktop\\Tasks\\Task_09")
library(phytools)
library(ape)
tree <- read.tree("http://www.phytools.org//Cordoba2017//data//Anolis.tre")
plot(tree, type="fan")
tree$tip.label
tree$edge
?phytools
library(help = phytools)
#######Question 1
tiplabels(frame="circle", bg='lightblue', cex=1)
tree$tip.label
head(tree)
edgelabels(tree$edge.length, bg="black", col="white", font=2)
data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors=F, row.names=1)
