
setwd("C:\\Users\\Brandon\\Desktop\\Tasks\\Project\\data")
library(seqinr)

Data <- read.fasta("koala.gz")

dotPlot(Data[[1]], Data[[2]])

# this is very slow
CodonMatrix <- matrix(NA, nrow=length(Data), ncol=64)
for (i in 1:nrow(CodonMatrix)){
  koala <- Data[[i]]
  Codons <- count(koala, 3)
  CodonMatrix[i,] <- Codons
}

# this might be faster
CodonMatrix <- sapply(Data, function(x) count(x, 3))

install.packages("readxl")
library("readxl")
read_excel("koala_microsatellite_genotypes (1).xlsx")

