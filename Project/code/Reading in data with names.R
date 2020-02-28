setwd("C:\\Users\\Brandon\\Desktop\\Tasks\\Project\\data") 
library(seqinr)

Data <- read.fasta("koala.gz")
Data

dotPlot(Data[[1]], Data[[2]])

# this is very slow
CodonMatrix1 <- matrix(NA, nrow=length(Data), ncol=64)
Matrix3 <- for (i in 1:nrow(CodonMatrix)){
  koala <- Data[[i]]
  Codons <- count(koala, 3)
  CodonMatrix[i,] <- Codons
}
# Naming matrices to seperate them
Matrix3
CodonMatrix1

# this might be faster
CodonMatrix2 <- sapply(Data, function(x) count(x, 3))
#Check Data
CodonMatrix2

#Reading in Excel Data
install.packages("readxl")
library("readxl")
read_excel("koala_microsatellite_genotypes (1).xlsx")


