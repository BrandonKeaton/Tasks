setwd("C:\\Users\\Brandon\\Desktop\\Tasks\\Task_07")
source("http://jonsmitchell.com/code/reformatData07.R")
source("http://jonsmitchell.com/code/simFxn.R")
#Plotting Data
plot(1,1, type = "n", xlim=c(1998, 2013), ylim=c(0,1))
s <- apply(overallFreq, 2, function(x) lines(overallFreq[,1], x, col=rgb(0,0,0,0.01)))     
smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin = 100)
smoothScatter(alleleFreqs$year, alleleFreqs$rfreq, colramp = Pal, nbin=100, xlab="year", ylab="change in allele freq. since 1998")  
addFit(nruns=50, n=100, ngens=18, startT = 1997, simCol = "gray40", rescale = TRUE)
##Practice (Very Slow) Has been runnung for 8 Hours
addFit(nruns=50, n=6936, ngens=4, startT = 1990, simCol = "gray40", rescale = TRUE)
#Back to the code
plot(alleleFreqs$d_freq, alleleFreqs$d_imm, xlim=c(-0.15, 0.15), xlab="overall freq. change", ylab="freq. change in subset")

#Birth and Survival
points(alleleFreqs$d_freq, alleleFreqs$d_birth, col='blue')     
points(alleleFreqs$d_freq, alleleFreqs$d_surv, col='red')
