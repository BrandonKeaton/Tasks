#Simulation
setwd("C:\\Users\\Brandon\\Desktop\\Tasks\\Task_05")
source("http://jonsmitchell.com/code/fxn05.R")
Pop1 <- simPop(Popsize = 50, nGenerations = 100, initial_p = 0.5, h=1, s=0)
plot(1:nrow(Pop1), Pop1[,1], ylim=c(0, 1), type = "l", xlab="generation", ylab="allele freq.", lwd=2)
lines(1:nrow(Pop1), Pop1[,2], lwd=2, col='red')
legend("topleft", legend = c("a", "b"), col = c("black", "red"), lwd=2, bty="n")
plotFit( nruns = 10, n=50, ngens = 100, init_p = 0.5, h=1, s=0)
## What is a test statistic

Observed <- c(15, 15, 5, 5)
Chisq <- sum( ( ( Expectation- Observed) ^2) / Expectation)
barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))
## The chi squared value for 4 10's is 10. 
Expectation <- c(40)
Chisq <- sum( ( ( Expectation- Observed) ^2) / Expectation)
# The chi squared value for one column of 40 is 92.5
barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))
# The more even the bars are, the lower the chi squared value will be.

setwd("C:\\Users\\Brandon\\Desktop\\Tasks\\Task_05")
### Reviewing the lab
results <- read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors = F)
counts <- results[,c("yellow", "red", "green", "blue", "black", "tan")]

backgrounds <- c("White", "Red", "Yellow", "Green", "Blue", "Black")
backgroundCol <- c("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")
calcChi(counts[1,])
Chisqs <- apply(counts, 1, calcChi)
plotChis(counts)
## The chi squared value is very high when the bars are uneven, and very low when the bars are even. A small chi squared value means the expected and observed values are very similar.
##The lower the chi squared value and the more even the bars are, the closer the observed data is to the expected data.
# The average is about 61 which means that the results werent what was expected. Since its much greater than the critical value there must be a significant difference.
Avg <- mean(Chisqs)
backgroundAvgs <- tapply(Chisqs, results[,3], mean)
propSig <- length( which( Chisqs > 11.70) ) / length(Chisqs)
percSig <- round (100 * propSig)
#Yes i wouldn't have expected that 92% of the results were different from the expectation.
# No i imagine there are more factors.
par(las = 1, mar = c(4, 4, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis=1)
hist(Chisqs, main="", xlab="chi-squared values", ylab="frequency")
par(las = 1, mar = c(4, 4, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis=1)
plot(1, 1, xlim=c(0, 400), ylim=c(1, 8.5), xlab="", ylab="", type="n", yaxt="n")
axis(2, at = 1:length (backgrounds), labels=backgrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)
counter <- 1
for (i in backgrounds) {
  Data <- Chisqs[which(results[,3] ==i)]
  addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
  counter <- counter + 1 
}
abline(v = 11.70, lty=2, lwd = 2,col='black')
## It is diffucult to tell due to the size of the figure, but they all seem to be similar
## Running a simulation
Simulation <- simDraws(10000)
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line=0, "simulated")
abline(v=11.70, lty=2, lwd=2)
Fit <- c(1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation2 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))
Fit <- c(0.1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation3 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0,0,0,0.25))
Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit) <- 1:6
Simulation4 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0,0,0,0,0.25))
Fit <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit)<- 1:6
Simulation5 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0,0,0,0,0.25))
Fit <- c(0.1, 0.1, 0.1, 0.1, 0.1, 1)
names(Fit) <- 1:6
Simulation6 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0,0,0,0,0.25))
mtext(side=2, at=8, line=0, "sel. sim.")   
Simulation7 <- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0,0,0,0,0.25))
### The simulated is a more realistic expectation of how things would work. 
### Yes, most of the student groups showed strong evidence of selection, because of the values to the right of the set value on the graph, showing a high chi swuared value and thus a large difference from the expected values. 
#### There is directional selection present in all the groups that is relatively strong. Some type of selection always occurs in this lab set up.
# VII Inference
# Directional selection is present based on the results and the lab set up and the grpahs shopw evidence of motre drift than the computer simulations.
## The computer simulations has directional selection and drift but less drift than the student results.
### The relative strength of the student simulations is quite strong becuase of the amount of drift present
#### Comparing the values to a critical number is more helpful because the results can be judged on a consistant basis.
##### The chi squared value would go down because the new color would create a new allele that could be selected for or against, allowing more selection to occurr for the best color.
