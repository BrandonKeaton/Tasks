## Task 04: Revisiting a Lab
setwd("C:\\Users\\Brandon\\Desktop\\Tasks\\Task_04")
## II Sampling
trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)
Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)
#Yes the populations were differnt, population to had a larger upper exteme and a smaller lower quartile than population1.
boxplot(Sample1, Sample2)
## III Basic Genetics
source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")
Alan <- makeBaby(PatGrandma, PatGrandpa)
# Make Brenda
Brenda <- makeBaby(MatGrandma, MatGrandpa)
Focus <- makeBaby(Brenda, Alan)
# This should be about 50%
ToMom <- length(grep("mom", Focus))/length(Focus)
# This should be about 25%, no they don't match my expectation
ToMomDad <- length(grep("grandpa_mom", Focus))/length(Focus)
ToMomDad
ToMomMom <- length(grep("grandma_mom", Focus))/length(Focus)
ToMomMom
ToDadMom <- length(grep("grandma_da", Focus))/length(Focus)
ToDadMom
ToDadDad <- length(grep("grandpa_da", Focus))/length(Focus)
ToDadDad
# No It is more related to its maternal Grandfather. This is not what I expected, I thought they would be closer. The averagre relatedness is exatctly 0.25 ot 25%.
Sibling_01 <- makeBaby(Brenda, Alan)
ToSib <- length(intersect(Focus, Sibling_01))/length(Focus)
ToSib
# In theory I would expect them to share a very high percentage of their DNA, instead they share 57%.
##
ManySiblings <- replicate(1e3,length(intersect(Focus, makeBaby(Brenda, Alan)))/length(Focus))
ManySiblings
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main="" , xlab="proportion shared genes")
# Since a child isn't always equally related to each parent, children won't be equally related to their siblings, because they made me more related to their mother and less related to their father.
## IV Hardy-Weinberg Equalibrium
HWE <-function(p) {
aa <- p^2  
ab <- 2 * p * (1-p)
bb <- (1-p)^2
return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)
plot(1, 1, type="n", xlim=c(0,1), ylim=c(0,1), xlab="freq. allele a", ylab="geno. freq")
p <- seq(from=0, to=1, by= 0.01)
GenoFreq <- t(sapply(p, HWE))
lines(p, GenoFreq[,"aa"], lwd=2, col="red")
lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[,"ab"], lwd=2, col="blue")
legend("top", legend=c("aa", "ab", "bb"), col=c("red", "purple","blue"), lty=1, lwd=2, bty="n")
# As the a allele goes up the aa population increases and bb will decrease. As a decreases the b allele will go up. Time is not shown, neither is space.
Pop <- simPop(500)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=22, bg="red")
# Yes it does match the Hardy-Weinberg equallibrium
Pop <- simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
## The population size has decreased so the frequency of aa has also gone down.
## V Two-Allele Drift
install.packages("learnPopGen", dep=T)
library(learnPopGen)
x <- genetic.drift(Ne=200, nrep=5, pause=0.01)
PopSizes <- 5:50
Samples <- rep(PopSizes, 5)
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
# As population increases their are fewer samples, this means that the data set has more variation over time.