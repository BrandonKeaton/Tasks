#Task 02C
#Hypothesis After he starts eating solid foods there will be a positive correlation between the amount that he eats and the number of bowel movements he has.
setwd("C:\\Users\\Brandon\\Desktop\\Tasks\\Task_02")
beren3 <- read.csv("beren_new.csv", stringsAsFactors=F)
BowelMovements <- which(beren3[,9] == "bowel")
 
berenDiapers <- beren3[BowelMovements,]
ndirty <- tapply(berenDiapers$event, berenDiapers$age, length)

#use which to find the thing date 
keyDate <- beren3[grep("intro_solidfood", beren3$event), "age"]

#use which to find younger before solid foods
youngerDays <- which(beren4$age < keyDate)
youngerBeren<- beren4[youngerDays,]
youngerSlope <- lm(ndirty ~ age, data=youngerBeren)

olderDays <- which(beren4$age > keyDate)
olderBeren <- which(beren4$age > keyDate )
berenDiapers

SolidFoodDays <- which(beren3[,9] == "solids")
berenSolids <- beren3[SolidFoodDays,]
solidSum <- tapply(berenSolids$value, berenSolids$age, sum, na.rm=T)
##Failed attempts at plotting
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-.01)
plot(x = solidSum, y =ndirty)
## Attempts to determine why plottting failed
ncol(ndirty)
ncol(solidSum)
nrow(ndirty)

nrow(solidSum)
avg()