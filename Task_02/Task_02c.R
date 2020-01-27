#Task 02C
#Hypothesis After he starts eating solid foods there will be a positive coreelation between the amount that he eats and the number of bowel movements he has.
setwd("C:\\Users\\Brandon\\Desktop\\Tasks\\Task_02")
beren3
BowelMovements <- which(beren3[,9] == "bowel")

berenDiapers <- beren3[BowelMovements,]
ndirty <- tapply(berenDiapers$event, berenDiapers$age, length)

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