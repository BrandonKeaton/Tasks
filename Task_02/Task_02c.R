#Task 02C
#Hypothesis After he starts eating solid foods there will be a positive correlation between the amount that he eats and the number of bowel movements he has.
setwd("C:\\Users\\Brandon\\Desktop\\Tasks\\Task_02")
beren3 <- read.csv("beren_new.csv", stringsAsFactors=F)

# this will be used to match solids & dirty diapers even when a day lacks one
Blank <- rep(0, nrow(beren3))
names(Blank) <- beren3$age

BowelMovements <- which(beren3[,9] == "bowel")
 #finding dirty diapers
berenDiapers <- beren3[BowelMovements,]
#number of dirty diapers each day
ndirty <- tapply(berenDiapers$event, berenDiapers$age, length)
Dirty <- Blank
Dirty[names(ndirty)] <- ndirty
#Amount of Solid Food Each Day
SolidFoodDays <- which(beren3[,9] == "solids")
berenSolids <- beren3[SolidFoodDays,]
solidSum <- tapply(berenSolids$value, berenSolids$age, sum, na.rm=T)
Solids <- Blank
Solids[names(solidSum)] <- solidSum
# Matcing axes lengths
common <- intersect(names(solidSum), names(ndirty))
csolidSum <- solidSum[common]
cndirty <- ndirty[common]
#Graphing the Function
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-.01)
plot(x = csolidSum, y = cndirty)

par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-.01)
plot(as.numeric(names(solidSum)), solidSum)
points(as.numeric(names(ndirty)), ndirty, pch=16, col='red')

par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-.01)
plot(Solids, Dirty, pch=16, col=rgb(0,0,0,0.25))

# make prediction
Model <- lm(Dirty ~ Solids)
abline(Model)
summary(Model)
Parameters <- summary(Model)$coef
