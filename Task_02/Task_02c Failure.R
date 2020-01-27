#Task 02C
#Hypothesis After he starts eating solid foods there will be a positive correlation between the amount that he eats and the number of bowel movements he has.
setwd("C:\\Users\\Brandon\\Desktop\\Tasks\\Task_02")
beren3 <- read.csv("beren_new.csv", stringsAsFactors=F)
BowelMovements <- which(beren3[,9] == "bowel")
 #finding dirty diapers
berenDiapers <- beren3[BowelMovements,]
#number of dirty diapers each day
ndirty <- tapply(berenDiapers$event, berenDiapers$age, length)

#finding the intro to solid food 
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
#failed attempt
DirtyDiaper <- tapply(ndirty, berenSolids$age, sum, na.rm=T)
avgFeed <- tapply(beren3$value[Feeds], beren3$age [Feeds], mean)
##Failed attempts at plotting
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-.01)
plot(x = solidSum, y =ndirty)
plot(x = solidSum, y =ndirty, type="l")

## Attempts to determine why plottting failed
ncol(ndirty)
ncol(solidSum)
nrow(ndirty)
nrow(ndirty)-14
ndirty>154
nrow(solidSum)
avg()
matchdata <- merge(ndirty, solidSum by.x = "age", by.y = "age", all.x = TRUE)
find.matches(x, y, tol=rep(0, ncol(y)), scale=tol, maxmatch=10)
matchdata(x, y, tol=rep(0, ncol(y)), scale=tol, maxmatch=10)
match((x,y,tol=rep(0, ncol(y)), scale=tol, maxmatch=10))
match_df(x, y, on = NULL)
matchdata(solidSum, ndirty)
merge(ndirty, solidSum, by = "beren3$age")
common <- intersect(solidSum, ndirty)
common
