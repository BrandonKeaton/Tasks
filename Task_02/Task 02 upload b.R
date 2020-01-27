setwd("C:\\Users\\Brandon\\Desktop\\Tasks\\Task_02")
Data <- read.csv("http://jonsmitchell.com/data/beren.csv" , stringsAsFactors = F)
Data
write.csv(Data, "raw_data.csv", quote=F)

length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1 ,]
Data[2 ,]
Data[1:3 ,]
Data[1:3 , 4]
Data[1:5 , 1:3]
beren <- Data
Feeds <- which(beren[,9] == "bottle")
berenMilk <- beren[Feeds,]
head(berenMilk)
Feeds <- which(beren[,"event"] == "bottle")
Feeds <- which(beren$event == "bottle")
dayID <- apply(beren, 1, function(x) paste(x[1:3], collapse="-"))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin =" 2019-04-18")
beren$age <- dateID - dateID[which(beren$event == "birth")]
head(beren)
beren2 <- beren
beren3 <- beren2[order(beren2$age),]
write.csv(beren3, "beren_new.csv", quote=F, row.names = FALSE)

#### Task 02b
# Question 01 There is very little data on food and it is seperated by when he started solid food.

beren3 <- read.csv("beren_new.csv", stringsAsFactors=F)
Feeds <- which(beren3$event == "bottle")

# This calculates average feed per day
avgMilk <- mean(beren3$value[Feeds])
# Value is 2.41 in fluid ounces per day
# Because the value column is the number of ounces of feed he consumed
# It refers to the object we saved as feeds previously
avgFeed <- tapply(beren3$value[Feeds], beren3$age [Feeds], mean)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalfeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds] , beren3$age[Feeds], length)

# This is a correlation analysis
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])

summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds]~beren3$caregiver[Feeds])

boxplot( beren3$value[Feeds]~beren3$caregiver[Feeds], xlab= "who gave the bottle", ylab = "amount of milk consumed (oz)")

?par
par(las=1, mar=c(5,5,1,1), mgp=c(2, 05, 0), tck=-.01)
plot(as.numeric(names(totalfeed)), totalfeed, type="b", pch=16, xlab=" age in days", ylab="ounces of milk")
abline(h=mean(totalfeed), lty=2, col='red')

pdf("r02b-totalMilkByDay.pdf", height = 4, width = 4)

par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5,0), tck=-0.01)
plot(as.numeric(names(totalfeed)), totalfeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalfeed), lty=2, col='red')

dev.off()

## Question 02 The data only shows what occurred at daycare and doesn't account for the rest of the time. It was also taken by various care givers who may have done things differently.
 
# Extra Credit
setwd()
beren3 <- read.csv("beren_new.csv", stringsAsFactors=F)

Naps <- which(beren3[,9]== "nap")
BerenNap <- beren3[Naps,]
Beren4 <- BerenNap
head(Beren4)
Beren4[5:6]
Beren4[7:8]
Beren4[7:8]-Beren4[5:6]
head(Beren4)
NapEndTime <- Beren4[7:8]/60
NapStartTime <- Beren4[5:6]/60
NapEndTime - NapStartTime
NapStartTime + NapEndTime
Beren4$end_minute <- Beren4$end_minute / 60
Beren4$start_minute <- Beren4$start_minute / 60
Beren4$NapStartTime <- Beren4$start_hour + Beren4$start_minute
Beren4$NapEndTime <- Beren4$end_hour + Beren4$end_minute

