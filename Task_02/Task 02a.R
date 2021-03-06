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
avgMilk <- mean(beren$value[Feeds])
avgFeed <- tapply(beren$value[Feeds], beren$age [Feeds], mean)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalfeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds] , beren3$age[Feeds], length)
Feeds <- which(beren3$event == "bottle")
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds]~beren3$caregiver[Feeds])
boxplot( beren3$value[Feeds]~beren3$caregiver[Feeds], xlab= "who gave the bottle", ylab = "amount of milk consumed (oz)")
par(las=1, mar=c(5,5,1,1), mgp=c(2, 05, 0), tck=-.01)
plot(as.numeric(names(totalfeed)), totalfeed, type="b", pch=16, xlab=" age in days", ylab="ounces of milk")
abline(h=avgFeed, lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height = 4, width = 4)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5,0), tck=-0.01)
plot(as.numeric(names(totalfeed)), totalfeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")