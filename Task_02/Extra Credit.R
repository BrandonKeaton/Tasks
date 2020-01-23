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
duration <- Beren4$NapEndTime - Beren4$NapStartTime  
TotalNapsEachDay <- tapply(Beren4$day[Naps], Beren4$age [Naps], sum)
TotalSleepTimePerNap <- TotalNapsEachDay
par(las=1, mar=c(5,5,1,1), mgp=c(2, 05, 0), tck=-.01)
plot(as.numeric(names(TotalSleepTimePerNap)), TotalSleepTimePerNap, type="b", pch=16, xlab="Day", ylab="TotalSleptTime")
cor.test(Beren4$NapStartTime[TotalSleepTimePerNap], Beren4$duration[duration])
cor.test(Beren4$age, duration)
# Since the correlation coefficient has a value of 0.22 there is a very negligible correlation between the time his nap starts and the duration of his naps.