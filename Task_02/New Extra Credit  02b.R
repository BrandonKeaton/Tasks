#Extra Credit
setwd("C:\\Users\\Brandon\\Desktop\\Tasks\\Task_02")

beren3 <- read.csv("beren_new.csv", stringsAsFactors=F)

Naps <- which(beren3$event == "nap")

beren4 <- beren3[Naps,]
head(beren4)

#Variable Names

beren4$end_minute <- beren4$end_minute / 60

beren4$start_minute <- beren4$start_minute / 60

beren4$napstart <- beren4$start_hour + beren4$start_minute

beren4$napend <- beren4$end_hour + beren4$end_minute

beren4$duration <- beren4$napend - beren4$napstart

naptime <- tapply(beren4$naplength, beren4$age, sum, na.rm=T)

#Plotting Graph

par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5 ,0), tck=-.01) 

plot(as.numeric(names(naptime)), naptime, type="b", pch=16, xlab="day", ylab="totalslepttime")
#Correlation Analysis
cor.test(beren4$age, beren4$duration)
# Since the correlation coefficient has a value of 0.22 there is a very negligible correlation between the time his nap starts and the duration of his naps.