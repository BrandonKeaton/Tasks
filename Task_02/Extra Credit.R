
# Extra Credit
beren
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
NapEndTime <- Beren4[7] : Beren4$end_minute /60
Beren4$NapStartTime <- Beren4$start_hour : Beren4$start_minute
Beren4$NapEndTime <- Beren4$end_hour : Beren4$end_minute
