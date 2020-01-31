# Downloading Data
install.packages("paleobioDB", dep = T)
setwd("C:\\Users\\Brandon\\Desktop\\Tasks\\Task_03")
library(paleobioDB)
Taxon <- "Dinosauria"
MinMA <- 66
MaxMA <- 252
fossils <- pbdb_occurrences(base_name = Taxon, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
#Analyzing the Data: Through Time
Res <- 5
nspeciesOverTime <- pbdb_richness(fossils, rank = "genus", temporal_extent = c(MaxMA, MinMA), res=Res)
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime)),nspeciesOverTime[ ,2],xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", main= Taxon)
newspeciesOverTime <- pbdb_orig_ext(fossils, res=5, rank="species",temporal_extent=c(MinMA, MaxMA))
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5, 0.5,0))
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime)), newspeciesOverTime[ , 1], xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", main=Taxon)
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime)), newspeciesOverTime[,2], col='red')
legend("topleft", legend=c("first appear", "go extinct"), col=c('black', 'red'), lty=1, bty="n")
#Analyzing the Data through Space
#The lines follow very similar trends up until 75 million years ago.No, the dinsaurs evolve the most at the end of the graph when extinction peaks.
OceanCol <- "light blue"
LandCol <- "black"
Cols <- c('#fee5d9', '#fcae91', '#fb6a4a', '#de2d26', '#a50f15')
par(las=0)
pbdb_map_richness(fossils, col.ocean=OceanCol, col.int= LandCol, col.rich=Cols)
#Analyzing the data through space and time
MinMA<- 201
MaxMA<- 252
triassic_fossils <- pbdb_occurrences(base_name = Taxon, show=c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
MinMA <- 145
MaxMA <- 201
jurassic_fossils <- pbdb_occurrences(base_name = Taxon, show=c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
MinMA <-66
MaxMA <- 145
cretaceous_fossils <- pbdb_occurrences(base_name = Taxon, show=c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
dev.new(height= 7.8, width=13)
pbdb_map_richness(triassic_fossils, col.ocean=OceanCol, col.int =LandCol, col.rich=Cols)
mtext(side=3, "Triassic (252 - 201Ma)", cex=3, line=-2)
#Analyzing the data : Comparing with another group
dev.new(height= 7.8, width=13)

pbdb_map_richness(jurassic_fossils, col.ocean=OceanCol, col.int =LandCol, col.rich=Cols)
mtext(side=3, "Jurassic (201 -145Ma)", cex=3, line=-2)
dev.new(height= 7.8, width=13)

pbdb_map_richness(cretaceous_fossils, col.ocean=OceanCol, col.int =LandCol, col.rich=Cols)
mtext(side=3, "Cretaceous (145 - 66Ma)", cex=3, line=-2)

Taxon2 <- "Mammalia"
MinMA <- 66
MaxMa <- 252
fossils2 <- pbdb_occurrences(base_name = Taxon2, show=c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
nspeciesOverTime2 <- pbdb_richness(fossils2 , rank = "genus", temporal_extent = c(MaxMA,MinMA), res=Res)

par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))

Col_dino <- Cols[length(Cols)]
Col_mammal <- Cols[1]
LineWidth <- 2

plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime)), nspeciesOverTime[,2], col = Col_dino, lwd=LineWidth, xlim=c(MaxMA, MinMA), type="1", xlab="age (millions of years ago)", ylab="num. of species", col=ColCol_dino, lwd=LineWidth)
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime)), nspeciesOverTime[,2], xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", col=Col_dino, lwd=LineWidth)
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime2)), nspeciesOverTime2[,2], col = Col_mammal, lwd=LineWidth)
legend("topleft", legend=c(Taxon,Taxon2), col=c(Col_dino, Col_mammal), bty="n", lwd=LineWidth)
#Extension
library(paleobioDB)
read(paleobioDB)
Taxon  
head(Taxon)
write.csv(anamalia, "Animal.csv", quote=F, row.names = FALSE)
write.csv(echinodermata, "Enchinoderm.csv", quote=F, row.names = FALSE)
echinodermata
Taxon3 <- "echinodermata"
Taxon3
MinMA <- 85
MaxMa <- 285
fossils <- pbdb_occurrences(base_name = Taxon3, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
Res <- 5
nspeciesOverTime <- pbdb_richness(fossils, rank = "genus", temporal_extent = c(MaxMA, MinMA), res=Res)
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime)),nspeciesOverTime[ ,2],xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", main= Taxon3)
newspeciesOverTime <- pbdb_orig_ext(fossils, res=5, rank="species",temporal_extent=c(MinMA, MaxMA))
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5, 0.5,0))
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime)), newspeciesOverTime[ , 1], xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", main=Taxon3)
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime)), newspeciesOverTime[,2], col='red')
legend("topleft", legend=c("first appear", "go extinct"), col=c('black', 'red'), lty=1, bty="n")
OceanCol <- "light blue"
LandCol <- "black"
Cols <- c('#fee5d9', '#fcae91', '#fb6a4a', '#de2d26', '#a50f15')
par(las=0)
pbdb_map_richness(fossils, col.ocean=OceanCol, col.int= LandCol, col.rich=Cols)
Taxon4 <- "Chordata"
MinMA <- 85
MaxMa <- 285
fossils <- pbdb_occurrences(base_name = Taxon, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
Res <- 5
nspeciesOverTime <- pbdb_richness(fossils, rank = "genus", temporal_extent = c(MaxMA, MinMA), res=Res)
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime)),nspeciesOverTime[ ,2],xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", main= Taxon4)
newspeciesOverTime <- pbdb_orig_ext(fossils, res=5, rank="species",temporal_extent=c(MinMA, MaxMA))
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5, 0.5,0))
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime)), newspeciesOverTime[ , 1], xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", main=Taxon4)
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime)), newspeciesOverTime[,2], col='red')
legend("topleft", legend=c("first appear", "go extinct"), col=c('black', 'red'), lty=1, bty="n")
OceanCol <- "light blue"
LandCol <- "black"
Cols <- c('#fee5d9', '#fcae91', '#fb6a4a', '#de2d26', '#a50f15')
par(las=0)
pbdb_map_richness(fossils, col.ocean=OceanCol, col.int= LandCol, col.rich=Cols)
fossils2 <- pbdb_occurrences(base_name = Taxon2, show=c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
nspeciesOverTime2 <- pbdb_richness(fossils2 , rank = "genus", temporal_extent = c(MaxMA,MinMA), res=Res)

par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))

Col_echino <- Cols[length(Cols)]
Col_chorda <- Cols[1]
LineWidth <- 2

plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime)), nspeciesOverTime[,2], xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", col=Col_echino, lwd=LineWidth)
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime2)), nspeciesOverTime2[,2], col = Col_chorda, lwd=LineWidth)
legend("topleft", legend=c(Taxon3,Taxon4), col=c(Col_echino, Col_chorda), bty="n", lwd=LineWidth)
# I hypothesize that chordata and echinodermata have a positive correlation because the atmosphere is more suited to life in certain time periods.

## VIII Extra Credit
#eBird
install.packages("auk")
f_in <- system.file("extdata/ebd-sample.txt", package = "auk")
f_out <- "ebd_filtered_grja.txt"
auk_species(species = "Canada Jay")%>%
read_ebd() 

  