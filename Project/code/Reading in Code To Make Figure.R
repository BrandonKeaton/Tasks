#####Don't Run until Line 20 or it will take ages
setwd("C:\\Users\\Brandon\\Desktop\\Tasks\\Project\\data")
library(seqinr)

Data <- read.fasta("koala.gz")
Data
dotPlot(Data[[1]], Data[[2]])

# this is very slow
CodonMatrix <- matrix(NA, nrow=length(Data), ncol=64)
for (i in 1:nrow(CodonMatrix)){
  koala <- Data[[i]]
  Codons <- count(koala, 3)
  CodonMatrix[i,] <- Codons
}
CodonMatrix
# this might be faster
CodonMatrix <- sapply(Data, function(x) count(x))
############# This Dataset was too large for github, so I had to restart
install.packages("readxl")
library("readxl")
read_excel("koala_microsatellite_genotypes (1).xlsx")
KoalaPhci <- read_excel("koala_microsatellite_genotypes (1).xlsx")
KoalaPhci
length(KoalaPhci)
nrow(KoalaPhci)
ncol(KoalaPhci)
colnames(KoalaPhci)
head(KoalaPhci)
KoalaPhci[1,]
KoalaPhci[,1]
KoalaPhci[,1:23]
BrisbaneRanges <- which(KoalaPhci[,1]=="Brisbane Ranges")
StonyRises <- which(KoalaPhci[,1]=="Stony Rises")
USZoo <- which(KoalaPhci[,1]=="US zoo")
head(BrisbaneRanges)
BrisbaneRanges
StonyRises
USZoo
StonyRises22 <- StonyRises[,1:22]
BrisbaneRanges <- which(KoalaPhci[,1:28]=="Brisbane Ranges")
#which(KoalaPhci$"Phci2")
#plot(x = BrisbaneRanges, y = USZoo)
?readxl
KoalaPhci[1:22,]
BrisbaneRanges22 <- KoalaPhci[1:22,]
StonyRises22 <- KoalaPhci[23:44,]
USZoo22 <- KoalaPhci[47:68,]
plot(x = BrisbaneRanges22, y = USZoo22)
ncol(USZoo22)
ncol(BrisbaneRanges22)
ncol(StonyRises22)
nrow(USZoo22)
nrow(BrisbaneRanges22)
nrow(StonyRises22)
###Making a Bar Graph
install.packages("ggplot2")
library(ggplot2)

data0 <- data.frame(
  name=c("Brisbane Ranges Phci Avg","Stoney Rises Phci Avg","US Zoo Phci Avg") ,  
  value=c(172.669,173.2,182.564)
)
ggplot(data0, aes(x=name, y=value)) + 
  geom_bar(stat = "identity")
# Chaning Colors
ggplot(data0, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", color="blue", fill=rgb(0.1,0.4,0.5,0.7))
######
ggplot(data0, aes(x=name, y=value)) + 
  geom_bar(stat = "identity") +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")
###############
ggplot(data0, aes(x=name, y=value)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")
########################
ggplot(data0, aes(x=name, y=value)) + 
  geom_bar(stat = "identity") +
scale_fill_grey(start = 0.25, end = 0.75) +
  theme(legend.position="none",axis.text.x = element_text(size=6) )
####################################
ggplot(data0, aes(x=name(cyl), fill=as.factor(cyl) )) +  
  geom_bar( ) +
  scale_fill_manual(values = c("red", "green", "blue") ) +
  theme(legend.position="none")
##################################################### Finally!The Actual Plot in Different Colors
ggplot(data0, aes(x=name, y=value, fill=name)) +
  geom_bar(stat="identity")+theme_minimal()
######### Data from each Phci Brisbane Ranges
Data1 <- data.frame(
  name=c("BR Phci 2", "Br Phci 5","BR Phci 9", "BR Phci 15", "BR Phci 17", "BR Phci 18", "BR Phci 19","BR Phci 21","BR Phci 22", "BR Phci 23", "BR Phci 24", "BR Phci 27", "BR Phci 30") ,  
  value=c(167.8181818,
          160.5,
          173,
          190.0952381,
          156.3809524,
          171.2857143,
          171.5238095,
          178,
          189.3809524,
          157.7142857,
          165.2857143,
          186.6666667,
          177.047619
  )
)
ggplot(Data1, aes(x=name, y=value, fill=name)) +
  geom_bar(stat="identity")+theme_minimal()
############### Data from each phci Stoney Rises
Data2 <- data.frame(
  name=c("SR Phci 2", "Sr Phci 5","SR Phci 9", "SR Phci 15", "SR Phci 17", "SR Phci 18", "SR Phci 19","SR Phci 21","SR Phci 22", "SR Phci 23", "SR Phci 24", "SR Phci 27", "SR Phci 30") ,  
  value=c(168.5,
          166.75,
          173.1875,
          202.25,
          156.125,
          171.9166667,
          173.625,
          170.625,
          187.4166667,
          161,
          185.4166667,
          155.2083333,
          179.5833333
  )
)
ggplot(Data2, aes(x=name, y=value, fill=name)) +
  geom_bar(stat="identity")+theme_minimal()
############### Data from each phci US Zoo
Data3 <- data.frame(
  name=c("US Phci 2", "US Phci 5","US Phci 9", "US Phci 15", "US Phci 17", "US Phci 18", "US Phci 19","US Phci 21","US Phci 22", "US Phci 23", "US Phci 24", "US Phci 27", "US Phci 30") ,  
  value=c(156.7037037,
          164.9259259,
          177.9444444,
          229.8888889,
          160.5555556,
          171.4444444,
          169.6666667,
          183.6666667,
          191.2222222,
          186.6111111,
          184.1481481,
          199.7037037,
          196.8518519
  )
)
ggplot(Data3, aes(x=name, y=value, fill=name)) +
  geom_bar(stat="identity")+theme_minimal()
###### Attempt to combine the last 3
TotalData <- data.frame(
  name=c("BR Phci 2", "Br Phci 5","BR Phci 9", "BR Phci 15", "BR Phci 17", "BR Phci 18", "BR Phci 19","BR Phci 21","BR Phci 22", "BR Phci 23", "BR Phci 24", "BR Phci 27", "BR Phci 30", "SR Phci 2", "Sr Phci 5","SR Phci 9", "SR Phci 15", "SR Phci 17", "SR Phci 18", "SR Phci 19","SR Phci 21","SR Phci 22", "SR Phci 23", "SR Phci 24", "SR Phci 27", "SR Phci 30", "US Phci 2", "US Phci 5","US Phci 9", "US Phci 15", "US Phci 17", "US Phci 18", "US Phci 19","US Phci 21","US Phci 22", "US Phci 23", "US Phci 24", "US Phci 27", "US Phci 30") ,
  value=c(167.8181818,
          160.5,
          173,
          190.0952381,
          156.3809524,
          171.2857143,
          171.5238095,
          178,
          189.3809524,
          157.7142857,
          165.2857143,
          186.6666667,
          177.047619,
          168.5,
          166.75,
          173.1875,
          202.25,
          156.125,
          171.9166667,
          173.625,
          170.625,
          187.4166667,
          161,
          185.4166667,
          155.2083333,
          179.5833333,
          156.7037037,
          164.9259259,
          177.9444444,
          229.8888889,
          160.5555556,
          171.4444444,
          169.6666667,
          183.6666667,
          191.2222222,
          186.6111111,
          184.1481481,
          199.7037037,
          196.8518519
  )
)
ggplot(TotalData, aes(x=name, y=value, fill=name)) +
  geom_bar(stat="identity")+theme_minimal()         
### T Tests
BR <- c(167.8181818, 160.5, 173, 190.0952381, 156.3809524, 171.2857143,171.5238095,178,189.3809524,157.7142857, 165.2857143,186.6666667,177.047619)
SR <- c(168.5,
        166.75,
        173.1875,
        202.25,
        156.125,
        171.9166667,
        173.625,
        170.625,
        187.4166667,
        161,
        185.4166667,
        155.2083333,
        179.5833333)



US <- c(156.7037037,
        164.9259259,
        177.9444444,
        229.8888889,
        160.5555556,
        171.4444444,
        169.6666667,
        183.6666667,
        191.2222222,
        186.6111111,
        184.1481481,
        199.7037037,
        196.8518519)

t.test(BR,SR)
t.test(BR,US)
t.test(SR,US)
?mar
ggplot(TotalData, aes(x=name, y=value, fill=name)) +
  geom_bar(stat="identity")+theme_minimal() 

theme(legend.position="none",axis.text.x = element_text(size=6))

ggplot(TotalData, aes(x=name, y=value, fill=name)) +
  geom_bar(stat="identity")+theme(axis.text.x = element_text(size=3))

ggplot(TotalData, aes(x=name, y=value, fill=name)) +
  geom_bar(stat="identity")+theme(axis.text.x = element_text(size=, angle = 90))

ggplot(Data2, aes(x=name, y=value, fill=name)) +
  geom_bar(stat="identity")+theme(axis.text.x = element_text(size=10, angle = 90))

ggplot(Data3, aes(x=name, y=value, fill=name)) +
  geom_bar(stat="identity")+ theme(axis.text.x = element_text(size=10, angle = 90))                             

ggplot(Data1, aes(x=name, y=value, fill=name)) +
  geom_bar(stat="identity")+theme(axis.text.x = element_text(size=10, angle = 90))
            
ggplot(data0, aes(x=name, y=value, fill=name)) +
  geom_bar(stat="identity")+theme(axis.text.x = element_text(size=10, angle = 8))        

                                  