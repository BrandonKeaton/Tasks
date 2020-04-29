library("readxl")
read_excel("Salamander_Regression.xlsx")
SalamanderData <-read_excel("Salamander_Regression.xlsx")
Conductivity <- SalamanderData[1:13,5]
SalamanderData[1:13,5]
pH <- SalamanderData[1:13,3]
pH
SalamanderData[1:13,4]
TotalSalamanders <- SalamanderData[1:13,4]
library(ggplot2)
scatter.smooth(x=TotalSalamanders$Salamaders_found, y=pH$pH1, main= "Regression Analysis for pH and Salamanders")
cor(pH$pH1,TotalSalamanders$Salamaders_found)
pH$pH1
TotalSalamanders$Salamaders_found
scatter.smooth(x=TotalSalamanders$Salamaders_found, y=Conductivity$Conductivity1, main= "Regression Analysis for Conductivity and Salamanders")
cor(Conductivity$Conductivity1,TotalSalamanders$Salamaders_found)

