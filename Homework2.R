library(ggplot2)
library(dplyr)

laborData <- read.csv("C:/Users/54208/Google Drive/School/Graduate School/Masters/Fall_2020/MSA8020/laborData.csv")


laborData$Unemployed.Level <- gsub(",","",laborData$Unemployed.Level)
laborData$Civilian.Labor.Force <- gsub(",","",laborData$Civilian.Labor.Force)

laborData$State <- trimws(laborData$State,which = c("both"))
laborData$Period <- trimws(laborData$Period,which = c("both"))
laborData$Unemployed.Rate <- as.numeric(laborData$Unemployed.Rate)
laborData$Unemployed.Level <- as.numeric(laborData$Unemployed.Level)
laborData$Civilian.Labor.Force <- as.numeric(laborData$Civilian.Labor.Force)

laborData[is.na(laborData$Civilian.Labor.Force),]

NorthEastData <- subset(laborData,State=='PA' | State=='NY' | State=='NJ' | State=='CT' | State=='RI' | State=='MA' | State=='VT' | State=='NH' | State=='ME')

NorthEastDataJun2019 <- subset(NorthEastData, Period=='Jun-19')

NorthEastDataJun2020 <- subset(NorthEastData, Period=='Jun-20')

aggregatedUnemployment2019 = data.frame(group_by(NorthEastDataJun2019, State) %>% summarise(UnemploymentRate = sum(Unemployed.Level, na.rm=TRUE)/sum(Civilian.Labor.Force, na.rm=TRUE) * 100,.groups = 'drop'))
aggregatedUnemployment2020 = data.frame(group_by(NorthEastDataJun2020, State) %>% summarise(UnemploymentRate = sum(Unemployed.Level, na.rm=TRUE)/sum(Civilian.Labor.Force, na.rm=TRUE) * 100,.groups = 'drop'))


# June 2019
ggplot(aggregatedUnemployment2019, aes(x=State,y=UnemploymentRate, fill=UnemploymentRate)) + geom_bar(stat='identity')+ geom_text(aes(label=round(UnemploymentRate,2)),vjust=-.3,size=3.5)+labs(x='State', y= 'Unemployment Rate', title='Northeastern Unemployment June 2019')

#June 2020
ggplot(aggregatedUnemployment2020, aes(x=State,y=UnemploymentRate, fill=UnemploymentRate)) + geom_bar(stat='identity') + geom_text(aes(label=round(UnemploymentRate,2)),vjust=-.3,size=3.5)  + labs(x='State', y= 'Unemployment Rate', title='Northeastern Unemployment June 2020')

