library(ggplot2)
library(dplyr)
install.packages("zoo")
library(zoo)
library(plotly)

laborData <- read.csv("C:/Users/54208/Google Drive/School/Graduate School/Masters/Fall_2020/MSA8020/laborData.csv")

laborData$Unemployed.Level <- gsub(",","",laborData$Unemployed.Level)
laborData$Civilian.Labor.Force <- gsub(",","",laborData$Civilian.Labor.Force)

laborData$State <- trimws(laborData$State,which = c("both"))
laborData$Period <- trimws(laborData$Period,which = c("both"))
laborData$Unemployed.Rate <- as.numeric(laborData$Unemployed.Rate)
laborData$Unemployed.Level <- as.numeric(laborData$Unemployed.Level)
laborData$Civilian.Labor.Force <- as.numeric(laborData$Civilian.Labor.Force)

laborData[is.na(laborData$Civilian.Labor.Force),]

laborData <- subset(laborData, (Period!='Jul-20(p)'))

seattleData <- subset(laborData, (State=='WA' & (Area.Title==' King County' | Area.Title==' Snohomish County' 
                                                | Area.Title== ' Pierce County')))

atlantaData <- subset(laborData, (State=='GA' & (Area.Title==' Fulton County' | Area.Title==' DeKalb County'
                                                 |  Area.Title ==' Gwinnett County' | Area.Title==' Cobb County'
                                                 | Area.Title ==' Clayton County' | Area.Title ==' Coweta County'
                                                 | Area.Title==' Douglas County' | Area.Title ==' Fayette County'
                                                 | Area.Title==' Henry County')))

aggregatedUnemploymentAtlanta = data.frame(group_by(atlantaData, Period) %>% summarise(UnemploymentRate = sum(Unemployed.Level, na.rm=TRUE)/sum(Civilian.Labor.Force, na.rm=TRUE) * 100,.groups = 'drop')) 
aggregatedUnemploymentAtlanta$Period <- as.Date(paste("01-", aggregatedUnemploymentAtlanta$Period,sep =""),format= "%d-%b-%y")
aggregatedUnemploymentAtlanta <- na.omit(aggregatedUnemploymentAtlanta[order(aggregatedUnemploymentAtlanta),])
aggregatedUnemploymentAtlanta['City'] <- 'ATL'

aggregatedUnemploymentSeattle = data.frame(group_by(seattleData, Period) %>% summarise(UnemploymentRate = sum(Unemployed.Level, na.rm=TRUE)/sum(Civilian.Labor.Force, na.rm=TRUE) * 100,.groups = 'drop')) 
aggregatedUnemploymentSeattle$Period <- as.Date(paste("01-", aggregatedUnemploymentSeattle$Period,sep =""),format= "%d-%b-%y")
aggregatedUnemploymentSeattle <- na.omit(aggregatedUnemploymentSeattle[order(aggregatedUnemploymentSeattle),])
aggregatedUnemploymentSeattle['City'] <- 'SEA'

aggregatedUnemployment <- rbind(aggregatedUnemploymentAtlanta,aggregatedUnemploymentSeattle)



###################
###PLOTLY GRAPHS###
###################

#Graph 1; interactive graph with Seattle and Atlanta on the same graph
aggregatedUnemployment %>% 
  group_by(City) %>% 
  plot_ly(x = aggregatedUnemployment$Period, y = aggregatedUnemployment$UnemploymentRate) %>%
  add_lines(color = aggregatedUnemployment$City) %>%
  layout(title = "Unemployment Rate from June 2019 - June 2020 in Atlanta and Seattle")

plot_ly(aggregatedUnemployment, x = ~Period, y = ~UnemploymentRate, color = I("red"), showlegend = TRUE) %>% 
  add_lines(color = ~City) %>%
  add_markers(color = ~UnemploymentRate) %>%
  layout(title = "Unemployment Rate from June 2019 - June 2020 in Atlanta and Seattle")


############ 
###D v R ###
############
republicanData <- subset(laborData, Political.Party == 'R')
democraticData <- subset(laborData, Political.Party =='D')

aggregatedUnemploymentRepublican = data.frame(group_by(republicanData, Period) %>% summarise(UnemploymentRate = sum(Unemployed.Level, na.rm=TRUE)/sum(Civilian.Labor.Force, na.rm=TRUE) * 100,.groups = 'drop')) 
aggregatedUnemploymentRepublican$Period <- as.Date(paste("01-", aggregatedUnemploymentRepublican$Period,sep =""),format= "%d-%b-%y")
aggregatedUnemploymentRepublican <- na.omit(aggregatedUnemploymentRepublican[order(aggregatedUnemploymentRepublican),])
aggregatedUnemploymentRepublican['PoliticalParty'] <- 'Republican'

aggregatedUnemploymentDemocrat = data.frame(group_by(democraticData, Period) %>% summarise(UnemploymentRate = sum(Unemployed.Level, na.rm=TRUE)/sum(Civilian.Labor.Force, na.rm=TRUE) * 100,.groups = 'drop')) 
aggregatedUnemploymentDemocrat$Period <- as.Date(paste("01-", aggregatedUnemploymentDemocrat$Period,sep =""),format= "%d-%b-%y")
aggregatedUnemploymentDemocrat <- na.omit(aggregatedUnemploymentDemocrat[order(aggregatedUnemploymentDemocrat),])
aggregatedUnemploymentDemocrat['PoliticalParty'] <- 'Democrat'

aggregatedUnemploymentParty <- rbind(aggregatedUnemploymentRepublican,aggregatedUnemploymentDemocrat)

aggregatedUnemploymentParty %>% 
  group_by(PoliticalParty) %>% 
  plot_ly(x = aggregatedUnemploymentParty$Period, y = aggregatedUnemploymentParty$UnemploymentRate) %>%
  add_lines(color = aggregatedUnemploymentParty$PoliticalParty) %>%
  layout(title = "Unemployment Rate from June 2019 - June 2020 by Political Party")
