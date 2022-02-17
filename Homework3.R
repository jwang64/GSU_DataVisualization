library(ggplot2)
library(dplyr)
library(scales)

covidData <- read.csv("C:/Users/54208/Google Drive/School/Graduate School/Masters/Fall_2020/MSA8020/covid19_data.csv")

covidData$Total.Deaths <- gsub(",","",covidData$Total.Deaths)
covidData$Total.Recovered <- gsub(",","",covidData$Total.Recovered)


covidData$Total.Deaths <- as.numeric(covidData$Total.Deaths)
covidData$Total.Recovered <- as.numeric(covidData$Total.Recovered)

covidData_removedTCNulls <- covidData[!is.na(covidData$Total.Recovered),]
covidData_removedTDNulls <- covidData_removedTCNulls[!is.na(covidData_removedTCNulls$Total.Deaths),]

totalDeaths <- sum(covidData$Total.Deaths, na.rm=TRUE)
totalRecovered <- sum(covidData$Total.Recovered, na.rm=TRUE)

covidPie <- data.frame(
  group = c('TotalDeaths','TotalRecovered'),
  value = c(totalDeaths,totalRecovered)
)

head(covidPie)

pct <- round(covidPie$value/(sum(covidPie$value)*100))

ggplot(covidPie, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width = 10, color="black") +
  geom_text(label = paste0(round(covidPie$value/(sum(covidPie$value))*100,digits = 3),"%"),size = 3, position = position_stack(vjust=0.5)) +
  labs(x=NULL, y=NULL, fill=NULL, title="Total Deaths vs Total Recovered") +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(axis.line = element_blank(),
        plot.title = element_text(hjust=0.5)) +
  scale_fill_brewer(palette = "Set1") #color palette to match slides
