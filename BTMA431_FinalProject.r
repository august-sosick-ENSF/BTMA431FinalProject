######################################################################################################################
#                                             BTMA 317 FINAL PROJECT                                               	#
######################################################################################################################

# Authors: Xinshui Zhang, Marie-Jeanne Dube, Lin Hung Wang, Jennifer Fernandez, August Sosick  
# Date: 2020-12-08

#LOAD PACKAGES
library(readxl)
library(dplyr)
library(gtrendsR)
library(tidyverse)
library(XML)
library(RCurl)
library(rvest)
library(ggplot2)
library(tinytex)
library(rworldmap)

#Working Directory to current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

######################################################################################################################
#                                               	   DATA COLLECTION                                           	#
######################################################################################################################

##################################################IMPORT EXCEL FILES##############################################

#Earthquakes
earthquakes <- read_csv("significant-earthquakes.csv")#load earthquakes dataset onto R
earthquakes <- earthquakes %>% filter(Year > 1986) %>% filter(Year < 2018)#Filter to have data between 1987-2017

#Volcanoes
volcanoes <- read_csv("significant-volcanic-eruptions.csv")#load volcanoes dataset onto R
volcanoes <- volcanoes %>% filter(Year > 1986) %>% filter(Year < 2018)#Filter to have data between 1987-2017

#Deaths b disaster type
deaths.type <- read_csv("number-of-deaths-from-natural-disasters.csv")#load seaths dataset onto R
names(deaths.type) <- c("Disaster Type", "Year", "Total Number of Deaths")

deathsT.df1 <- as.data.frame(deaths.type %>% filter(Year >= 1990) %>% filter(Year < 2000))#Filter to have data between 1990-1999
deathsT.df2 <- as.data.frame(deaths.type %>% filter(Year >= 2000) %>% filter(Year < 2009))#Filter to have data between 2000-2008
deathsT.df3 <- as.data.frame(deaths.type %>% filter(Year >= 2009) %>% filter(Year < 2018))#Filter to have data between 2008-2017

#Deaths by country from disaster
deaths.country <- read_csv("deaths-natural-disasters-ihme.csv")#load earthquakes dataset onto R
deaths.country <- deaths.country[, -c(2)]
names(deaths.country) <- c("Country", "Year", "Total Number of Deaths")

deathsC.df1 <- as.data.frame(deaths.country %>% filter(Year >= 1990) %>% filter(Year < 2000))#Filter to have data between 1990-1999
deathsC.df2 <- as.data.frame(deaths.country %>% filter(Year >= 2000) %>% filter(Year < 2009))#Filter to have data between 2000-2008
deathsC.df3 <- as.data.frame(deaths.country %>% filter(Year >= 2009) %>% filter(Year < 2018))#Filter to have data between 2008-2017

#################################################################################################################

###################################################WEB SCRAPING - TRENDS##############################################

periodCount <- 191
enviromentalDF <- data.frame("Date" = 1:periodCount, "Climate" = "", "Global_Warming" = "",
                             "Enviroment" = "", "Climate_Change" = "","Green_Party" = "",
                             "Greenhouse_Gas" = "", "Fossil_Fuels"="", "Emissions"="",
                             "Sea_Level_Rise"="", "Renewable_Energy"="", "Carbon_Dixoide"=""
)
print("Downloading from Google Trends...")
i <- 2 #skip Date column
while (i <= length(enviromentalDF) + 1) {
  results <- gtrends(keyword = gsub("_"," ",colnames(enviromentalDF)[i]), geo = "CA-AB", time = "2005-01-01 2020-11-26")
  #Add the data column only once
  if(i == 2){
    enviromentalDF$Date <- results %>% .$interest_over_time %>% .$date
  }
  enviromentalDF[i] <- results %>% .$interest_over_time %>% .$hits
  i <- i + 1
}

###################################################WEB SCRAPING - CLEANING########################################

#Data for US
############################
#read in csv files containing information
USHeat <- read_csv("heat-wave-index-usa.csv")
USPrecp <- read_csv("unusually-high-precipitation-usa.csv")
USAcresBurned <- read_csv("acres-burned-usa.csv")
USNumOfWildFires <- read_csv("wildfire-numbers-usa.csv")
USDrought <- read_csv("drought-severity-index-us.csv")
USEarthquake <- read_csv("significant-earthquakes.csv") %>% filter(Entity == "United States")
USVolcanos <- read_csv("significant-volcanic-eruptions.csv") %>% filter(Entity == "United States")

#Get US drought and earthquake information
joinedUS <- earthquakes %>% filter(Entity == "United States")

USDrought <- USDrought %>% filter(Entity == "United States")
USDrought$Entity <- NULL

#Merge all into US DisasterFrame
USDisasterFrame <- merge(USHeat, USPrecp,by = "Year")
USDisasterFrame <- merge(USDisasterFrame, USAcresBurned,by = "Year")
USDisasterFrame <- merge(USDisasterFrame, USNumOfWildFires,by = "Year")
USDisasterFrame <- merge(USDisasterFrame, USDrought,by = "Year")
USDisasterFrame <- merge(USDisasterFrame, USEarthquake,by = "Year")
USDisasterFrame <- merge(USDisasterFrame, USVolcanos,by = "Year")

#Clean
USDisasterFrame$Code <- NULL
USDisasterFrame$Entity<-NULL
USDisasterFrame$Code.x <- NULL
USDisasterFrame$Code.y <- NULL
USDisasterFrame$Entity.y <- NULL
USDisasterFrame$Entity.x <- NULL
USDisasterFrame$Code.x <- NULL
USDisasterFrame$Code.y <- NULL

#print out csv
write.csv(USDisasterFrame, "USDisasterFrame.csv")

##################################################################################################################

######################################################DATAFRAMES#################################################

joined_df <- merge(volcanoes, earthquakes, by = c("Year", "Entity"))
joined_df$Code.x <- NULL
joined_df$Code.y <- NULL

names(joined_df) <- c("Year", "Country", "Number of Significant Volcanic Eruptions", 
                      "Number of Significant Earthquakes")

#print out csv
write.csv(joined_df, "joined_df.csv")

#####################################################################################################################

######################################################################################################################
#                                               	     ANALYSIS                                                	#
######################################################################################################################

######################################################QUESTION 1###################################################

##Question: Is there a higher concentration of volcanic eruptions in certain locations?

#H0: There is no higher concentration of volcanic eruptions in certain locations.
#H1: There is higher concentration of volcanic eruptions in certain locations.

#Regression

data1 <- data.frame(volcano.N = as.numeric(volcanoes$`Number of significant volcanic eruptions (NGDC-WDS)`),
                    country.volcano = as.character(volcanoes$Entity))


data2 <- data.frame(earthquakes.N = as.numeric(earthquakes$`Significant earthquake events (NGDC-NASA)`),
                     country.earthquakes = as.character(earthquakes$Entity))
 
fit <- lm(volcano.N ~ country.volcano, data = data1)
summary(fit)
 
fit2 <- lm(earthquakes.N ~ country.earthquakes, data = data2)
summary(fit2)

###################################################QUESTION 1 - SUB QUESTION######################################

##Question: Does the concentration of volcanic eruptions have a correlation with the concentration of earthquakes?


#H0: There is no correlation between the concentration of volcanic eruptions and the concentration of earthquakes.
#H1: There is correlation between the concentration of volcanic eruptions and the concentration of earthquakes.

cor(joined_df$`Number of Significant Volcanic Eruptions`, joined_df$`Number of Significant Earthquakes`)

###################################################################################################################

########################################################QUESTION 2#################################################

##Question: Overtime, has there been a higher death rate associated with a specific natural disaster?

#H0: There has not been a higher death rate associated with a specific natural disaster overtime.
#H1: There has been a higher death rate associated with a specific natural disaster overtime.

#1990-1999
fit.Range1 <- lm(deathsT.df1$`Total Number of Deaths` ~ deathsT.df1$`Disaster Type`, data = deathsT.df1)
summary(fit.Range1)

#2000-2008
fit.Range2 <- lm(deathsT.df2$`Total Number of Deaths` ~ deathsT.df2$`Disaster Type`, data = deathsT.df2)
summary(fit.Range2)

#2009-2017
fit.Range3 <- lm(deathsT.df3$`Total Number of Deaths` ~ deathsT.df3$`Disaster Type`, data = deathsT.df3)
summary(fit.Range3)

#################################################QUESTION 2 - SUB QUESTION########################################

##Question: Overtime, was there a country which had a higher death rate due to natural disasters? 

#H0: There has not been a country with a higher death rate related to natural disasters.
#H1: There has been a country with a higher death rate related to natural disasters.

#1990-1999
fit2.Range1 <- lm(deathsC.df1$`Total Number of Deaths` ~ deathsC.df1$Country, data = deathsC.df1)
summary(fit2.Range1)

#2000-2008
fit2.Range2 <- lm(deathsC.df2$`Total Number of Deaths` ~ deathsC.df2$Country, data = deathsC.df2)
summary(fit2.Range2)

#2009-2017
fit2.Range3 <- lm(deathsC.df3$`Total Number of Deaths` ~ deathsC.df3$Country, data = deathsC.df3)
summary(fit2.Range3)


###Furthermore, is there a relationship between the frequency in the number of death due to 
#specific disasters and the country it occured in?


df1 <- merge(deathsC.df1$`Total Number of Deaths`, deathsT.df1$`Total Number of Deaths`)

df2 <- merge(deathsC.df2$`Total Number of Deaths`, deathsT.df2$`Total Number of Deaths`)

df3 <- merge(deathsC.df3$`Total Number of Deaths`, deathsT.df3$`Total Number of Deaths`)

df.joined <- data.frame(rbind(df1, df2, df3))

#Correlation to find relationship between the number of deaths based on a specific country and the disaster type
cor(df.joined$x, df.joined$y)

####################################################QUESTION 3######################################################

##Question: Do US citizens search more about environmentalism when they experience high rates of natural 
#disasters?

#H0: There is no relationship between the average rate of Americans searching environmentally conscious 
#terms and the rate of natural disasters.

#H1: There is relationship between the average rate of Americans searching environmentally conscious 
#terms and the rate of natural disasters.

#Regression Model

enviromentalDF$Date = as.Date(enviromentalDF$Date, format = "%Y-%m-%d")
enviromentalDF <- enviromentalDF %>%mutate( year = format(Date, "%Y"))

#Need to remove "<" from rows
enviromentalDF <- enviromentalDF %>% mutate(Green_Party = replace(Green_Party, Green_Party == "<1", 1))
enviromentalDF$Green_Party <- as.numeric(enviromentalDF$Green_Party)

enviromentalDF$Average_Terms <- (enviromentalDF$Climate + enviromentalDF$Global_Warming + enviromentalDF$Enviroment + enviromentalDF$Climate_Change +
                                   enviromentalDF$Green_Party + enviromentalDF$Greenhouse_Gas + enviromentalDF$Fossil_Fuels + enviromentalDF$Emissions + enviromentalDF$Sea_Level_Rise +
                                   enviromentalDF$Renewable_Energy) / 10

#aggregate based on years, not months
aggregatedEnviromentalDF <- aggregate(Average_Terms ~ year, enviromentalDF, mean)
aggregatedEnviromentalDF <- aggregatedEnviromentalDF %>% filter(year < 2016)

USReg <- USDisasterFrame %>% filter(Year >2004)

USReg$Year <- NULL
USReg$Date <- NULL


regressionModel <- lm(aggregatedEnviromentalDF$Average_Terms ~ . , data = USReg)
summary(regressionModel)

######
#VISUALIZING REGRESSION for average search terms, number of wildfires, and number of severe precipation events for the US
####

#prepare data through merging dataframe in order to preform regression (years 2004-2016)
enviromentRegressionDF <- enviromentalDF %>% filter(year > 2004) %>% filter(year<2016)
regressionViaYear <- data.frame("Year" = 1:11)
i<-1
while(i <= ncol(enviromentRegressionDF)-1){
  regressionViaYear[[i+1]] <- aggregate(enviromentRegressionDF[[i+1]] ~ year, enviromentRegressionDF, mean)[[2]]
  names(regressionViaYear)[i+1] = colnames(enviromentRegressionDF)[i+1]
  i <- i + 1
}

regressionViaYear$WildFire <- USReg$`Number of wildfires - comparable data (NIFC)`
regressionViaYear$Precipitation <- USReg$`Share of land area experienced unusually high annual precipitation - 9yr avg (NOAA)`

ggplot(regressionViaYear, aes(y = Average_Terms , x =Precipitation)) + geom_point()+ geom_smooth(method = "lm") + ggtitle("Severe Precipitation & Term Searches Regression")
ggplot(regressionViaYear, aes(y = Average_Terms, x =WildFire)) + geom_point() + geom_smooth(method = "lm")  + ggtitle("# of Wildfires & Term Searches Regression")

#cleans
USReg$Year <- NULL
USReg$Date <- NULL

#preform regression
regressionModel <- lm(aggregatedEnviromentalDF$Average_Terms ~ . , data = USReg)
summary(regressionModel)

###############################################QUESTION 3 - SUB QUESTION###########################################

##Question: Which natural disasters in the U.S. correlates best with a change in Americans searching about 
#environmentalism?

#H0: There is no relationship between Americans search environmentally conscious terms and a specific 
#natural disaster.

#H1: There is relationship between Americans search environmentally conscious terms and a specific 
#natural disaster.

question3DF <- data.frame("Type_of_Disaster" = "", "P-value" = "", "Mutliple_R_squared" = "", "F_Stat")

#run regression for each disaster in order to predict the ablility for it to determine the average search terms for Americans
#retrieve the p-value, multiple R-squared, and F-stats for each test to analyze their predictive power.
i<-1
while(i <= length(USReg)){
  regressionM <- summary(lm(aggregatedEnviromentalDF$Average_Terms ~ USReg[,i], data = USReg))
  question3DF[i,] <- c(colnames(USReg)[i], round(as.numeric(regressionM$coefficients[,4][2]), 3), round(regressionM$r.squared,3), round(as.numeric(regressionM$fstatistic[1]),3))
  i<-i+1
}

print("reponses for question 3 sub question avaliable in question3DF Dataframe.")

######################################################################################################################
#                                               	        END                                                 	#
######################################################################################################################

