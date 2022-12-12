library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)

rm(list = ls())

setwd("~/git_data/Data-331-Final")

cleanedLWA <- read_excel("Cleaned Data LWA .xlsx", .name_repair = "universal")
cabbageButterfly <- read_excel("CompletePierisData_2022-03-09 (1).xlsx", .name_repair = "universal")

cabbageButterfly <- cabbageButterfly %>%
  mutate(SexUpdated = recode(SexUpdated, M = "male", F = "female"))

cabbageButterflySexData <- cleanedLWA %>%
  group_by(sex) %>%
  summarise(averageRightArea = mean(RW.apex.A),
            averageLeftArea = mean(LW.apex.A),
            averageRightWidth = mean(RW.width), 
            averageLeftWidth = mean(LW.width),
            averageRightLength = mean(RW.length), 
            averageLeftLength = mean(LW.length))

cabbageButterfly$LAnteriorSpotM3 <- as.numeric(cabbageButterfly$LAnteriorSpotM3)
cabbageButterfly$RAnteriorSpotM3 <- as.numeric(cabbageButterfly$RAnteriorSpotM3)
cabbageButterfly$RBlackPatchApex <- as.numeric(cabbageButterfly$RBlackPatchApex)
cabbageButterfly$LBlackPatchApex <- as.numeric(cabbageButterfly$LBlackPatchApex)
cabbageButterfly$RWingLength <- as.numeric(cabbageButterfly$RWingLength)
cabbageButterfly$RWingWidth <- as.numeric(cabbageButterfly$RWingWidth)
cabbageButterfly$LWingLength <- as.numeric(cabbageButterfly$LWingLength)
cabbageButterfly$LWingWidth <- as.numeric(cabbageButterfly$LWingWidth)
cabbageButterfly$dwc.month <- as.numeric(cabbageButterfly$dwc.month)

anteriorSpotDataMale <- cabbageButterfly%>%
  select(SexUpdated, LAnteriorSpotM3, RAnteriorSpotM3)%>%
  dplyr::filter(SexUpdated == 'male')%>%
  na.omit(anteriorSpotDataMale)%>%
  summarise(rightSpotAverageMale = mean(RAnteriorSpotM3), leftSpotAverageMale = mean(LAnteriorSpotM3))

anteriorSpotDataFemale <- cabbageButterfly%>%
  select(SexUpdated, LAnteriorSpotM3, RAnteriorSpotM3)%>%
  dplyr::filter(SexUpdated == 'female')%>%
  na.omit(anteriorSpotDataFemale)%>%
  summarise(rightSpotAverageFemale = mean(LAnteriorSpotM3), leftSpotAverageFemale = mean(RAnteriorSpotM3))

cabbageButterfly$dwc.year <- as.numeric(cabbageButterfly$dwc.year)
na.omit(cabbageButterfly$dwc.year)
cabbageButterfly$decade <- substring(cabbageButterfly$dwc.year, first = 3, last = 3)
cabbageButterfly$decade <- paste0(cabbageButterfly$decade, '0')

na.omit(cabbageButterfly$RBlackPatchApex)
na.omit(cabbageButterfly$LBlackPatchApex)
na.omit(cabbageButterfly$RWingLength)
na.omit(cabbageButterfly$RWingWidth)
na.omit(cabbageButterfly$LWingLength)
na.omit(cabbageButterfly$LWingWidth)
na.omit(cabbageButterfly$RAnteriorSpotM3)
na.omit(cabbageButterfly$LAnteriorSpotM3)
na.omit(cabbageButterfly$SexUpdated)
na.omit(cabbageButterfly$decade)
na.omit(cabbageButterfly$dwc.country)
na.omit(cabbageButterfly$dwc.month)
  
decadeData <- cabbageButterfly%>%
  dplyr::group_by(decade)%>%
  filter(decade == "00" || decade == "90")%>%
  dplyr::summarise(averageRightArea = mean(RBlackPatchApex), 
            averageLeftArea = mean(LBlackPatchApex),
            averageRightWidth = mean(RWingWidth), 
            averageLeftWidth = mean(LWingWidth),
            averageRightLength = mean(RWingLength), 
            averageLeftLength = mean(LWingLength),
            rightSpotAverage = mean(RAnteriorSpotM3),
            leftSpotAverageMale = mean(LAnteriorSpotM3))

decadeData2 <- data.frame(t(decadeData[-1]))
colnames(decadeData2)[1] = "00"
colnames(decadeData2)[2] = "90"

barplot(decadeData2$`00`, col = "green", main = '2000 Data', xlab = 'Body Part', ylab = "Millimeters")
barplot(decadeData2$`90`, col = "green", main = '1990 Data', xlab = 'Body Part', ylab = "Millimeters")

cabbageButterflySexData2 <- data.frame(t(cabbageButterflySexData[-1]))
colnames(cabbageButterflySexData2)[1] = "female"
colnames(cabbageButterflySexData2)[2] = "male"
cabbageButterflySexData2 <- cabbageButterflySexData2[,-c(3)]

barplot(cabbageButterflySexData2$female, col = "pink", main = 'Female Data', xlab = 'Body Part', ylab = "Millimeters")
barplot(cabbageButterflySexData2$male, col = "blue", main = 'Male Data', xlab = 'Body Part', ylab = "Millimeters")

territoryData <- cabbageButterfly%>%
  dplyr::group_by(dwc.country)%>%
  dplyr::summarise(averageRightArea = mean(RBlackPatchApex), 
                   averageLeftArea = mean(LBlackPatchApex),
                   averageRightWidth = mean(RWingWidth), 
                   averageLeftWidth = mean(LWingWidth),
                   averageRightLength = mean(RWingLength), 
                   averageLeftLength = mean(LWingLength),
                   rightSpotAverage = mean(RAnteriorSpotM3),
                   leftSpotAverageMale = mean(LAnteriorSpotM3))

territoryData2 <- data.frame(t(territoryData[-1]))
territoryData2 <- territoryData2[,-c(2,3, 5, 6, 7, 8)]
colnames(territoryData2)[1] = "Canada"
colnames(territoryData2)[2] = "USA"

barplot(territoryData2$Canada, col = "red", main = 'Canada Data', xlab = 'Body Part', ylab = "Millimeters")
barplot(territoryData2$USA, col = "blue", main = 'USA Data', xlab = 'Body Part', ylab = "Millimeters")

yearlyData <- cabbageButterfly%>%
  dplyr::group_by(YearUpdated)%>%
  dplyr::summarise(averageRightArea = mean(RBlackPatchApex), 
                   averageLeftArea = mean(LBlackPatchApex),
                   averageRightWidth = mean(RWingWidth), 
                   averageLeftWidth = mean(LWingWidth),
                   averageRightLength = mean(RWingLength), 
                   averageLeftLength = mean(LWingLength),
                   rightSpotAverage = mean(RAnteriorSpotM3),
                   leftSpotAverageMale = mean(LAnteriorSpotM3))

yearlyData2 <- data.frame(t(yearlyData[-1]))
yearlyData2 <- yearlyData2[,-c(1, 3:122, 124)]
colnames(yearlyData2)[1] = "1821"
colnames(yearlyData2)[2] = "2017"

barplot(yearlyData2$"1821", col = "blue", main = '1821 Data', xlab = 'Body Part', ylab = "Millimeters")
barplot(yearlyData2$"2017", col = "blue", main = '2017 Data', xlab = 'Body Part', ylab = "Millimeters")


decadeDataMale <- cabbageButterfly%>%
  dplyr::group_by(decade)%>%
  filter(decade == "00" || decade == "90" || decade == "10" && SexUpdated == 'male')%>%
  dplyr::summarise(averageRightWidth = mean(RWingWidth), 
                   averageLeftWidth = mean(LWingWidth),
                   averageRightLength = mean(RWingLength), 
                   averageLeftLength = mean(LWingLength))

decadeDataMale2 <- data.frame(t(decadeDataMale[-1]))
colnames(decadeDataMale2)[1] = "00"
colnames(decadeDataMale2)[2] = "90"

barplot(decadeDataMale2$`00`, col = "green", main = '00 Decade Data Male', xlab = 'Body Part', ylab = "Millimeters")
barplot(decadeDataMale2$`90`, col = "green", main = '90 Decade Data Male', xlab = 'Body Part', ylab = "Millimeters")

decadeDataFemale <- cabbageButterfly%>%
  dplyr::group_by(decade)%>%
  filter(decade == "00" || decade == "90" || decade == "10" && SexUpdated == 'female')%>%
  dplyr::summarise(averageRightWidth = mean(RWingWidth), 
                   averageLeftWidth = mean(LWingWidth),
                   averageRightLength = mean(RWingLength), 
                   averageLeftLength = mean(LWingLength))

decadeDataFemale2 <- data.frame(t(decadeDataFemale[-1]))
decadeDataFemale2 <- decadeDataFemale2[,-c(2)]
colnames(decadeDataFemale2)[1] = "00"
colnames(decadeDataFemale2)[2] = "90"

barplot(decadeDataFemale2$`00`, col = "green", main = '00 Decade Data Female', xlab = 'Body Part', ylab = "Millimeters")
barplot(decadeDataFemale2$`90`, col = "green", main = '90 Decade Data Female', xlab = 'Body Part', ylab = "Millimeters")


apexVsSpotArea <- cabbageButterfly%>%
  select(LAnteriorSpotM3, RAnteriorSpotM3, RBlackPatchApex, LBlackPatchApex)%>%
  na.omit(apexVsSpotArea)

apexVsWingLength <- cabbageButterfly%>%
  select(RBlackPatchApex, LBlackPatchApex, RWingLength, LWingLength)%>%
  na.omit(apexVsWingLength)

spotVsWingLength <- cabbageButterfly%>%
  select(LAnteriorSpotM3, RAnteriorSpotM3, RWingLength, LWingLength)%>%
  na.omit(spotVsWingLength)
  

sexData <- cabbageButterfly%>%
  dplyr::group_by(dwc.sex)%>%
  filter(dwc.sex == "Male" || dwc.sex == "Female")%>%
  dplyr::summarise(averageRightArea = mean(RBlackPatchApex), 
                   averageLeftArea = mean(LBlackPatchApex),
                   averageRightWidth = mean(RWingWidth), 
                   averageLeftWidth = mean(LWingWidth),
                   averageRightLength = mean(RWingLength), 
                   averageLeftLength = mean(LWingLength),
                   rightSpotAverage = mean(RAnteriorSpotM3),
                   leftSpotAverageMale = mean(LAnteriorSpotM3))

sexData2 <- data.frame(t(sexData[-1]))
colnames(sexData2)[1] = "Female"
colnames(sexData2)[2] = "Male"

barplot(sexData2$`Female`, col = "pink", main = 'Sex Data Female', xlab = 'Body Part', ylab = "Millimeters")
barplot(sexData2$`Male`, col = "blue", main = 'Sex Data Male', xlab = 'Body Part', ylab = "Millimeters")

            

