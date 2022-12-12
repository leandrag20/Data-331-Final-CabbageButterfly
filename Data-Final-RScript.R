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

anteriorSpotDataMale <- cabbageButterfly%>%
  select(SexUpdated, LAnteriorSpotM3, RAnteriorSpotM3)%>%
  dplyr::filter(SexUpdated == 'male')%>%
  na.omit(anteriorSpotDataMale)%>%
  summarise(rightSpotAverageMale = mean(LAnteriorSpotM3), leftSpotAverageMale = mean(RAnteriorSpotM3))

anteriorSpotDataFemale <- cabbageButterfly%>%
  select(SexUpdated, LAnteriorSpotM3, RAnteriorSpotM3)%>%
  dplyr::filter(SexUpdated == 'female')%>%
  na.omit(anteriorSpotDataFemale)%>%
  summarise(rightSpotAverageFemale = mean(LAnteriorSpotM3), leftSpotAverageFemale = mean(RAnteriorSpotM3))

cabbageButterfly$decade <- substring(cabbageButterfly$dwc.year, first = 3, last = 3)
cabbageButterfly$decade <- paste0(cabbageButterfly$decade, '0')

na.omit(cabbageButterfly$RBlackPatchApex)
na.omit(cabbageButterfly$LBlackPatchApex)
na.omit(cabbageButterfly$RWingLength)
na.omit(cabbageButterfly$RWingWidth)
na.omit(cabbageButterfly$LWingLength)
na.omit(cabbageButterfly$LWingWidth)

cabbageButterfly$RBlackPatchApex <- as.numeric(cabbageButterfly$RBlackPatchApex)
cabbageButterfly$LBlackPatchApex <- as.numeric(cabbageButterfly$LBlackPatchApex)
cabbageButterfly$RWingLength <- as.numeric(cabbageButterfly$RWingLength)
cabbageButterfly$RWingWidth <- as.numeric(cabbageButterfly$RWingWidth)
cabbageButterfly$LWingLength <- as.numeric(cabbageButterfly$LWingLength)
cabbageButterfly$LWingWidth <- as.numeric(cabbageButterfly$LWingWidth)
  
decadeData <- cabbageButterfly%>%
  dplyr::group_by(decade)%>%
  dplyr::summarise(averageRightArea = mean(RBlackPatchApex), 
            averageLeftArea = mean(LBlackPatchApex),
            averageRightWidth = mean(RWingWidth), 
            averageLeftWidth = mean(LWingWidth),
            averageRightLength = mean(RWingLength), 
            averageLeftLength = mean(LWingLength))


            

