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

test <- cleanedLWA %>%
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
  summarise(rightSpotAverage = mean(LAnteriorSpotM3), leftSpotAverage = mean(RAnteriorSpotM3))

anteriorSpotDataFemale <- cabbageButterfly%>%
  select(SexUpdated, LAnteriorSpotM3, RAnteriorSpotM3)%>%
  dplyr::filter(SexUpdated == 'female')%>%
  na.omit(anteriorSpotDataFemale)%>%
  summarise(rightSpotAverage = mean(LAnteriorSpotM3), leftSpotAverage = mean(RAnteriorSpotM3))



  
  


  
            

