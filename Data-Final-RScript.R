library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)

rm(list = ls())

setwd("~/git_data/Data-331-Final")

ladybugData <- read_excel("Ladybug Data.xlsx", .name_repair = "universal")
cleanedLWA <- read_excel("Cleaned Data LWA .xlsx", .name_repair = "universal")
cabbageButterfly <- read_excel("CompletePierisData_2022-03-09 (1).xlsx", .name_repair = "universal")

cabbageButterfly <- cabbageButterfly %>%
  mutate(SexUpdated = recode(SexUpdated, M = "male", F = "female"))

ladybugMap <- ladybugData %>%
  select(Species, coordinates)%>%
  arrange(Species, desc(Species))

femaleWingLength <- cleanedLWA %>%
  select(sex, LW.length, RW.length)%>%
  dplyr::filter(sex == 'female')%>%
  summarise(averageRight = mean(RW.length), averageLeft = mean(LW.length))

maleWingLength <- cleanedLWA%>%
  select(sex, LW.length, RW.length)%>%
  dplyr::filter(sex == 'male')%>%
  summarise(averageRight = mean(RW.length), averageLeft = mean(LW.length))

femaleWingWidth <- cleanedLWA %>%
  select(sex, LW.width, RW.width)%>%
  dplyr::filter(sex == 'female')%>%
  summarise(averageRight = mean(RW.width), averageLeft = mean(LW.width))

maleWingWidth <- cleanedLWA%>%
  select(sex, LW.width, RW.width)%>%
  dplyr::filter(sex == 'male')%>%
  summarise(averageRight = mean(RW.width), averageLeft = mean(LW.width))

femaleApexArea <- cleanedLWA%>%
  select(sex, LW.apex.A, RW.apex.A)%>%
  dplyr::filter(sex == 'female')%>%
  summarise(averageRight = mean(RW.apex.A), averageLeft = mean(LW.apex.A))

maleApexArea <- cleanedLWA%>%
  select(sex, LW.apex.A, RW.apex.A)%>%
  dplyr::filter(sex == 'male')%>%
  summarise(averageRight = mean(RW.apex.A), averageLeft = mean(LW.apex.A))
            

