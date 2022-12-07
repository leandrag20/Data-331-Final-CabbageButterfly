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
  mutate(SexUpdated = recode(SexUpdated, M = "male", F = "female"))%>%
  lubridate::parse_date_time(dwc.eventDate, c('Ymd'))


ladybugMap <- ladybugData %>%
  select(Species, coordinates)%>%
  arrange(Species, desc(Species))


