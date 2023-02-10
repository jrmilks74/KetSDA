#Load data

library(tidyverse)
library(lubridate)
library(scales)
library(timetk)
library(knitr)
library(googlesheets4)

options(
        gargle_oauth_email = TRUE,
        gargle_oauth_cache = "KetSDA/.secrets/"
)
gs4_auth(email = "office@ketsda.org")

Weekly_total <- read_sheet("https://docs.google.com/spreadsheets/d/1BsQR4TyAMkV2H09jzuj5yrOIotnOBTzLoSZ0r5PsBpk/edit#gid=0",
                           col_names = TRUE,
                           col_types = "Ddddddc")

# Calculations for the first tab
##Create weekly mean per month in-person attendance
base_data_set <- Weekly_total %>%
        select(Date, 
               total.attendance, 
               First, 
               Second, 
               FirstServe24, 
               SecondServe24) %>%
        rename(In_person = total.attendance, 
               Ascent = First, 
               Sanctuary = Second, 
               Ascent_online = FirstServe24, 
               Sanctuary_online = SecondServe24) %>%
        group_by(Date) %>%
        mutate(Online = sum(c(Ascent_online, Sanctuary_online), na.rm = TRUE)) %>%
        mutate(Online = replace(Online, Online == 0, NA)) %>%
        mutate(Total = sum(c(In_person, Online), na.rm = TRUE)) %>%
        mutate(Total = replace(Total, Total == 0, NA)) %>%
        group_by(Date = floor_date(Date, "month")) %>%
        summarise(Ascent = mean(Ascent, na.rm = TRUE),
                  Sanctuary = mean(Sanctuary, na.rm = TRUE),
                  Ascent_online = mean(Ascent_online, na.rm = TRUE),
                  Sanctuary_online = mean(Sanctuary_online, na.rm = TRUE),
                  In_person = mean(In_person, na.rm = TRUE),
                  Online = mean(Online, na.rm = TRUE),
                  Total = mean(Total, na.rm = TRUE)) %>%
        mutate(Month = month(Date, label = TRUE),
               Year = year(Date)) %>%
        mutate(Era = ifelse(Date <= "2020-03-01", "Prepandemic", ifelse(Date > "2020-03-01" & Date < "2021-04-01", "Pandemic", "Postpandemic")))

###Read in older attendance data
Monthly_average <- read_sheet("https://docs.google.com/spreadsheets/d/1DUtoxOBcbZaLovhMUrzyTBtkb8ZqvIveMMMu-eAQMns/edit#gid=0",
                              col_names = TRUE)

Monthly_average$Date <- sprintf("%d-%02d", Monthly_average$Year, Monthly_average$Month)
Monthly_average$Date <- as.Date(paste(Monthly_average$Date, "-01", sep = ""))
Monthly_average <- Monthly_average %>%
        select(Date, Attendance) %>%
        rename(attendance = Attendance) %>%
        filter(Date < "2010-01-01")