library(tidyverse)
library(scales)
library(timetk)

source("source_data.R")

#Calculations for the main tab

###Combine the older and the recent attendance data into one
in_person_alone <- base_data_set %>%
        select(Date, In_person) %>%
        rename(attendance = In_person)

in_person_full_data <- rbind(Monthly_average, in_person_alone)

##Create weekly mean per month online viewership
online <- base_data_set %>%
        select(Date, Online) %>%
        rename(attendance = Online) %>%
        filter(Date >= "2017-12-01")

##Create weekly mean per month total attendance
combined_part <- base_data_set %>%
        select(Date, Total) %>%
        rename(attendance = Total)

combined <- rbind(Monthly_average, combined_part)

##Summarize data by year for combined, online-only, and in-person attendance
Combined_yearly <- Weekly_total %>%
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
        group_by(Date = floor_date(Date, "year")) %>%
        summarise(Ascent = mean(Ascent, na.rm = TRUE),
                  Sanctuary = mean(Sanctuary, na.rm = TRUE),
                  Ascent_online = mean(Ascent_online, na.rm = TRUE),
                  Sanctuary_online = mean(Sanctuary_online, na.rm = TRUE),
                  In_person = mean(In_person, na.rm = TRUE),
                  Online = mean(Online, na.rm = TRUE),
                  Total = mean(Total, na.rm = TRUE))

yearly_in_person <- Combined_yearly %>%
        select(Date, In_person) %>%
        rename(attendance = In_person)

yearly_online <- Combined_yearly %>%
        select(Date, Online) %>%
        rename(attendance = Online) %>%
        filter(Date >= "2017-01-01")

yearly_combined <- Combined_yearly %>%
        select(Date, Total) %>%
        rename(attendance = Total)

##Year-over-year actual and percent change
combined_last_month_change <-
        tail(combined$attendance, 14)[13] -
        tail(combined$attendance, 14)[1]

combined_last_month_percent_change <-
        percent(((tail(combined$attendance, 14)[13] -
                          tail(combined$attendance, 14)[1])
                 /
                         tail(combined$attendance, 14)[1])
        )

in_person_last_month_change <-
        tail(in_person_full_data$attendance, 14)[13] -
        tail(in_person_full_data$attendance, 14)[1]

in_person_last_month_percent_change <-
        percent(((tail(in_person_full_data$attendance, 14)[13] -
                          tail(in_person_full_data$attendance, 14)[1])
                 /
                         tail(in_person_full_data$attendance, 14)[1])
        )

online_last_month_change <-
        tail(online$attendance, 14)[13] -
        tail(online$attendance, 14)[1]

online_last_month_percent_change <-
        percent(((tail(online$attendance, 14)[13] -
                          tail(online$attendance, 14)[1])
                 /
                         tail(online$attendance, 14)[1])
        )

combined_current_month_change <-
        tail(combined$attendance, 13)[13] -
        tail(combined$attendance, 13)[1]

combined_current_month_percent_change <-
        percent(((tail(combined$attendance, 13)[13] -
                          tail(combined$attendance, 13)[1])
                 /
                         tail(combined$attendance, 13)[1])
        )

in_person_current_month_change <-
        tail(in_person_full_data$attendance, 13)[13] -
        tail(in_person_full_data$attendance, 13)[1]

in_person_current_month_percent_change <-
        percent(((tail(in_person_full_data$attendance, 13)[13] -
                          tail(in_person_full_data$attendance, 13)[1])
                 /
                         tail(in_person_full_data$attendance, 13)[1])
        )

online_current_month_change <-
        tail(online$attendance, 13)[13] -
        tail(online$attendance, 13)[1]

online_current_month_percent_change <-
        percent(((tail(online$attendance, 13)[13] -
                          tail(online$attendance, 13)[1])
                 /
                         tail(online$attendance, 13)[1])
        )

#Pull weekly data together for table
weekly_base_data <- Weekly_total %>%
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
        mutate(Total = replace(Total, Total == 0, NA))

Weekly_in_person <- weekly_base_data %>%
        select(Date, In_person) %>%
        rename(attendance = In_person)

Weekly_online <- weekly_base_data %>%
        select(Date, Online) %>%
        rename(attendance = Online) %>%
        filter(Date >= "2017-12-01")

Weekly_combined <- weekly_base_data %>%
        select(Date, Total) %>%
        rename(attendance = Total)

##Pull monthly data out for tables
previous_month <- month.name[as.numeric(format(as.Date(tail(combined$Date, 13)[12]), "%m"))]
current_month <- month.name[as.numeric(format(as.Date(tail(combined$Date, 13)[13]), "%m"))]

##Create tables
latest <- matrix(c(tail(Weekly_combined$attendance, 1),
                   tail(Weekly_in_person$attendance, 1),
                   tail(Weekly_online$attendance, 1),
                   tail(combined$attendance, 1),
                   tail(in_person_full_data$attendance, 1),
                   tail(online$attendance, 1),
                   tail(combined$attendance, 2)[1],
                   tail(in_person_full_data$attendance, 2)[1],
                   tail(online$attendance, 2)[1],
                   tail(yearly_combined$attendance, 1),
                   tail(yearly_in_person$attendance, 1),
                   tail(yearly_online$attendance, 1),
                   tail(yearly_combined$attendance, 2)[1],
                   tail(yearly_in_person$attendance, 2)[1],
                   tail(yearly_online$attendance, 2)[1]
),
ncol = 5,
byrow = FALSE
)

colnames(latest) <- c("Last week",
                      "Current Month-to-date",
                      "Previous Month Average",
                      "Current Year-to-date",
                      "Previous Year Average")

rownames(latest) <- c("Total",
                      "In person",
                      "Online")

total_change_table <- matrix(c(previous_month,
                               format(round(combined_last_month_change, 1),
                                      nsmall = 1),
                               format(round(in_person_last_month_change, 1),
                                      nsmall = 1),
                               format(round(online_last_month_change, 1),
                                      nsmall = 1),
                               previous_month,
                               combined_last_month_percent_change,
                               in_person_last_month_percent_change,
                               online_last_month_percent_change,
                               current_month,
                               format(round(combined_current_month_change, 1),
                                      nsmall = 1),
                               format(round(in_person_current_month_change, 1),
                                      nsmall = 1),
                               format(round(online_current_month_change, 1),
                                      nsmall = 1),
                               current_month,
                               combined_current_month_percent_change,
                               in_person_current_month_percent_change,
                               online_current_month_percent_change
),
ncol = 4,
byrow = FALSE
)

colnames(total_change_table) <- c("Total change",
                                  "Percent change",
                                  "Total change",
                                  "Percent change")

rownames(total_change_table) <- c("Month",
                                  "Combined",
                                  "In person",
                                  "Online")
