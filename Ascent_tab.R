library(tidyverse)
library(lubridate)
library(scales)
library(timetk)
library(knitr)

source("source_data.R")

# Calculations for the Ascent tab
##Create the base dataset
Ascent_base <- Weekly_total %>%
        select(Date,
               First, 
               FirstServe24) %>%
        rename(Ascent = First,
               Ascent_online = FirstServe24) %>%
        group_by(Date) %>%
        mutate(total = sum(c(Ascent, Ascent_online), na.rm = TRUE)) %>%
        mutate(total = replace(total, total == 0, NA)) %>%
        arrange(Date)

##Weekly mean per month
Ascent_monthly_average <- Ascent_base %>%
        group_by(Date = floor_date(Date, "month")) %>%
        summarize(in.person = mean(Ascent, na.rm = TRUE),
                  online = mean(Ascent_online, na.rm = TRUE),
                  total = mean(total, na.rm = TRUE)) %>%
        mutate(Month = month(Date, label = TRUE),
               Year = year(Date)) %>%
        mutate(Era = ifelse(Date <= "2020-03-01", "Prepandemic", ifelse(Date > "2020-03-01" & Date < "2021-04-01", "Pandemic", "Postpandemic")))

##Weekly mean per year
Ascent_yearly_average <- Ascent_base %>%
        group_by(year = floor_date(Date, "year")) %>%
        summarize(in.person = mean(Ascent, na.rm = TRUE),
                  online = mean(Ascent_online, na.rm = TRUE),
                  total = mean(total, na.rm = TRUE))

##Total and percent change year-over-year
Ascent_combined_last_month_change <-
        tail(Ascent_monthly_average$total, 14)[13] -
        tail(Ascent_monthly_average$total, 14)[1]

Ascent_combined_last_month_percent_change <-
        percent(((tail(Ascent_monthly_average$total, 14)[13] -
                          tail(Ascent_monthly_average$total, 14)[1])
                 /
                         tail(Ascent_monthly_average$total, 14)[1])
        )

Ascent_in_person_last_month_change <-
        tail(Ascent_monthly_average$in.person, 14)[13] -
        tail(Ascent_monthly_average$in.person, 14)[1]

Ascent_in_person_last_month_percent_change <-
        percent(((tail(Ascent_monthly_average$in.person, 14)[13] -
                          tail(Ascent_monthly_average$in.person, 14)[1])
                 /
                         tail(Ascent_monthly_average$in.person, 14)[1])
        )

Ascent_online_last_month_change <-
        tail(Ascent_monthly_average$online, 14)[13] -
        tail(Ascent_monthly_average$online, 14)[1]

Ascent_online_last_month_percent_change <-
        percent(((tail(Ascent_monthly_average$online, 14)[13] -
                          tail(Ascent_monthly_average$online, 14)[1])
                 /
                         tail(Ascent_monthly_average$online, 14)[1])
        )

Ascent_combined_current_month_change <-
        tail(Ascent_monthly_average$total, 13)[13] -
        tail(Ascent_monthly_average$total, 13)[1]

Ascent_combined_current_month_percent_change <-
        percent(((tail(Ascent_monthly_average$total, 13)[13] -
                          tail(Ascent_monthly_average$total, 13)[1])
                 /
                         tail(Ascent_monthly_average$total, 13)[1])
        )

Ascent_in_person_current_month_change <- 
        tail(Ascent_monthly_average$in.person, 13)[13] -
        tail(Ascent_monthly_average$in.person, 13)[1]

Ascent_in_person_current_month_percent_change <-
        percent(((tail(Ascent_monthly_average$in.person, 13)[13] -
                          tail(Ascent_monthly_average$in.person, 13)[1])
                 /
                         tail(Ascent_monthly_average$in.person, 13)[1])
        )

Ascent_online_current_month_change <- tail(Ascent_monthly_average$online, 13)[13] -
        tail(Ascent_monthly_average$online, 13)[1]

Ascent_online_current_month_percent_change <-
        percent(((tail(Ascent_monthly_average$online, 13)[13] -
                          tail(Ascent_monthly_average$online, 13)[1])
                 /
                         tail(Ascent_monthly_average$online, 13)[1])
        )

###Pull out monthly data
Ascent_previous_month <- month.name[as.numeric(format(as.Date(tail(Ascent_monthly_average$Date, 13)[12]), "%m"))]
Ascent_current_month <- month.name[as.numeric(format(as.Date(tail(Ascent_monthly_average$Date, 13)[13]), "%m"))]

##Tables
Ascent_change_table <- matrix(c(Ascent_previous_month,
                                format(round(Ascent_combined_last_month_change, 1),
                                       nsmall = 1),
                                format(round(Ascent_in_person_last_month_change, 1),
                                       nsmall = 1),
                                format(round(Ascent_online_last_month_change, 1),
                                       nsmall = 1),
                                Ascent_previous_month,
                                Ascent_combined_last_month_percent_change,
                                Ascent_in_person_last_month_percent_change,
                                Ascent_online_last_month_percent_change,
                                Ascent_current_month,
                                format(round(Ascent_combined_current_month_change, 1),
                                       nsmall = 1),
                                format(round(Ascent_in_person_current_month_change, 1),
                                       nsmall = 1),
                                format(round(Ascent_online_current_month_change, 1),
                                       nsmall = 1),
                                Ascent_current_month,
                                Ascent_combined_current_month_percent_change,
                                Ascent_in_person_current_month_percent_change,
                                Ascent_online_current_month_percent_change
),
ncol = 4,
byrow = FALSE
)

colnames(Ascent_change_table) <- c("Total change",
                                   "Percent change",
                                   "Total change",
                                   "Percent change")

rownames(Ascent_change_table) <- c("Month",
                                   "Combined",
                                   "In person",
                                   "Online")

Ascent_overview_table <- matrix(c(tail(Ascent_base$total, 1),
                                  tail(Ascent_base$Ascent, 1),
                                  tail(Ascent_base$Ascent_online, 1),
                                  tail(Ascent_monthly_average$total, 1),
                                  tail(Ascent_monthly_average$in.person, 1),
                                  tail(Ascent_monthly_average$online, 1),
                                  tail(Ascent_monthly_average$total, 2)[1],
                                  tail(Ascent_monthly_average$in.person, 2)[1],
                                  tail(Ascent_monthly_average$online, 2)[1],
                                  tail(Ascent_yearly_average$total, 1),
                                  tail(Ascent_yearly_average$in.person, 1),
                                  tail(Ascent_yearly_average$online, 1),
                                  tail(Ascent_yearly_average$total, 2)[1],
                                  tail(Ascent_yearly_average$in.person, 2)[1],
                                  tail(Ascent_yearly_average$online, 2)[1]),
                                ncol = 5,
                                byrow = FALSE)

colnames(Ascent_overview_table) <- c("Last week",
                                     "Current Month-to-date",
                                     "Previous Month Average",
                                     "Current Year-to-date",
                                     "Previous Year Average")

rownames(Ascent_overview_table) <- c("Total",
                                     "In person",
                                     "Online")

Ascent_in_person <- Ascent_monthly_average %>%
        select(Date, in.person) %>%
        rename(attendance = in.person)

Ascent_online <- Ascent_monthly_average %>%
        select(Date, online) %>%
        rename(attendance = online) %>%
        filter(Date >= "2017-12-01")

Ascent_total <- Ascent_monthly_average %>%
        select(Date, total) %>%
        rename(attendance = total)
