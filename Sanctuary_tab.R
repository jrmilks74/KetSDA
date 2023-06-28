library(tidyverse)
library(scales)
library(timetk)
library(knitr)

source("source_data.R")

# Calculations for the Sanctuary tab
##Base dataset
Sanctuary_base <- Weekly_total %>%
        select(Date, Second, SecondServe24) %>%
        rename(Sanctuary = Second, Sanctuary_online = SecondServe24) %>%
        group_by(Date) %>%
        mutate(total = sum(c(Sanctuary, Sanctuary_online), na.rm = TRUE)) %>%
        mutate(total = replace(total, total == 0, NA))

##Weekly average per month
Sanctuary_monthly_average <- Sanctuary_base %>%
        group_by(Date = floor_date(Date, "month")) %>%
        summarize(in.person = mean(Sanctuary, na.rm = TRUE),
                  online = mean(Sanctuary_online, na.rm = TRUE),
                  total = mean(total, na.rm = TRUE))

##Weekly average per year
Sanctuary_yearly_average <- Sanctuary_base %>%
        group_by(year = floor_date(Date, "year")) %>%
        summarize(in.person = mean(Sanctuary, na.rm = TRUE),
                  online = mean(Sanctuary_online, na.rm = TRUE),
                  total = mean(total, na.rm = TRUE)
        )

##Total and percent changes
Sanctuary_combined_last_month_change <-
        tail(Sanctuary_monthly_average$total, 14)[13] - 
        tail(Sanctuary_monthly_average$total, 14)[1]

Sanctuary_combined_last_month_percent_change <-
        percent(((tail(Sanctuary_monthly_average$total, 14)[13] -
                          tail(Sanctuary_monthly_average$total, 14)[1])
                 /
                         tail(Sanctuary_monthly_average$total, 14)[1])
        )

Sanctuary_in_person_last_month_change <-
        tail(Sanctuary_monthly_average$in.person, 14)[13] -
        tail(Sanctuary_monthly_average$in.person, 14)[1]

Sanctuary_in_person_last_month_percent_change <-
        percent(((tail(Sanctuary_monthly_average$in.person, 14)[13] -
                          tail(Sanctuary_monthly_average$in.person, 14)[1])
                 /
                         tail(Sanctuary_monthly_average$in.person, 14)[1])
        )

Sanctuary_online_last_month_change <-
        tail(Sanctuary_monthly_average$online, 14)[13] -
        tail(Sanctuary_monthly_average$online, 14)[1]

Sanctuary_online_last_month_percent_change <-
        percent(((tail(Sanctuary_monthly_average$online, 14)[13] -
                          tail(Sanctuary_monthly_average$online, 14)[1])
                 /
                         tail(Sanctuary_monthly_average$online, 14)[1])
        )

Sanctuary_combined_current_month_change <-
        tail(Sanctuary_monthly_average$total, 13)[13] -
        tail(Sanctuary_monthly_average$total, 13)[1]

Sanctuary_combined_current_month_percent_change <-
        percent(((tail(Sanctuary_monthly_average$total, 13)[13] -
                          tail(Sanctuary_monthly_average$total, 13)[1])
                 /
                         tail(Sanctuary_monthly_average$total, 13)[1])
        )

Sanctuary_in_person_current_month_change <-
        tail(Sanctuary_monthly_average$in.person, 13)[13] -
        tail(Sanctuary_monthly_average$in.person, 13)[1]

Sanctuary_in_person_current_month_percent_change <-
        percent(((tail(Sanctuary_monthly_average$in.person, 13)[13] -
                          tail(Sanctuary_monthly_average$in.person, 13)[1])
                 /
                         tail(Sanctuary_monthly_average$in.person, 13)[1])
        )

Sanctuary_online_current_month_change <-
        tail(Sanctuary_monthly_average$online, 13)[13] -
        tail(Sanctuary_monthly_average$online, 13)[1]

Sanctuary_online_current_month_percent_change <- 
        percent(((tail(Sanctuary_monthly_average$online, 13)[13] -
                          tail(Sanctuary_monthly_average$online, 13)[1])
                 /
                         tail(Sanctuary_monthly_average$online, 13)[1])
        )

###Pull out monthly data
Sanctuary_previous_month <- month.name[as.numeric(format(as.Date(tail(Sanctuary_monthly_average$Date, 13)[12]), "%m"))]
Sanctuary_current_month <- month.name[as.numeric(format(as.Date(tail(Sanctuary_monthly_average$Date, 13)[13]), "%m"))]

##Tables
Sanctuary_change_table <- matrix(c(Sanctuary_previous_month,
                                   format(round(Sanctuary_combined_last_month_change, 1),
                                          nsmall = 1),
                                   format(round(Sanctuary_in_person_last_month_change, 1),
                                          nsmall = 1),
                                   format(round(Sanctuary_online_last_month_change, 1),
                                          nsmall = 1),
                                   Sanctuary_previous_month,
                                   Sanctuary_combined_last_month_percent_change,
                                   Sanctuary_in_person_last_month_percent_change,
                                   Sanctuary_online_last_month_percent_change,
                                   Sanctuary_current_month,
                                   format(round(Sanctuary_combined_current_month_change, 1),
                                          nsmall = 1),
                                   format(round(Sanctuary_in_person_current_month_change, 1),
                                          nsmall = 1),
                                   format(round(Sanctuary_online_current_month_change, 1),
                                          nsmall = 1),
                                   Sanctuary_current_month,
                                   Sanctuary_combined_current_month_percent_change,
                                   Sanctuary_in_person_current_month_percent_change,
                                   Sanctuary_online_current_month_percent_change
),
ncol = 4,
byrow = FALSE
)

colnames(Sanctuary_change_table) <- c("Total change",
                                      "Percent change",
                                      "Total change",
                                      "Percent change")

rownames(Sanctuary_change_table) <- c("Month",
                                      "Combined",
                                      "In person",
                                      "Online")

Sanctuary_overview_table <- matrix(c(tail(Sanctuary_base$total, 1),
                                     tail(Sanctuary_base$Sanctuary, 1),
                                     tail(Sanctuary_base$Sanctuary_online, 1),
                                     tail(Sanctuary_monthly_average$total, 1),
                                     tail(Sanctuary_monthly_average$in.person, 1),
                                     tail(Sanctuary_monthly_average$online, 1),
                                     tail(Sanctuary_monthly_average$total, 2)[1],
                                     tail(Sanctuary_monthly_average$in.person, 2)[1],
                                     tail(Sanctuary_monthly_average$online, 2)[1],
                                     tail(Sanctuary_yearly_average$total, 1),
                                     tail(Sanctuary_yearly_average$in.person, 1),
                                     tail(Sanctuary_yearly_average$online, 1),
                                     tail(Sanctuary_yearly_average$total, 2)[1],
                                     tail(Sanctuary_yearly_average$in.person, 2)[1],
                                     tail(Sanctuary_yearly_average$online, 2)[1]),
                                   ncol = 5,
                                   byrow = FALSE
)

colnames(Sanctuary_overview_table) <- c("Last week",
                                        "Current Month-to-date",
                                        "Previous Month Average",
                                        "Current Year-to-date",
                                        "Previous Year Average")

rownames(Sanctuary_overview_table) <- c("Total",
                                        "In person",
                                        "Online")

Sanctuary_in_person <- Sanctuary_monthly_average %>%
        select(Date, in.person) %>%
        rename(attendance = in.person)

Sanctuary_online <- Sanctuary_monthly_average %>%
        select(Date, online) %>%
        rename(attendance = online) %>%
        filter(Date >= "2017-12-01")

Sanctuary_total <- Sanctuary_monthly_average %>%
        select(Date, total) %>%
        rename(attendance = total)
