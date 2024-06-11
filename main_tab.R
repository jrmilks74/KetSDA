library(tidyverse)
library(scales)
library(timetk)

source("source_data.R")

# Calculations for the main tab

### Main table calculations
# Get last week's attendance numbers
Last_week <- tail(Weekly_data, 1) |>
        select(In_person,
               Online,
               Total)

# Calculate the month-to-date weekly average for the current month
Month_to_date <- Weekly_data |>
        select(Date,
               In_person,
               Online,
               Total) |>
        group_by(floor_date(Date, "month")) |>
        summarize(In_person = round(mean(In_person, na.rm = TRUE), 2),
                  Online = round(mean(Online, na.rm = TRUE), 2),
                  Total = round(mean(Total, na.rm = TRUE), 2)) |>
        filter(row_number() == n())

# Calculate the weekly average for the same month the previous year
Year_over_year <- Weekly_data |>
        select(Date,
               In_person,
               Online,
               Total) |>
        group_by(floor_date(Date, "month")) |>
        summarize(In_person = round(mean(In_person, na.rm = TRUE), 2),
                  Online = round(mean(Online, na.rm = TRUE), 2),
                  Total = round(mean(Total, na.rm = TRUE), 2)) |>
        slice_tail(n = 13) |>
        slice_head(n = 1)

# Absolute year-over-year change and percent change
YoY_Month <- data.frame(Total = round(Month_to_date$Total - Year_over_year$Total, 1),
                        In_person = round(Month_to_date$In_person - Year_over_year$In_person, 1),
                        Online = round(Month_to_date$Online - Year_over_year$Online, 1))
YoY_Month <- YoY_Month |>
        mutate(Total_percent = YoY_Month$Total / Year_over_year$Total,
               In_person_percent = YoY_Month$In_person / Year_over_year$In_person,
               Online_percent = YoY_Month$Online / Year_over_year$Online)

# Year-to-date averages for current and past year
By_week <- Weekly_data |>
        mutate(Year = year(Date)) |>
        mutate(Month = month(Date, label = TRUE))

End_week <- tail(By_week$Week, 1)
Current_year <- tail(By_week$Year, 1)
Previous_year <- Current_year - 1
Current_month <- tail(By_week$Month, 1)

Attendance_past_year <- subset(By_week, Year == Previous_year & Week <= End_week)
Attendance_current_year <- subset(By_week, Year == Current_year)

Weekly_ave_past_year <- Attendance_past_year |>
        group_by(Year) |>
        summarise(Total = round(mean(Total), 1),
                  In_person = round(mean(In_person), 1),
                  Online = round(mean(Online), 1))

Weekly_ave_current_year <- Attendance_current_year |>
        group_by(Year) |>
        summarise(Total = round(mean(Total), 2),
                  In_person = round(mean(In_person), 2),
                  Online = round(mean(Online), 2))

# Main summary table
main_table <- matrix(c(
        Last_week$Total,
        Last_week$In_person,
        Last_week$Online,
        Month_to_date$Total,
        Month_to_date$In_person,
        Month_to_date$Online,
        Year_over_year$Total,
        Year_over_year$In_person,
        Year_over_year$Online,
        YoY_Month$Total,
        YoY_Month$In_person,
        YoY_Month$Online,
        label_percent(accuracy = 0.1)(YoY_Month$Total_percent),
        label_percent(accuracy = 0.1)(YoY_Month$In_person_percent),
        label_percent(accuracy = 0.1)(YoY_Month$Online_percent)),
        ncol = 5,
        byrow = FALSE
)

colnames(main_table) <- c("Last week",
                      paste(Current_month, Current_year, sep = " "),
                      paste(Current_month, Previous_year, sep = " "),
                      "Change",
                      "Percent change")

rownames(main_table) <- c("Combined",
                          "In person",
                          "Online")

# Year-to-date averages, absolute change, and percent change table
ytd_change <- data.frame(Total = Weekly_ave_current_year$Total - Weekly_ave_past_year$Total,
                         In_person = Weekly_ave_current_year$In_person - Weekly_ave_past_year$In_person,
                         Online = Weekly_ave_current_year$Online - Weekly_ave_past_year$Online)

ytd_change <- ytd_change |>
        mutate(Total_percent = ytd_change$Total / Weekly_ave_past_year$Total,
               In_person_percent = ytd_change$In_person / Weekly_ave_past_year$In_person,
               Online_percent = ytd_change$Online / Weekly_ave_past_year$Online)

ytd_table <- matrix(c(Weekly_ave_current_year$Total,
                               Weekly_ave_current_year$In_person,
                               Weekly_ave_current_year$Online,
                               Weekly_ave_past_year$Total,
                               Weekly_ave_past_year$In_person,
                               Weekly_ave_past_year$Online,
                               round(ytd_change$Total, 1),
                               round(ytd_change$In_person, 1),
                               round(ytd_change$Online, 1),
                               label_percent(accuracy = 0.1)(ytd_change$Total_percent),
                               label_percent(accuracy = 0.1)(ytd_change$In_person_percent),
                               label_percent(accuracy = 0.1)(ytd_change$Online_percent)),
                             ncol = 4,
                             byrow = FALSE
        
)

colnames(ytd_table) <- c(paste(Current_year),
                                  paste(Previous_year),
                                  "Change",
                                  "Percent change")

rownames(ytd_table) <- c("Combined",
                                  "In person",
                                  "Online")

# Data sets for reactive selection option
## In person
in_person_data <- Weekly_data |>
        select(Date,
               In_person) |>
        group_by(Date = floor_date(Date, "month")) |>
        summarize(attendance = round(mean(In_person, na.rm = TRUE), 2))

## Online
online <- Weekly_data |>
        select(Date,
               Online) |>
        group_by(Date = floor_date(Date, "month")) |>
        summarize(attendance = round(mean(Online, na.rm = TRUE), 2))

## Combined
combined <- Weekly_data |>
        select(Date,
               Total) |>
        group_by(Date = floor_date(Date, "month")) |>
        summarize(attendance = round(mean(Total, na.rm = TRUE), 2))