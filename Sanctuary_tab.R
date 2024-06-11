library(tidyverse)
library(scales)
library(timetk)
library(knitr)

source("source_data.R")

# Calculations for the Ascent tab
### Create the base data set
Sanctuary_base <- Weekly_data |>
        select(Date,
               Total_Sanctuary,
               Sanctuary, 
               SanctuaryOnline) |>
        rename(Total = Total_Sanctuary,
               In_person = Sanctuary,
               Online = SanctuaryOnline)

# Sanctuary Main Table
Sanctuary_Last_week <- tail(Sanctuary_base, 1) |>
        select(Total,
               In_person,
               Online)

# Weekly mean month-to-date for current month
Sanctuary_Month_To_Date <- Sanctuary_base |>
        select(Date,
               Total,
               In_person,
               Online) |>
        group_by(floor_date(Date, "month")) |>
        summarize(In_person = round(mean(In_person, na.rm = TRUE), 2),
                  Online = round(mean(Online, na.rm = TRUE), 2),
                  Total = round(mean(Total, na.rm = TRUE), 2)) |>
        filter(row_number() == n())

# Calculate the weekly average for the same month the previous year
Sanctuary_Year_over_year <- Sanctuary_base |>
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
Sanctuary_YoY_Month <- data.frame(Total = round(Sanctuary_Month_To_Date$Total - Sanctuary_Year_over_year$Total, 1),
                               In_person = round(Sanctuary_Month_To_Date$In_person - Sanctuary_Year_over_year$In_person, 1),
                               Online = round(Sanctuary_Month_To_Date$Online - Sanctuary_Year_over_year$Online, 1))
Sanctuary_YoY_Month <- Sanctuary_YoY_Month |>
        mutate(Total_percent = Sanctuary_YoY_Month$Total / Sanctuary_Year_over_year$Total,
               In_person_percent = Sanctuary_YoY_Month$In_person / Sanctuary_Year_over_year$In_person,
               Online_percent = Sanctuary_YoY_Month$Online / Sanctuary_Year_over_year$Online)

# Year-to-date averages for current and past year
Sanctuary_By_week <- Weekly_data |>
        mutate(Year = year(Date)) |>
        mutate(Month = month(Date, label = TRUE))

Sanctuary_End_week <- tail(Sanctuary_By_week$Week, 1)
Sanctuary_Current_year <- tail(Sanctuary_By_week$Year, 1)
Sanctuary_Previous_year <- Sanctuary_Current_year - 1
Sanctuary_Current_month <- tail(Sanctuary_By_week$Month, 1)

Sanctuary_Attendance_past_year <- subset(Sanctuary_By_week, Year == Previous_year & Week <= End_week)
Sanctuary_Attendance_current_year <- subset(Sanctuary_By_week, Year == Sanctuary_Current_year)

Sanctuary_Weekly_ave_past_year <- Sanctuary_Attendance_past_year |>
        group_by(Year) |>
        summarise(Total = round(mean(Total_Sanctuary), 1),
                  In_person = round(mean(Sanctuary), 1),
                  Online = round(mean(SanctuaryOnline), 1))

Sanctuary_Weekly_ave_current_year <- Sanctuary_Attendance_current_year |>
        group_by(Year) |>
        summarise(Total = round(mean(Total_Sanctuary), 2),
                  In_person = round(mean(Sanctuary), 2),
                  Online = round(mean(SanctuaryOnline), 2))

# Sanctuary summary table
Sanctuary_main_table <- matrix(c(
        Sanctuary_Last_week$Total,
        Sanctuary_Last_week$In_person,
        Sanctuary_Last_week$Online,
        Sanctuary_Month_To_Date$Total,
        Sanctuary_Month_To_Date$In_person,
        Sanctuary_Month_To_Date$Online,
        Sanctuary_Year_over_year$Total,
        Sanctuary_Year_over_year$In_person,
        Sanctuary_Year_over_year$Online,
        Sanctuary_YoY_Month$Total,
        Sanctuary_YoY_Month$In_person,
        Sanctuary_YoY_Month$Online,
        label_percent(accuracy = 0.1)(Sanctuary_YoY_Month$Total_percent),
        label_percent(accuracy = 0.1)(Sanctuary_YoY_Month$In_person_percent),
        label_percent(accuracy = 0.1)(Sanctuary_YoY_Month$Online_percent)),
        ncol = 5,
        byrow = FALSE
)

colnames(Sanctuary_main_table) <- c("Last week",
                                 paste(Sanctuary_Current_month, Sanctuary_Current_year, sep = " "),
                                 paste(Sanctuary_Current_month, Sanctuary_Previous_year, sep = " "),
                                 "Change",
                                 "Percent change")

rownames(Sanctuary_main_table) <- c("Combined",
                                 "In person",
                                 "Online")

# Year-to-date averages, absolute change, and percent change table
Sanctuary_ytd_change <- data.frame(Total = Sanctuary_Weekly_ave_current_year$Total - Sanctuary_Weekly_ave_past_year$Total,
                                In_person = Sanctuary_Weekly_ave_current_year$In_person - Sanctuary_Weekly_ave_past_year$In_person,
                                Online = Sanctuary_Weekly_ave_current_year$Online - Sanctuary_Weekly_ave_past_year$Online)

Sanctuary_ytd_change <- Sanctuary_ytd_change |>
        mutate(Total_percent = Sanctuary_ytd_change$Total / Sanctuary_Weekly_ave_past_year$Total,
               In_person_percent = Sanctuary_ytd_change$In_person / Sanctuary_Weekly_ave_past_year$In_person,
               Online_percent = Sanctuary_ytd_change$Online / Sanctuary_Weekly_ave_past_year$Online)

Sanctuary_ytd_table <- matrix(c(Sanctuary_Weekly_ave_current_year$Total,
                                Sanctuary_Weekly_ave_current_year$In_person,
                                Sanctuary_Weekly_ave_current_year$Online,
                                Sanctuary_Weekly_ave_past_year$Total,
                                Sanctuary_Weekly_ave_past_year$In_person,
                                Sanctuary_Weekly_ave_past_year$Online,
                             round(Sanctuary_ytd_change$Total, 1),
                             round(Sanctuary_ytd_change$In_person, 1),
                             round(Sanctuary_ytd_change$Online, 1),
                             label_percent(accuracy = 0.1)(Sanctuary_ytd_change$Total_percent),
                             label_percent(accuracy = 0.1)(Sanctuary_ytd_change$In_person_percent),
                             label_percent(accuracy = 0.1)(Sanctuary_ytd_change$Online_percent)),
                           ncol = 4,
                           byrow = FALSE
                           
)

colnames(Sanctuary_ytd_table) <- c(paste(Current_year),
                                paste(Previous_year),
                                "Change",
                                "Percent change")

rownames(Sanctuary_ytd_table) <- c("Combined",
                                "In person",
                                "Online")


# Data sets for reactive selection
## Sanctuary in person
Sanctuary_in_person <- Sanctuary_base |>
        select(Date,
               In_person) |>
        group_by(Date = floor_date(Date, "month")) |>
        summarize(attendance = round(mean(In_person, na.rm = TRUE), 2))

## Sanctuary online
Sanctuary_online <- Sanctuary_base |>
        select(Date,
               Online) |>
        group_by(Date = floor_date(Date, "month")) |>
        summarize(attendance = round(mean(Online, na.rm = TRUE), 2))

## Sanctuary combined attendance
Sanctuary_total <- Sanctuary_base |>
        select(Date,
               Total) |>
        group_by(Date = floor_date(Date, "month")) |>
        summarise(attendance = round(mean(Total, na.rm = TRUE), 2))