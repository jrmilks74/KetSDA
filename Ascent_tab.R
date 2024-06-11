library(tidyverse)
library(scales)
library(timetk)

source("source_data.R")

# Calculations for the Ascent tab
### Create the base data set
Ascent_base <- Weekly_data |>
        select(Date,
               Total_Ascent,
               Ascent, 
               AscentOnline) |>
        rename(Total = Total_Ascent,
               In_person = Ascent,
               Online = AscentOnline)

# Ascent Main Table
Ascent_Last_week <- tail(Ascent_base, 1) |>
        select(Total,
               In_person,
               Online)

# Weekly mean month-to-date for current month
Ascent_Month_To_Date <- Ascent_base |>
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
Ascent_Year_over_year <- Ascent_base |>
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
Ascent_YoY_Month <- data.frame(Total = round(Ascent_Month_To_Date$Total - Ascent_Year_over_year$Total, 1),
                        In_person = round(Ascent_Month_To_Date$In_person - Ascent_Year_over_year$In_person, 1),
                        Online = round(Ascent_Month_To_Date$Online - Ascent_Year_over_year$Online, 1))
Ascent_YoY_Month <- Ascent_YoY_Month |>
        mutate(Total_percent = Ascent_YoY_Month$Total / Ascent_Year_over_year$Total,
               In_person_percent = Ascent_YoY_Month$In_person / Ascent_Year_over_year$In_person,
               Online_percent = Ascent_YoY_Month$Online / Ascent_Year_over_year$Online)

# Year-to-date averages for current and past year
Ascent_By_week <- Weekly_data |>
        mutate(Year = year(Date)) |>
        mutate(Month = month(Date, label = TRUE))

Ascent_End_week <- tail(Ascent_By_week$Week, 1)
Ascent_Current_year <- tail(Ascent_By_week$Year, 1)
Ascent_Previous_year <- Ascent_Current_year - 1
Ascent_Current_month <- tail(Ascent_By_week$Month, 1)

Ascent_Attendance_past_year <- subset(Ascent_By_week, Year == Previous_year & Week <= End_week)
Ascent_Attendance_current_year <- subset(Ascent_By_week, Year == Current_year)

Ascent_Weekly_ave_past_year <- Ascent_Attendance_past_year |>
        group_by(Year) |>
        summarise(Total = round(mean(Total_Ascent), 1),
                  In_person = round(mean(Ascent), 1),
                  Online = round(mean(AscentOnline), 1))

Ascent_Weekly_ave_current_year <- Attendance_current_year |>
        group_by(Year) |>
        summarise(Total = round(mean(Total_Ascent), 2),
                  In_person = round(mean(Ascent), 2),
                  Online = round(mean(AscentOnline), 2))

# Ascent summary table
Ascent_main_table <- matrix(c(
        Ascent_Last_week$Total,
        Ascent_Last_week$In_person,
        Ascent_Last_week$Online,
        Ascent_Month_To_Date$Total,
        Ascent_Month_To_Date$In_person,
        Ascent_Month_To_Date$Online,
        Ascent_Year_over_year$Total,
        Ascent_Year_over_year$In_person,
        Ascent_Year_over_year$Online,
        Ascent_YoY_Month$Total,
        Ascent_YoY_Month$In_person,
        Ascent_YoY_Month$Online,
        label_percent(accuracy = 0.1)(Ascent_YoY_Month$Total_percent),
        label_percent(accuracy = 0.1)(Ascent_YoY_Month$In_person_percent),
        label_percent(accuracy = 0.1)(Ascent_YoY_Month$Online_percent)),
        ncol = 5,
        byrow = FALSE
)

colnames(Ascent_main_table) <- c("Last week",
                          paste(Ascent_Current_month, Ascent_Current_year, sep = " "),
                          paste(Ascent_Current_month, Ascent_Previous_year, sep = " "),
                          "Change",
                          "Percent change")

rownames(Ascent_main_table) <- c("Combined",
                          "In person",
                          "Online")

# Year-to-date averages, absolute change, and percent change table
Ascent_ytd_change <- data.frame(Total = Ascent_Weekly_ave_current_year$Total - Ascent_Weekly_ave_past_year$Total,
                         In_person = Ascent_Weekly_ave_current_year$In_person - Ascent_Weekly_ave_past_year$In_person,
                         Online = Ascent_Weekly_ave_current_year$Online - Ascent_Weekly_ave_past_year$Online)

Ascent_ytd_change <- Ascent_ytd_change |>
        mutate(Total_percent = Ascent_ytd_change$Total / Ascent_Weekly_ave_past_year$Total,
               In_person_percent = Ascent_ytd_change$In_person / Ascent_Weekly_ave_past_year$In_person,
               Online_percent = Ascent_ytd_change$Online / Ascent_Weekly_ave_past_year$Online)

Ascent_ytd_table <- matrix(c(Ascent_Weekly_ave_current_year$Total,
                      Ascent_Weekly_ave_current_year$In_person,
                      Ascent_Weekly_ave_current_year$Online,
                      Ascent_Weekly_ave_past_year$Total,
                      Ascent_Weekly_ave_past_year$In_person,
                      Ascent_Weekly_ave_past_year$Online,
                      round(Ascent_ytd_change$Total, 1),
                      round(Ascent_ytd_change$In_person, 1),
                      round(Ascent_ytd_change$Online, 1),
                      label_percent(accuracy = 0.1)(Ascent_ytd_change$Total_percent),
                      label_percent(accuracy = 0.1)(Ascent_ytd_change$In_person_percent),
                      label_percent(accuracy = 0.1)(Ascent_ytd_change$Online_percent)),
                    ncol = 4,
                    byrow = FALSE
                    
)

colnames(Ascent_ytd_table) <- c(paste(Current_year),
                         paste(Previous_year),
                         "Change",
                         "Percent change")

rownames(Ascent_ytd_table) <- c("Combined",
                         "In person",
                         "Online")

# Data sets for reactive selection
## Ascent In person
Ascent_in_person <- Ascent_base |>
        select(Date,
               In_person) |>
        group_by(Date = floor_date(Date, "month")) |>
        summarize(attendance = round(mean(In_person, na.rm = TRUE), 2))

## Ascent online
Ascent_online <- Ascent_base |>
        select(Date,
               Online) |>
        group_by(Date = floor_date(Date, "month")) |>
        summarise(attendance = round(mean(Online, na.rm = TRUE), 2))

## Ascent combined
Ascent_total <- Ascent_base |>
        select(Date,
               Total) |>
        group_by(Date = floor_date(Date, "month")) |>
        summarise(attendance = round(mean(Total, na.rm = TRUE), 2))
