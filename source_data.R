# Load libraries
library(tidyverse)
library(scales)
library(timetk)
library(googlesheets4)

# Set Google Sheets options
options(
        gargle_oauth_email = TRUE,
        gargle_oauth_cache = "KetSDA/.secrets/"
)
gs4_auth(email = "office@ketsda.org")

# Load weekly data
Weekly_data <- read_sheet("https://docs.google.com/spreadsheets/d/1BsQR4TyAMkV2H09jzuj5yrOIotnOBTzLoSZ0r5PsBpk/edit#gid=0",
                           col_names = TRUE,
                           col_types = "Ddddddc") |>
        rename(Ascent = "First/Ascent",
               Sanctuary = "Second/Sanctuary",
               In_person = "total.attendance") |>
        mutate(Week = week(Date)) |>
        group_by(Date) |>
        mutate(Online = sum(c(AscentOnline, SanctuaryOnline), na.rm = TRUE),
               Total = sum(c(Online, In_person), na.rm = TRUE),
               Total_Ascent = sum(c(Ascent, AscentOnline), na.rm = TRUE),
               Total_Sanctuary = sum(c(Sanctuary, SanctuaryOnline), na.rm = TRUE)) |>
        mutate(Online = replace(Online, Online == 0, NA),
               Total = replace(Total, Total == 0, NA),
               Total_Ascent = replace(Total_Ascent, Total_Ascent == 0, NA),
               Total_Sanctuary = replace(Total_Sanctuary, Total_Sanctuary == 0, NA))
