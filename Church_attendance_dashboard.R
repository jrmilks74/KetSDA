#
# This is a Shiny web application.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Title: Kettering Adventist Church attendance dashboard
#Created by Jim Milks.
#All rights reserved
#
#Version 1 created: 03 November 2021
#Version 2 created: 17 March 2022. Added tabs 2 and 3
#Version 2.1 created: 02 June 2022. Added Year-over-year change tables to all tabs

library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(timetk)
library(plotly)
library(bbplot)
library(knitr)
library(googlesheets4)

# Loading the data
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
In_person <- Weekly_total %>%
        select(Date, First, Second) %>%
        rename(Ascent = First, Sanctuary = Second)
        pivot_longer(c("Ascent", "Sanctuary"), 
                     names_to = "Service", 
                     values_to = "attendance") %>%
        group_by(Date) %>%
        summarize(attendance = sum(attendance, na.rm = TRUE)) %>%
        mutate(attendance = replace(attendance, attendance == 0, NA)) %>%
        group_by(Date = floor_date(Date, "month")) %>%
        summarise(attendance = mean(attendance, na.rm = TRUE))

###Read in older attendance data
Monthly_average <- read_sheet("https://docs.google.com/spreadsheets/d/1DUtoxOBcbZaLovhMUrzyTBtkb8ZqvIveMMMu-eAQMns/edit#gid=0",
                              col_names = TRUE)

Monthly_average$Date <- sprintf("%d-%02d", Monthly_average$Year, Monthly_average$Month)
Monthly_average_sub <- subset(Monthly_average, Year < 2010)
Monthly_average_sub <- Monthly_average_sub[, -c(1, 2, 4)]
names(Monthly_average_sub) <- c("attendance",
                                "Date")
Monthly_average_sub$Date <- as.Date(paste(Monthly_average_sub$Date, "-01", sep = ""))

###Combine the older and the recent attendance data into one
in_person <- rbind(Monthly_average_sub, In_person)

##Create weekly mean per month online viewership
online <- Weekly_total %>%
        select(Date, FirstServe24, SecondServe24) %>%
        rename(Ascent = FirstServe24, Sanctuary = SecondServe24) %>%
        pivot_longer(c("Ascent", "Sanctuary"), 
                     names_to = "Service", 
                     values_to = "attendance") %>%
        group_by(Date) %>%
        summarise(attendance = sum(attendance, na.rm = TRUE)) %>%
        mutate(attendance = replace(attendance, attendance == 0, NA)) %>%
        group_by(Date = floor_date(Date, "month")) %>%
        summarise(attendance = mean(attendance, na.rm = TRUE))

##Create weekly mean per month total attendance
combined <- Weekly_total %>%
        select(Date, First, Second, FirstServe24, SecondServe24) %>%
        pivot_longer(c("First", "Second", "FirstServe24", "SecondServe24"), 
                     names_to = "Service", 
                     values_to = "attendance") %>%
        group_by(Date) %>%
        summarise(attendance = sum(attendance, na.rm = TRUE)) %>%
        mutate(attendance = replace(attendance, attendance == 0, NA)) %>%
        group_by(Date = floor_date(Date, "month")) %>%
        summarise(attendance = mean(attendance, na.rm = TRUE))

##Summarize data by year for combined, online-only, and in-person attendance
yearly_combined <- Weekly_total %>%
        select(Date, First, Second, FirstServe24, SecondServe24) %>%
        pivot_longer(c("First", "Second", "FirstServe24", "SecondServe24"), 
                     names_to = "Service", 
                     values_to = "attendance") %>%
        group_by(Date) %>%
        summarise(attendance = sum(attendance, na.rm = TRUE)) %>%
        mutate(attendance = replace(attendance, attendance == 0, NA)) %>%
        group_by(Date = floor_date(Date, "year")) %>%
        summarise(attendance = mean(attendance, na.rm = TRUE))

yearly_in_person <- Weekly_total %>%
        select(Date, First, Second) %>%
        pivot_longer(c("First", "Second"), 
                     names_to = "Service", 
                     values_to = "attendance") %>%
        group_by(Date) %>%
        summarise(attendance = sum(attendance, na.rm = TRUE)) %>%
        mutate(attendance = replace(attendance, attendance == 0, NA)) %>%
        group_by(Date = floor_date(Date, "year")) %>%
        summarise(attendance = mean(attendance, na.rm = TRUE))

yearly_online <- Weekly_total %>%
        select(Date, FirstServe24, SecondServe24) %>%
        pivot_longer(c("FirstServe24", "SecondServe24"), 
                     names_to = "Service", 
                     values_to = "attendance") %>%
        group_by(Date) %>%
        summarise(attendance = sum(attendance, na.rm = TRUE)) %>%
        mutate(attendance = replace(attendance, attendance == 0, NA)) %>%
        group_by(Date = floor_date(Date, "year")) %>%
        summarise(attendance = mean(attendance, na.rm = TRUE))

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
        tail(in_person$attendance, 14)[13] -
        tail(in_person$attendance, 14)[1]

in_person_last_month_percent_change <-
        percent(((tail(in_person$attendance, 14)[13] -
                          tail(in_person$attendance, 14)[1])
                 /
                         tail(in_person$attendance, 14)[1])
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
        tail(in_person$attendance, 13)[13] -
        tail(in_person$attendance, 13)[1]

in_person_current_month_percent_change <-
        percent(((tail(in_person$attendance, 13)[13] -
                          tail(in_person$attendance, 13)[1])
                 /
                         tail(in_person$attendance, 13)[1])
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
Weekly_combined <- Weekly_total %>%
        select(Date, First, Second, FirstServe24, SecondServe24) %>%
        pivot_longer(c("First", "Second", "FirstServe24", "SecondServe24"), 
                     names_to = "Service", 
                     values_to = "attendance") %>%
        group_by(Date) %>%
        summarise(attendance = sum(attendance, na.rm = TRUE)) %>%
        mutate(attendance = replace(attendance, attendance == 0, NA))

Weekly_in_person <- Weekly_total %>%
        select(Date, First, Second) %>%
        pivot_longer(c("First", "Second"), 
                     names_to = "Service", 
                     values_to = "attendance") %>%
        group_by(Date) %>%
        summarise(attendance = sum(attendance, na.rm = TRUE)) %>%
        mutate(attendance = replace(attendance, attendance == 0, NA))

Weekly_online <- Weekly_total %>%
        select(Date, FirstServe24, SecondServe24) %>%
        pivot_longer(c("FirstServe24", "SecondServe24"), 
                     names_to = "Service", 
                     values_to = "attendance") %>%
        group_by(Date) %>%
        summarise(attendance = sum(attendance, na.rm = TRUE)) %>%
        mutate(attendance = replace(attendance, attendance == 0, NA))

##Pull monthly data out for tables
previous_month <- month.name[as.numeric(format(as.Date(tail(combined$Date, 13)[12]), "%m"))]
current_month <- month.name[as.numeric(format(as.Date(tail(combined$Date, 13)[13]), "%m"))]

##Create tables
latest <- matrix(c(tail(Weekly_combined$attendance, 1),
                   tail(Weekly_in_person$attendance, 1),
                   tail(Weekly_online$attendance, 1),
                   tail(combined$attendance, 1),
                   tail(in_person$attendance, 1),
                   tail(online$attendance, 1),
                   tail(combined$attendance, 2)[1],
                   tail(in_person$attendance, 2)[1],
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

# Calculations for the Ascent tab
##Create the base dataset
Ascent <- Weekly_total %>%
        select(Date, First, FirstServe24) %>%
        rename(Ascent = First, Ascent_online = FirstServe24) %>%
        group_by(Date) %>%
        mutate(total = sum(c(Ascent, Ascent_online), na.rm = TRUE)) %>%
        mutate(total = replace(total, total == 0, NA))

##Weekly mean per month
Ascent_monthly_average <- Ascent %>%
        group_by(month = floor_date(Date, "month")) %>%
        summarize(in.person = mean(Ascent, na.rm = TRUE),
                  online = mean(Ascent_online, na.rm = TRUE),
                  total = mean(total, na.rm = TRUE)
                  )

##Weekly mean per year
Ascent_yearly_average <- Ascent %>%
        group_by(year = floor_date(Date, "year")) %>%
        summarize(in.person = mean(Ascent, na.rm = TRUE),
                  online = mean(Ascent_online, na.rm = TRUE),
                  total = mean(total, na.rm = TRUE)
                  )

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
Ascent_previous_month <- month.name[as.numeric(format(as.Date(tail(Ascent_monthly_average$month, 13)[12]), "%m"))]
Ascent_current_month <- month.name[as.numeric(format(as.Date(tail(Ascent_monthly_average$month, 13)[13]), "%m"))]

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

Ascent_overview_table <- matrix(c(tail(Ascent$total, 1),
                                  tail(Ascent$Ascent, 1),
                                  tail(Ascent$Ascent_online, 1),
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

Ascent_in_person <- data.frame(Date = Ascent_monthly_average$month,
                               attendance = Ascent_monthly_average$in.person)

Ascent_online <- data.frame(Date = Ascent_monthly_average$month,
                            attendance = Ascent_monthly_average$online)

Ascent_total <- data.frame(Date = Ascent_monthly_average$month,
                           attendance = Ascent_monthly_average$total)

# Calculations for the Sanctuary tab
##Base dataset
Sanctuary <- Weekly_total %>%
        select(Date, Second, SecondServe24) %>%
        rename(Sanctuary = Second, Sanctuary_online = SecondServe24) %>%
        group_by(Date) %>%
        mutate(total = sum(c(Sanctuary, Sanctuary_online), na.rm = TRUE)) %>%
        mutate(total = replace(total, total == 0, NA))

##Weekly average per month
Sanctuary_monthly_average <- Sanctuary %>%
        group_by(month = floor_date(Date, "month")) %>%
        summarize(in.person = mean(Sanctuary, na.rm = TRUE),
                  online = mean(Sanctuary_online, na.rm = TRUE),
                  total = mean(total, na.rm = TRUE)
                  )

##Weekly average per year
Sanctuary_yearly_average <- Sanctuary %>%
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
Sanctuary_previous_month <- month.name[as.numeric(format(as.Date(tail(Sanctuary_monthly_average$month, 13)[12]), "%m"))]
Sanctuary_current_month <- month.name[as.numeric(format(as.Date(tail(Sanctuary_monthly_average$month, 13)[13]), "%m"))]

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

Sanctuary_overview_table <- matrix(c(tail(Sanctuary$total, 1),
                                     tail(Sanctuary$Sanctuary, 1),
                                     tail(Sanctuary$Sanctuary_online, 1),
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

Sanctuary_in_person <- data.frame(Date = Sanctuary_monthly_average$month,
                                  attendance = Sanctuary_monthly_average$in.person)

Sanctuary_online <- data.frame(Date = Sanctuary_monthly_average$month,
                               attendance = Sanctuary_monthly_average$online)

Sanctuary_total <- data.frame(Date = Sanctuary_monthly_average$month,
                              attendance = Sanctuary_monthly_average$total)


# Define UI for application that displays a table and graph
ui <- fluidPage(

    # Application title
    titlePanel("Kettering SDA Church Attendance"),
    tabsetPanel(

    # Sidebar with a menu input for data set displayed on the graph
    tabPanel(
            "Overall",
            sidebarLayout(
                    sidebarPanel(
                            selectInput("dataset", label = h3("Attendance data set"),
                                        choices = list("Combined" = "combined",
                                                       "In Person" = "in_person",
                                                       "Online" = "online"),
                                        selected = combined),
                            p("There are three data sets to choose from: In-person attendance, online audience, and combined in-person + online. The default view is the combined data set."),
                            
                            selectInput("time", label = h3("Month"),
                                        choices = list("All" = "all",
                                                       "January" = "jan",
                                                       "February" = "feb",
                                                       "March" = "mar",
                                                       "April" = "apr",
                                                       "May" = "may",
                                                       "June" = "jun",
                                                       "July" = "jul",
                                                       "August" = "aug",
                                                       "September" = "sept",
                                                       "October" = "oct",
                                                       "November" = "nov",
                                                       "December" = "dec"),
                                        selected = all),
                            p("Select the month to display on the graph")
                    ),
                    
                    mainPanel(
                            h3("By the Numbers"),
                            h4("Current Attendance per week"),
                            tableOutput("latest"),
                            h3("Year-over-year change from the same month last year"),
                            tableOutput("total_change_table"),
                            h3("Attendance Over Time"),
                            plotlyOutput("p")
                            )
                    )
            ),
    
    tabPanel(
            "Ascent",
            sidebarLayout(
                    sidebarPanel(
                            selectInput("Ascent_dataset", label = h3("Attendance data set"),
                                        choices = list("Total" = "Ascent_total",
                                                       "In Person" = "Ascent_in_person",
                                                       "Online" = "Ascent_online"),
                                        selected = Ascent_total),
                            p("There are three data sets to choose from: In-person attendance, online audience, and total attendance. The default view is total attendance.")
                            
                            selectInput("Ascent_time", label = h3("Month"),
                                        choices = list("All" = "all",
                                                       "January" = "jan",
                                                       "February" = "feb",
                                                       "March" = "mar",
                                                       "April" = "apr",
                                                       "May" = "may",
                                                       "June" = "jun",
                                                       "July" = "jul",
                                                       "August" = "aug",
                                                       "September" = "sept",
                                                       "October" = "oct",
                                                       "November" = "nov",
                                                       "December" = "dec"),
                                        selected = all),
                            p("Select the month to display on the graph")
                    ),
                    
                    mainPanel(
                            h3("By the Numbers"),
                            h4("Current Attendance per week"),
                            tableOutput("Ascent_overview_table"),
                            h4("Year-over-year change from the same month last year"),
                            tableOutput("Ascent_change_table"),
                            h3("Attendance Over Time"),
                            plotlyOutput("Ascent_plot")
                            )
                    )
            ),
    
    tabPanel(
            "Sanctuary",
            sidebarLayout(
                    sidebarPanel(
                            selectInput("Sanctuary_dataset", label = h3("Attendance data set"),
                                        choices = list("Total" = "Sanctuary_total",
                                                       "In Person" = "Sanctuary_in_person",
                                                       "Online" = "Sanctuary_online"),
                                        selected = Sanctuary_total),
                            p("There are three data sets to choose from: In-person attendance, online audience, and total attendance. The default view is total attendance.")
                            
                            selectInput("Sanctuary_time", label = h3("Month"),
                                        choices = list("All" = "all",
                                                       "January" = "jan",
                                                       "February" = "feb",
                                                       "March" = "mar",
                                                       "April" = "apr",
                                                       "May" = "may",
                                                       "June" = "jun",
                                                       "July" = "jul",
                                                       "August" = "aug",
                                                       "September" = "sept",
                                                       "October" = "oct",
                                                       "November" = "nov",
                                                       "December" = "dec"),
                                        selected = all),
                            p("Select the month to display on the graph")
                    ),
                    mainPanel(
                            h3("By the Numbers"),
                            h4("Current attendance per week"),
                            tableOutput("Sanctuary_overview_table"),
                            h4("Year-over-year change from the same month last year"),
                            tableOutput("Sanctuary_change_table"),
                            h3("Attendance Over Time"),
                            plotlyOutput("Sanctuary_plot")
                            )
                    )
            )
    ),
    
    hr(),
    h4("Created by: Jim Milks"),
    "Version 2.1",
    br(),
    "Code and data available at:", 
    a(href = "https://github.com/jrmilks74/KetSDA/tree/main", "https://github.com/jrmilks74/KetSDA/tree/main"),
    br(),
    "Data updated every Monday"
)

# Define server logic required to render the table and time series plot
server <- function(input, output) {
        output$latest <- renderTable(latest, align = "c", rownames = TRUE)
        output$total_change_table <- renderTable(total_change_table, align = "c", rownames = TRUE)
        
        data_reactive <- reactive({
                if (input$dataset == "in_person")
                        in_person
                else if (input$dataset == "online")
                        online
                else
                        combined
        })
        
        time_period <- reactive({
                if (input$time == "jan")
                        1
                else if (input$time == "feb")
                        2
                else if (input$time == "mar")
                        3
                else if (input$time == "apr")
                        4
                else if (input$time == "may")
                        5
                else if (input$time == "jun")
                        6
                else if (input$time == "jul")
                        7
                else if (input$time == "aug")
                        8
                else if (input$time == "sept")
                        9
                else if (input$time == "oct")
                        10
                else if (input$time == "nov")
                        11
                else if (input$time == "dec")
                        12
                else
                        1:12
        })
        
        output$p <- renderPlotly({ 
                data_set = data_reactive() %>%
                        filter(month(Date) == time_period())
                
                ggplot(data = data_set, aes(x = Date, y = attendance)) +
                        geom_point() +
                        geom_line() +
                        geom_smooth(method = "loess", formula = "y~x", col = "red") +
                        bbc_style() +
                        labs(title = "Weekly average attendance by month",
                             x = "Date",
                             y = "Average attendance")
        })        


        output$Ascent_overview_table <- renderTable(Ascent_overview_table, align = "c",
                                                    rownames = TRUE)
        
        output$Ascent_change_table <- renderTable(Ascent_change_table, align = "c",
                                                  rownames = TRUE)
        
        Ascent_data_reactive <- reactive({
                if (input$Ascent_dataset == "Ascent_in_person")
                        Ascent_in_person
                else if (input$Ascent_dataset == "Ascent_online")
                        Ascent_online
                else
                        Ascent_total
        })
        
        Ascent_time_period <- reactive({
                if (input$Ascent_time == "jan")
                        1
                else if (input$Ascent_time == "feb")
                        2
                else if (input$Ascent_time == "mar")
                        3
                else if (input$Ascent_time == "apr")
                        4
                else if (input$Ascent_time == "may")
                        5
                else if (input$Ascent_time == "jun")
                        6
                else if (input$Ascent_time == "jul")
                        7
                else if (input$Ascent_time == "aug")
                        8
                else if (input$Ascent_time == "sept")
                        9
                else if (input$Ascent_time == "oct")
                        10
                else if (input$Ascent_time == "nov")
                        11
                else if (input$Ascent_time == "dec")
                        12
                else
                        1:12
        })
        
        output$Ascent_plot <- renderPlotly({ 
            Ascent_data_set = Ascent_data_reactive() %>%
                    filter(month(Date) == Ascent_time_period())
            
            ggplot(data = Ascent_data_set, aes(x = Date, y = attendance)) +
                    geom_point() +
                    geom_line() +
                    geom_smooth(method = "loess", formula = "y~x", col = "red") +
                    bbc_style() +
                    labs(title = "Weekly average attendance by month",
                         x = "Date",
                         y = "Average attendance")
    })
        output$Sanctuary_overview_table <- renderTable(Sanctuary_overview_table, align = "c",
                                                       rownames = TRUE)
        
        output$Sanctuary_change_table <- renderTable(Sanctuary_change_table, align = "c",
                                                     rownames = TRUE)
        
        Sanctuary_data_reactive <- reactive({
                if (input$Sanctuary_dataset == "Sanctuary_in_person")
                        Sanctuary_in_person
                else if (input$Sanctuary_dataset == "Sanctuary_online")
                        Sanctuary_online
                else
                        Sanctuary_total
                }
        )
        
        Sanctuary_time_period <- reactive({
                if (input$Sanctuary_time == "jan")
                        1
                else if (input$Sanctuary_time == "feb")
                        2
                else if (input$Sanctuary_time == "mar")
                        3
                else if (input$Sanctuary_time == "apr")
                        4
                else if (input$Sanctuary_time == "may")
                        5
                else if (input$Sanctuary_time == "jun")
                        6
                else if (input$Sanctuary_time == "jul")
                        7
                else if (input$Sanctuary_time == "aug")
                        8
                else if (input$Sanctuary_time == "sept")
                        9
                else if (input$Sanctuary_time == "oct")
                        10
                else if (input$Sanctuary_time == "nov")
                        11
                else if (input$Sanctuary_time == "dec")
                        12
                else
                        1:12
        })
        
        output$Sanctuary_plot <- renderPlotly({ 
                Sanctuary_data_set = Sanctuary_data_reactive() %>%
                        filter(month(Date) == Sanctuary_time_period())
                
                ggplot(data = Sanctuary_data_set, aes(x = Date, y = attendance)) +
                        geom_point() +
                        geom_line() +
                        geom_smooth(method = "loess", formula = "y~x", col = "red") +
                        bbc_style() +
                        labs(title = "Weekly average attendance by month",
                             x = "Date",
                             y = "Average attendance")
                }
        )  
}

shinyApp(ui = ui, server = server)
