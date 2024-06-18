#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(scales)
library(timetk)
library(plotly)
library(knitr)
library(kableExtra)

# Loading data and analysis results
source("main_tab.R")
source("Ascent_tab.R")
source("Sanctuary_tab.R")

# Define server logic required to draw a histogram
function(input, output, session) {
        # Main table
        output$main_table <- function() ({
          main_table <- kbl(main_table) |>
                  kable_styling(bootstrap_options = c("striped"),
                                full_width = FALSE) |>
                  add_header_above(c(" ", " ", "Weekly Average" = 2, "Year-over-year" = 2)) |>
                  column_spec(1, bold = T)
          main_table
        })
        
        # Year-to-date table
        output$year_to_date_table <- function() ({
                year_to_date_table <- kbl(ytd_table) |>
                        kable_styling(bootstrap_options = c("striped"),
                                      full_width = FALSE) |>
                        add_header_above(c(" ", "Year-to-date weekly average" = 2, " ", " ")) |>
                        column_spec(1, bold = T)
                year_to_date_table
        })
        
        # Reactive data set selection
        data_reactive <- reactive({
                if (input$dataset == "in_person")
                        in_person_data
                else if (input$dataset == "online")
                        online
                else
                        combined
        })
        
        # Reactive month selection
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
        
        # Seasonal plot, main tab
        output$p <- renderPlotly({ 
                data_set = data_reactive() %>%
                        filter(lubridate::month(Date) %in% time_period())
                
                ggplot(data = data_set, aes(x = Date, y = attendance)) +
                        theme_classic() +
                        geom_point() +
                        geom_line() +
                        geom_smooth(method = "loess", formula = "y~x", col = "red") +
                        labs(title = "Weekly average attendance by month",
                             x = "Date",
                             y = "Average attendance")
        })
        
        # Time series plot, main tab
        output$s <- renderPlotly({
                data_set = data_reactive()
                
                s <- data_set %>%
                        mutate(Year = factor(year(Date)),
                               Date = update(Date, year = 1)) %>%
                        ggplot(aes(x = Date, y = attendance, colour = Year)) +
                        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                        geom_line(aes(group = Year), colour = "black", alpha = 0.1) +
                        geom_line(data = function(x) filter(x, Year == Previous_year), lwd = 0.5) +
                        geom_line(data = function(x) filter(x, Year == Current_year), lwd = 1) +
                        theme_bw() +
                        labs(y = "Average attendance per week",
                             x = "Month")
                ggplotly(s)
        })
        
        # Ascent tab output
        # Ascent main table
        output$Ascent_main_table <- function() ({
                Ascent_main_table <- kbl(Ascent_main_table) |>
                        kable_styling(bootstrap_options = c("striped"),
                                      full_width = FALSE) |>
                        add_header_above(c(" ", " ", "Weekly Average" = 2, "Year-over-year" = 2)) |>
                        column_spec(1, bold = T)
                Ascent_main_table
        })
        
        # Ascent year-to-date table
        output$Ascent_ytd_table <- function() ({
                Ascent_ytd_table <- kbl(Ascent_ytd_table) |>
                        kable_styling(bootstrap_options = c("striped"),
                                      full_width = FALSE) |>
                        add_header_above(c(" ", "Weekly Average" = 2, " ", " ")) |>
                        column_spec(1, bold = T)
                Ascent_ytd_table
        })
        
        # Ascent data set reactive
        Ascent_data_reactive <- reactive({
                if (input$Ascent_dataset == "Ascent_in_person")
                        Ascent_in_person
                else if (input$Ascent_dataset == "Ascent_online")
                        Ascent_online
                else
                        Ascent_total
        })
        
        # Ascent month selection reactive
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
        
        # Ascent seasonal plot
        output$Ascent_plot <- renderPlotly({ 
                Ascent_data_set = Ascent_data_reactive() %>%
                        filter(lubridate::month(Date) %in% Ascent_time_period())
                
                ggplot(data = Ascent_data_set, aes(x = Date, y = attendance)) +
                        theme_classic() +
                        geom_point() +
                        geom_line() +
                        geom_smooth(method = "loess", formula = "y~x", col = "red") +
                        labs(title = "Weekly average attendance by month",
                             x = "Date",
                             y = "Average attendance")
        })
        
        # Ascent time series plot
        output$Ascent_s <- renderPlotly({
                Ascent_this_year <- Ascent_Current_year
                Ascent_previous_year <- Ascent_Previous_year
                Ascent_data_set = Ascent_data_reactive()
                
                Ascent_s <- Ascent_data_set %>%
                        mutate(Year = factor(year(Date)),
                               Date = update(Date, year = 1)) %>%
                        ggplot(aes(x = Date, y = attendance, colour = Year)) +
                        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                        geom_line(aes(group = Year), colour = "black", alpha = 0.1) +
                        geom_line(data = function(x) filter(x, Year == Ascent_previous_year), lwd = 0.5) +
                        geom_line(data = function(x) filter(x, Year == Ascent_this_year), lwd = 1) +
                        theme_bw() +
                        labs(y = "Average attendance per week",
                             x = "Month")
                ggplotly(Ascent_s)
        })
        
        # Sanctuary tab output
        output$Sanctuary_main_table <- function() ({
                Sanctuary_main_table <- kbl(Sanctuary_main_table) |>
                        kable_styling(bootstrap_options = c("striped"),
                                      full_width = FALSE) |>
                        add_header_above(c(" ", " ", "Weekly Average" = 2, "Year-over-year" = 2)) |>
                        column_spec(1, bold = T)
                Sanctuary_main_table
        })
        
        output$Sanctuary_ytd_table <- function() ({
                Sanctuary_ytd_table <- kbl(Sanctuary_ytd_table) |>
                        kable_styling(bootstrap_options = c("striped"),
                                      full_width = FALSE) |>
                        add_header_above(c(" ", "Weekly Average" = 2, " ", " ")) |>
                        column_spec(1, bold = T)
                Sanctuary_ytd_table
        })
        
        Sanctuary_data_reactive <- reactive({
                if (input$Sanctuary_dataset == "Sanctuary_in_person")
                        Sanctuary_in_person
                else if (input$Sanctuary_dataset == "Sanctuary_online")
                        Sanctuary_online
                else
                        Sanctuary_total
        })
        
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
                        filter(lubridate::month(Date) %in% Sanctuary_time_period())
                
                ggplot(data = Sanctuary_data_set, aes(x = Date, y = attendance)) +
                        theme_classic() +
                        geom_point() +
                        geom_line() +
                        geom_smooth(method = "loess", formula = "y~x", col = "red") +
                        labs(title = "Weekly average attendance by month",
                             x = "Date",
                             y = "Average attendance")
        })
        
        output$Sanctuary_s <- renderPlotly({
                this_year <- Sanctuary_Current_year
                previous_year <- Sanctuary_Previous_year
                Sanctuary_data_set = Sanctuary_data_reactive()
                
                Sanctuary_s <- Sanctuary_data_set %>%
                        mutate(Year = factor(year(Date)),
                               Date = update(Date, year = 1)) %>%
                        ggplot(aes(x = Date, y = attendance, colour = Year)) +
                        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                        geom_line(aes(group = Year), colour = "black", alpha = 0.1) +
                        geom_line(data = function(x) filter(x, Year == previous_year), lwd = 0.5) +
                        geom_line(data = function(x) filter(x, Year == this_year), lwd = 1) +
                        theme_bw() +
                        labs(y = "Average attendance per week",
                             x = "Month")
                ggplotly(Sanctuary_s)
        })
}
