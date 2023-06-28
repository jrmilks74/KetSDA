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
#Version 3 created 17 Jan 2023. Rewrote data wrangling, added month drop down menu

library(shiny)
library(tidyverse)
library(scales)
library(timetk)
library(plotly)
library(knitr)

# Loading data and analysis results
source("main_tab.R")
source("Ascent_tab.R")
source("Sanctuary_tab.R")

# Define UI for application
ui <- fluidPage(
        
        # Application title
        titlePanel("Kettering SDA Church Attendance"),
        tabsetPanel(
                
                # Sidebar with a menu input for data set displayed on the graph
                tabPanel(
                        "Overall",
                        sidebarLayout(
                                sidebarPanel(
                                        selectInput("dataset", 
                                                    label = h3("Attendance data set"),
                                                    choices = list("Combined" = "combined",
                                                                   "In Person" = "in_person",
                                                                   "Online" = "online"),
                                                    selected = combined),
                                        p("There are three data sets to choose from: In-person attendance, online audience, and combined in-person + online. The default view is the combined data set."),
                                        
                                        selectInput("time", 
                                                    label = h3("Month"),
                                                    choices = list("All" = "all_months",
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
                                                    selected = "all_months"),
                                        p("Select the month to display on the graph")
                                ),
                                
                                mainPanel(
                                        h3("By the Numbers"),
                                        h4("Current Attendance per week"),
                                        tableOutput("latest"),
                                        h3("Year-over-year change from the same month last year"),
                                        tableOutput("total_change_table"),
                                        h3("Attendance Over Time"),
                                        plotlyOutput("p"),
                                        h3("Seasonal attendance patterns"),
                                        plotlyOutput("s")
                                )
                        )
                ),
                
                tabPanel(
                        "Ascent",
                        sidebarLayout(
                                sidebarPanel(
                                        selectInput("Ascent_dataset", 
                                                    label = h3("Attendance data set"),
                                                    choices = list("Total" = "Ascent_total",
                                                                   "In Person" = "Ascent_in_person",
                                                                   "Online" = "Ascent_online"),
                                                    selected = Ascent_total),
                                        p("There are three data sets to choose from: In-person attendance, online audience, and total attendance. The default view is total attendance."),
                                        
                                        selectInput("Ascent_time", 
                                                    label = h3("Month"),
                                                    choices = list("All" = "all_months",
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
                                                    selected = "all_months"),
                                        p("Select the month to display on the graph")
                                ),
                                
                                mainPanel(
                                        h3("By the Numbers"),
                                        h4("Current Attendance per week"),
                                        tableOutput("Ascent_overview_table"),
                                        h4("Year-over-year change from the same month last year"),
                                        tableOutput("Ascent_change_table"),
                                        h3("Attendance Over Time"),
                                        plotlyOutput("Ascent_plot"),
                                        h3("Seasonal attendance patterns"),
                                        plotlyOutput("Ascent_s")
                                )
                        )
                ),
                
                tabPanel(
                        "Sanctuary",
                        sidebarLayout(
                                sidebarPanel(
                                        selectInput("Sanctuary_dataset", 
                                                    label = h3("Attendance data set"),
                                                    choices = list("Total" = "Sanctuary_total",
                                                                   "In Person" = "Sanctuary_in_person",
                                                                   "Online" = "Sanctuary_online"),
                                                    selected = Sanctuary_total),
                                        p("There are three data sets to choose from: In-person attendance, online audience, and total attendance. The default view is total attendance."),
                                        
                                        selectInput("Sanctuary_time", 
                                                    label = h3("Month"),
                                                    choices = list("All" = "all_months",
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
                                                    selected = "all_months"),
                                        p("Select the month to display on the graph")
                                ),
                                mainPanel(
                                        h3("By the Numbers"),
                                        h4("Current attendance per week"),
                                        tableOutput("Sanctuary_overview_table"),
                                        h4("Year-over-year change from the same month last year"),
                                        tableOutput("Sanctuary_change_table"),
                                        h3("Attendance Over Time"),
                                        plotlyOutput("Sanctuary_plot"),
                                        h3("Seasonal attendance patterns"),
                                        plotlyOutput("Sanctuary_s")
                                )
                        )
                )
        ),
        
        hr(),
        h4("Created by: Jim Milks"),
        "Version 4.0",
        br(),
        "28 June 2023",
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
                        in_person_full_data
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
        
        output$s <- renderPlotly({
                        this_year <- year(tail(Combined_yearly$Date)[6])
                        previous_year <- year(tail(Combined_yearly$Date)[5])
                        data_set = data_reactive()
                        
                        s <- data_set %>%
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
                        ggplotly(s)
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
        
        output$Ascent_s <- renderPlotly({
                this_year <- year(tail(Combined_yearly$Date)[6])
                previous_year <- year(tail(Combined_yearly$Date)[5])
                Ascent_data_set = Ascent_data_reactive()
                
                Ascent_s <- Ascent_data_set %>%
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
                ggplotly(Ascent_s)
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
                this_year <- year(tail(Combined_yearly$Date)[6])
                previous_year <- year(tail(Combined_yearly$Date)[5])
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

shinyApp(ui = ui, server = server)