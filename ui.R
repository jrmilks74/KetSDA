#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application
fluidPage(
        
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
                                                    selected = "combined"),
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
                                                    selected = "Ascent_total"),
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
                                                    selected = "Sanctuary_total"),
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