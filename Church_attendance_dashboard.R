#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Created by Jim Milks
#Version 1 created: 03 November 2021
#Version 2 created: 17 March 2022. Added tabs 2 and 3


library(shiny)
library(tidyverse)
library(lubridate)
library(shiny)
library(timetk)
library(plotly)
library(knitr)

# Loading the data
Weekly_total <- read.csv("Weekly total-Table 1.csv", header = T)
Weekly_total$Date <- as.Date(Weekly_total$Date)
Weekly_total$Online <- with(Weekly_total,
                            ifelse(is.na(FirstServe24) & is.na(SecondServe24), NA,
                                   ifelse(is.na(FirstServe24), 0 + SecondServe24,
                                          ifelse(is.na(SecondServe24), FirstServe24 + 0,
                                                 FirstServe24 + SecondServe24
                                          )
                                   )
                            )
)

Weekly_total$Combined <- with(Weekly_total,
                              ifelse(is.na(total.attendance) & is.na(Online), NA,
                                     ifelse(is.na(total.attendance), 0 + Online,
                                            ifelse(is.na(Online), total.attendance + 0,
                                                   total.attendance + Online
                                            )
                                     )
                              )
)


# Calculations for the first tab

Average_by_month <- Weekly_total %>% 
        group_by(month = floor_date(Date, "month")) %>% 
        summarize(attendance = mean(total.attendance, na.rm = TRUE))
names(Average_by_month) <- c("Date", "attendance")

Monthly_average <- read.csv("Monthly average-KetSDA.csv", header = T)
Monthly_average$Date <- sprintf("%d-%02d", Monthly_average$Year, Monthly_average$Month)
Monthly_average_sub <- subset(Monthly_average, Year < 2010)
Monthly_average_sub <- Monthly_average_sub[,-c(1,2,4)]
names(Monthly_average_sub) <- c("attendance", "Date")
Monthly_average_sub$Date <- as.Date(paste(Monthly_average_sub$Date, "-01", sep = ""))

in_person <- rbind(Monthly_average_sub, Average_by_month)

online <- Weekly_total %>% 
        group_by(Date = floor_date(Date, "month")) %>% 
        summarize(attendance = mean(Online, na.rm = TRUE)) %>%
        filter(Date >= as.Date("2017-12-01"))

combined <- Weekly_total %>% 
        group_by(Date = floor_date(Date, "month")) %>% 
        summarize(attendance = mean(Combined, na.rm = TRUE))

combined <- rbind(Monthly_average_sub, combined) %>%
        mutate(color = ifelse(Date < as.Date("2017-12-01"), "In-person", "In-person + Online"))

yearly_combined <- Weekly_total %>%
        group_by(Date = floor_date(Date, "year")) %>%
        summarize(attendance = mean(Combined, na.rm = TRUE))

yearly_in_person <- Weekly_total %>%
        group_by(Date = floor_date(Date, "year")) %>%
        summarize(attendance = mean(total.attendance, na.rm = TRUE))

yearly_online <- Weekly_total %>%
        group_by(Date = floor_date(Date, "year")) %>%
        summarize(attendance = mean(Online, na.rm = TRUE))

latest <- matrix(c(tail(Weekly_total$Combined, 1), tail(Weekly_total$total.attendance, 1), tail(Weekly_total$Online, 1), tail(combined$attendance, 1), tail(in_person$attendance, 1), tail(online$attendance, 1), tail(combined$attendance, 2)[1], tail(in_person$attendance, 2)[1], tail(online$attendance, 2)[1], tail(yearly_combined$attendance, 1), tail(yearly_in_person$attendance, 1), tail(yearly_online$attendance, 1), tail(yearly_combined$attendance, 2)[1], tail(yearly_in_person$attendance, 2)[1], tail(yearly_online$attendance, 2)[1]), ncol = 5, byrow = FALSE)
colnames(latest) <- c("Last week", "Current Month-to-date", "Previous Month Average", "Current Year-to-date", "Previous Year Average")
rownames(latest) <- c("Total", "In person", "Online")

# Calculations for the Ascent tab

Ascent <- Weekly_total %>%
        select(Date, First, FirstServe24)
Ascent$First <- as.numeric(Ascent$First)
Ascent$FirstServe24 <- as.numeric(Ascent$FirstServe24)
Ascent$total <- with(Ascent,
                     ifelse(is.na(First) & is.na(FirstServe24), NA,
                            ifelse(is.na(First), NA,
                                   ifelse(is.na(FirstServe24), First + 0,
                                          First + FirstServe24
                                          )
                                   )
                            )
)

Ascent_monthly_average <- Ascent %>%
        group_by(month = floor_date(Date, "month")) %>%
        summarize(in.person = mean(First, na.rm = TRUE), online = mean(FirstServe24, na.rm = TRUE), total = mean(total, na.rm = TRUE))

Ascent_yearly_average <- Ascent %>%
        group_by(year = floor_date(Date, "year")) %>%
        summarize(in.person = mean(First, na.rm = TRUE), online = mean(FirstServe24, na.rm = TRUE), total = mean(total, na.rm = TRUE))

Ascent_overview_table <- matrix(c(tail(Ascent$total, 1), tail(Ascent$First, 1), tail(Ascent$FirstServe24, 1), tail(Ascent_monthly_average$total, 1), tail(Ascent_monthly_average$in.person, 1), tail(Ascent_monthly_average$online, 1), tail(Ascent_monthly_average$total, 2)[1], tail(Ascent_monthly_average$in.person, 2)[1], tail(Ascent_monthly_average$online, 2)[1], tail(Ascent_yearly_average$total, 1), tail(Ascent_yearly_average$in.person, 1), tail(Ascent_yearly_average$online, 1), tail(Ascent_yearly_average$total, 2)[1], tail(Ascent_yearly_average$in.person, 2)[1], tail(Ascent_yearly_average$online, 2)[1]), ncol = 5, byrow = FALSE)
colnames(Ascent_overview_table) <- c("Last week", "Current Month-to-date", "Previous Month Average", "Current Year-to-date", "Previous Year Average")
rownames(Ascent_overview_table) <- c("Total", "In person", "Online")

Ascent_in_person <- data.frame(Date = Ascent_monthly_average$month, attendance = Ascent_monthly_average$in.person)
Ascent_online <- data.frame(Date = Ascent_monthly_average$month, attendance = Ascent_monthly_average$online)
Ascent_total <- data.frame(Date = Ascent_monthly_average$month, attendance = Ascent_monthly_average$total)

# Calculations for the Sanctuary tab

Sanctuary <- Weekly_total %>%
        select(Date, Second, SecondServe24)
Sanctuary$Second <- as.numeric(Sanctuary$Second)
Sanctuary$total <- with(Sanctuary,
                     ifelse(is.na(Second) & is.na(SecondServe24), NA,
                            ifelse(is.na(Second), NA,
                                   ifelse(is.na(SecondServe24), Second + 0,
                                          Second + SecondServe24
                                   )
                            )
                     )
)

Sanctuary_monthly_average <- Sanctuary %>%
        group_by(month = floor_date(Date, "month")) %>%
        summarize(in.person = mean(Second, na.rm = TRUE), online = mean(SecondServe24, na.rm = TRUE), total = mean(total, na.rm = TRUE))

Sanctuary_yearly_average <- Sanctuary %>%
        group_by(year = floor_date(Date, "year")) %>%
        summarize(in.person = mean(Second, na.rm = TRUE), online = mean(SecondServe24, na.rm = TRUE), total = mean(total, na.rm = TRUE))

Sanctuary_overview_table <- matrix(c(tail(Sanctuary$total, 1), tail(Sanctuary$Second, 1), tail(Sanctuary$SecondServe24, 1), tail(Sanctuary_monthly_average$total, 1), tail(Sanctuary_monthly_average$in.person, 1), tail(Sanctuary_monthly_average$online, 1), tail(Sanctuary_monthly_average$total, 2)[1], tail(Sanctuary_monthly_average$in.person, 2)[1], tail(Sanctuary_monthly_average$online, 2)[1], tail(Sanctuary_yearly_average$total, 1), tail(Sanctuary_yearly_average$in.person, 1), tail(Sanctuary_yearly_average$online, 1), tail(Sanctuary_yearly_average$total, 2)[1], tail(Sanctuary_yearly_average$in.person, 2)[1], tail(Sanctuary_yearly_average$online, 2)[1]), ncol = 5, byrow = FALSE)
colnames(Sanctuary_overview_table) <- c("Last week", "Current Month-to-date", "Previous Month Average", "Current Year-to-date", "Previous Year Average")
rownames(Sanctuary_overview_table) <- c("Total", "In person", "Online")

Sanctuary_in_person <- data.frame(Date = Sanctuary_monthly_average$month, attendance = Sanctuary_monthly_average$in.person)
Sanctuary_online <- data.frame(Date = Sanctuary_monthly_average$month, attendance = Sanctuary_monthly_average$online)
Sanctuary_total <- data.frame(Date = Sanctuary_monthly_average$month, attendance = Sanctuary_monthly_average$total)


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
                                        choices = list("Combined" = "combined", "In Person" = "in_person", "Online" = "online"),
                                        selected = combined),
                            p("There are three data sets to choose from: In-person attendance, online audience, and combined in-person + online. The default view is the combined data set.")    
                    ),
                    mainPanel(
                            h3("By the Numbers"),
                            h4("Current Attendance per week"),
                            tableOutput("latest"),
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
                                        choices = list("Total" = "Ascent_total", "In Person" = "Ascent_in_person", "Online" = "Ascent_online"),
                                        selected = combined),
                            p("There are three data sets to choose from: In-person attendance, online audience, and total attendance. The default view is total attendance.")
                    ),
                    mainPanel(
                            h3("By the Numbers"),
                            h4("Current Attendance per week"),
                            tableOutput("Ascent_overview_table"),
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
                                        choices = list("Total" = "Sanctuary_total", "In Person" = "Sanctuary_in_person", "Online" = "Sanctuary_online"),
                                        selected = combined),
                            p("There are three data sets to choose from: In-person attendance, online audience, and total attendance. The default view is total attendance.")
                    ),
                    mainPanel(
                            h3("By the Numbers"),
                            h4("Current Attendance per week"),
                            tableOutput("Sanctuary_overview_table"),
                            h3("Attendance Over Time"),
                            plotlyOutput("Sanctuary_plot")
                    )
                    
            )
    )
    ),
    hr(),
    h4("Created by: Jim Milks"),
    "Version 1: 03 November 2021",
    br(),
    "Version 2: 17 March 2022",
    br(),
    "Data updated: 19 April 2022"
)

# Define server logic required to render the table and time series plot
server <- function(input, output) {
        output$latest <- renderTable(latest, align = "c", rownames = TRUE)
        
        data_reactive <- reactive({
                if (input$dataset == "in_person")
                        in_person
                else if (input$dataset == "online")
                        online
                else
                        combined
        })
        
        output$p <- renderPlotly({ 
                data_set = data_reactive()
                
                ggplot(data = data_set, aes(x = Date, y = attendance)) +
                        geom_point() +
                        geom_line() +
                        geom_smooth(method = "loess", formula = "y~x", col = "red") +
                        labs(title = "Weekly average attendance by month",
                             x = "Date",
                             y = "Average attendance")
        })        


        output$Ascent_overview_table <- renderTable(Ascent_overview_table, align = "c", rownames = TRUE)
        
        Ascent_data_reactive <- reactive({
                if (input$Ascent_dataset == "Ascent_in_person")
                        Ascent_in_person
                else if (input$Ascent_dataset == "Ascent_online")
                        Ascent_online
                else
                        Ascent_total
        })
        
        output$Ascent_plot <- renderPlotly({ 
            Ascent_data_set = Ascent_data_reactive()
            
            ggplot(data = Ascent_data_set, aes(x = Date, y = attendance)) +
                    geom_point() +
                    geom_line() +
                    geom_smooth(method = "loess", formula = "y~x", col = "red") +
                    labs(title = "Weekly average attendance by month",
                         x = "Date",
                         y = "Average attendance")
    })
        output$Sanctuary_overview_table <- renderTable(Sanctuary_overview_table, align = "c", rownames = TRUE)
        
        Sanctuary_data_reactive <- reactive({
                if (input$Sanctuary_dataset == "Sanctuary_in_person")
                        Sanctuary_in_person
                else if (input$Sanctuary_dataset == "Sanctuary_online")
                        Sanctuary_online
                else
                        Sanctuary_total
        })
        
        output$Sanctuary_plot <- renderPlotly({ 
                Sanctuary_data_set = Sanctuary_data_reactive()
                
                ggplot(data = Sanctuary_data_set, aes(x = Date, y = attendance)) +
                        geom_point() +
                        geom_line() +
                        geom_smooth(method = "loess", formula = "y~x", col = "red") +
                        labs(title = "Weekly average attendance by month",
                             x = "Date",
                             y = "Average attendance")
        })  
}

shinyApp(ui = ui, server = server)
