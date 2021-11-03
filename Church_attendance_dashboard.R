#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(tidyverse)
library(lubridate)
library(shiny)
library(timetk)
library(plotly)
library(knitr)

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
                              ifelse(is.na(attendance) & is.na(Online), NA,
                                     ifelse(is.na(attendance), 0 + Online,
                                            ifelse(is.na(Online), attendance + 0,
                                                   attendance + Online
                                            )
                                     )
                              )
)

Average_by_month <- Weekly_total %>% 
        group_by(month = floor_date(Date, "month")) %>% 
        summarize(attendance = mean(attendance, na.rm = TRUE))
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
        summarize(attendance = mean(attendance, na.rm = TRUE))

yearly_online <- Weekly_total %>%
        group_by(Date = floor_date(Date, "year")) %>%
        summarize(attendance = mean(Online, na.rm = TRUE))

latest <- matrix(c(tail(Weekly_total$Combined, 1), tail(Weekly_total$attendance, 1), tail(Weekly_total$Online, 1), tail(combined$attendance, 1), tail(in_person$attendance, 1), tail(online$attendance, 1), tail(yearly_combined$attendance, 1), tail(yearly_in_person$attendance, 1), tail(yearly_online$attendance, 1)), ncol = 3, byrow = FALSE)
colnames(latest) <- c("Last week", "Average Month-to-date", "Average Year-to-date")
rownames(latest) <- c("Total", "In person", "Online")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Kettering SDA Church Attendance"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
                selectInput("dataset", label = h3("Attendance data set"),
                            choices = list("Combined" = "combined", "In Person" = "in_person", "Online" = "online"),
                            selected = combined),
                p("There are three data sets to choose from: In-person attendance, online audience, and combined in-person + online. The default view is the combined data set.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
                h3("By the Numbers"),
                h4("Current Attendance"),
                tableOutput("latest"),
                h3("Attendance Over Time"),
                plotlyOutput("p"),
                hr(),
                h4("Created by: Jim Milks"),
                "3 November 2021"
        )
    )
)

# Define server logic required to draw a histogram
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
}

# Run the application 
shinyApp(ui = ui, server = server)
