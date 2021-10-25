#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinysurveys)

KetSDA_demograpics <- data.frame(question = c("What is your age?", "What is your gender?", "What is your gender?", "Do you attend in person or online?", "Do you attend in person or online?", "Which service do you normally attend/watch?", "Which service do you normally attend/watch?"),
                                 option = c("", "Female", "Male", "In person", "Online", "Ascent", "Sanctuary"),
                                 input_type = c("numeric", "mc", "mc", "mc", "mc", "mc", "mc"),
                                 input_id = c("Age", "gender", "gender", "mode", "mode", "service", "service"),
                                 dependence = c(NA, NA, NA, NA, NA, NA, NA),
                                 dependence_value = c(NA, NA, NA, NA, NA, NA, NA),
                                 required = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
)

# Define UI for application that draws a histogram
ui <- fluidPage(
        surveyOutput(df = KetSDA_demograpics,
                     survey_title = "Kettering SDA Church demographics",
                     survey_description = "Welcome! We would like to know a bit more about our congregation. The information you provide will be used to create a population pyramid for our congregation to help us identify current and future needs. Please fill this form out for each member of your immediate household, reloading it each time. Thank you for participating!")
        
)

server <- function(input, output, session) {
        renderSurvey()
        
        observeEvent(input$submit, {
                showModal(modalDialog(
                        title = "Thank you!"
                ))
        })
}

shinyApp(ui, server)