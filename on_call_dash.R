## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##   Shiny App for Keeping Track of On Call Dashboard                                        ##
##   Written by Daniel Mulder, April 2022                                                    ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

## Preamble
# This script does not constitute medical advice and is only to be used for the purposes of learning or preparing personal templates
# This script contains no real medical information, any information contained within is fictional example information

# Load required packages ----
library(shiny) # for interactive web application framework
library(tidyverse) # for basic data organization
library(lubridate) # for creating/parsing date objects
library(calendR) # to create calendar objects (based on ggplot)

# at this point, requires pre-written .csv file that is a list of dates in YYYY-MM-DD format below the column title "Date"
shifts <- read_csv("on_call_shifts.csv",
                   col_names = TRUE,
                   cols(Date = col_date(format = "")))

shifts_char <- as.character(shifts$Date)

shifts_day_of_month <- mday(as.POSIXlt(shifts_char, format="%Y-%m-%d"))

shifts_count_for_month <- length(shifts$Date)

ui <- fluidPage(
  mainPanel(
    br(),
    tags$b(h1(strong("On Call Dashboard"), style = "font-size:30px;")),
    "(goal: ensure I am providing at least 1 in 4 coverage as is mandatory)",
    br(),
    
    # enter in the date I'm on call
    dateInput("date_1", label = h5("First On Call")),
    actionButton("shift_1", "Save Date as On Call", icon("calendar")),
    
    # list of calls
    tableOutput("on_call_table"),
    
    plotOutput("calendar_this_month"),
    
    textOutput("coverage_for_month"),
    
    textOutput("coverage_conclusion"),
    
    # output a calendar showing the dates I am on duty highlighted
    
    # output the proportion of the month I am on duty (and highlight if at least 25%) "You ARE covering 1 in 4 this month"
   
    # output the proportion of the three month block I am on duty (and highlight if at least 25%) "You ARE covering 1 in 4 for this 3 month block"  

  )
)

server <- function(input, output, session) {
  
  observeEvent(input$shift_1, {
    shifts2 <- rbind(shifts, as.character(input$date_1))
    converted_data <- shifts2$Date
    write_csv(shifts2, file = paste0("/Clinical/Other/on_call_shifts.csv"))
    })
  
  output$on_call_table <- renderTable(shifts_char)
  
  # this month
  output$calendar_this_month <- renderPlot(calendR(year = 2022,
                                 month = month(today()),
                                 special.days = c(shifts_day_of_month),
                                 special.col = "lightblue"))
  
  # calculating proportion of the month I am on duty (and highlight if at least 25%)

  output$coverage_for_month <- reactive({
    paste("Covering 1 in", round(days_in_month(today())/shifts_count_for_month, 2), "days this month.")
    })
  
  output$coverage_conclusion <- renderText(if (days_in_month(today())/shifts_count_for_month > 4.0) {
    paste("This is not enough coverage.")
  } else {
    paste("This is sufficient coverage.")
  })
  
  # reactive function to take dates input above and turn them into a calendar graphic

  
  # calculator for proportion of the three month block I am on duty (and highlight if at least 25%)
  
}
  
shinyApp(ui, server)
