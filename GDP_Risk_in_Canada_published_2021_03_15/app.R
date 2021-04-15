#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#






library(shiny)
source("functions_server_v2.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Canada’s GDP at Risk"),
   
   print("Sample text"),
   
   # print(dates_start),
   # print(dates_end),

   
   # Date Input.
   dateRangeInput("dateRange", "Date range:",
     start = "2001-01-01",
     end = "2020-04-01",
     min = "2001-01-01",
     max = "2020-04-01",
     format = "yyyy-mm-dd",
     startview = "month",
     weekstart = 0,
     language = "en",
     separator = " to ",
     width = NULL,
     autoclose = TRUE
   ),
   
   # Show a plot of the generated distribution
   mainPanel(
      plotOutput("densityPlot")
   )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$densityPlot <- renderPlot({
      
      density_plots(input$dateRange[1], input$dateRange[2] )
      
   })
   
   
   output$dateRangeText  <- renderText({
      paste("input$dateRange is", 
            paste(as.character(input$dateRange), collapse = " to ")
      )
   })
   

}

# Run the application 
shinyApp(ui = ui, server = server)


# print(as.character(Input$dateRange[1]))
# print(as.character(Input$dateRange[2]))
