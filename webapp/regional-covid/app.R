#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    #sidebarLayout(
        # sidebarPanel(
        #     sliderInput("bins",
        #                 "Number of bins:",
        #                 min = as.Date("2020-07-02"),
        #                 max = as.Date("2020-12-06"),
        #                 value = as.Date("2020-12-06"))
        # ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           sliderInput("date",
                       "Date:",
                       min = as.Date("2020-07-02"),
                       max = as.Date("2020-12-06"),
                       value = as.Date("2020-12-06"))
        )
    #)
)

source("~/Git/uk-covid-datatools/vignettes/in-development/map-playground.R")

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        ggplot(ccgMap3 %>% filter(subgroup=="self care" & date == input$date),aes(fill=Prob))+geom_sf()+scale_fill_viridis_d(drop=FALSE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
