#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- navbarPage("United States Housing Market", theme = shinytheme("flatly"),
   tabPanel("National Overview", 
     
     # Application title
     titlePanel("Current National Overview"),
     
     # Sidebar with a slider input for number of bins 
     sidebarLayout(
        sidebarPanel(
           pickerInput("selectCities",
                       "Select Cities",
                       choices = citiesVector,
                       options = list("actions-box" = TRUE),
                       multiple = TRUE,
                       selected = "all")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot1")
        )
     )
   ),
   
   tabPanel("About",
    titlePanel("About"),
    
    br(),
    
    p(paste("My aim, in the final project, is to examine the housing market of Portland, Oregon. During the summer after my sophomore year, I spent time working with a company that builds accessory dwelling units in Portland. One of the aims of the company was to help solve the housing stock shortage within the city. As a city in high demand and zoned primarily for single family homes, Portland, like many large American cities, is facing a housing crisis. My hope for this project is to use available data to learn more about how the housing market in Portland has changed over recent years and how that has affected the affordability and accessibility of housing in the metropolitan area.")),
    
    br(),
    
    p(paste("The data I am planning on using for the project is from two main sources. I have data from Zillow to show national trends in urban and suburban housing and rental markets. I also have annual data from the American Community Survey, conducted by the Census Bureau, that gives me more localized data on the Portland market. This data allows me to analyze both the prices/costs as well as the types of housing that make up the Portland market and how they have changed over the past decade or so.")),
    
    br(),
    
    p(paste("I, Pieter Quinton, am a senior at Harvard College studying Government with a secondary focus in Economics. ")),
    
    br()
    
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$plot1 <- renderPlot({
     
     plot1 <- zillow_data %>%
       filter(RegionName %in% input$selectCities) %>%
       ggplot(aes(x=X2019.08, y=SizeRank)) +
         geom_jitter(alpha = 0.75) +
         scale_x_log10(lim = c(50000, 1000000)) + 
         scale_y_reverse(lim =  c(805, 0)) +
         geom_smooth(method = "lm", se = FALSE) +
         labs(title = "Relationship Between Population and Median House Price",
              subtitle = "Data from the 805 largest cities in the United States as of July, 2019",
              x = "Median House Price ($)",
              y = "Population Ranked",
              caption = "As the plot shows, there is a positive relationship between population and median house price.\nThe scale on the y-axis has been flipped so the most populous cities are at the top and the least populous cities are at the bottom.") +
         theme(
           plot.caption = element_text(hjust = 0)
         )
     plot1
    
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

