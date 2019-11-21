#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Portland, OR tab still needs work.
# First tab is close to done but the second tab needs to include the permit data.
# Plus it isn't aesthetically pleasing as of yet.

# Turned off the use of scientific notation.

options(scipen = 999)

# Loaded in all the libraries the app will need to run.

library(shiny)
library(shinyWidgets)
library(fs)
library(shinythemes)
library(moderndive)
library(gt)
library(tidyverse)

# Read in the Zillow cities data.

cities <- read_csv("cities.csv")

# Created a vector of the city names that will be used in the pickerInput.

citiesVector <- as.vector(cities$RegionName)

# Read in the Zillow neighborhood data.

neighborhoods <- read_csv("neighborhoods.csv")

# Created a vector of neighborhood names that will be used in the pickerInput.

neighborhoodsVector <- as.vector(neighborhoods$RegionName)

# Define UI for application.
# Set theme 
# Titled it United States Housing Market

ui <- navbarPage("United States Housing Market", theme = shinytheme("flatly"),
   
    # Created multiple tabs in the app
    # First one is titled National Overview
    
    tabPanel("National Overview", 
     
     # Tab title
     
     titlePanel("Current National Overview"),
     
     # Sidebar with a pickerInput to select the cities to display in the graph.
     
     sidebarLayout(
        sidebarPanel(
           pickerInput("selectCities",
                       "Select Cities",
                       choices = citiesVector,
                       options = list("actions-box" = TRUE),
                       multiple = TRUE
                      )
        ),
        
        # Main panel
        # Created two tabs, one for the plot and one for the model.
        
        mainPanel(
          tabsetPanel(id = "tabsMain",
              tabPanel("Plot",
                       plotOutput("plot1")
                       ),
              tabPanel("Model",
                       gt_output("model")
                       )
          )
        )
     )
   ),
   
   # Second tab
   # Titled Portland, OR
   
   tabPanel("Portland, OR",
     titlePanel("Neighborhood Trends Over Time in Portland, Oregon"),
     
     # Similar setup to above with a sidebar panel that has a pickerInput
     
     sidebarLayout(
       sidebarPanel(
         pickerInput("selectNeighborhoods",
                     "Select Neighborhoods",
                     choices = neighborhoodsVector,
                     options = list("actions-box" = TRUE),
                     multiple = TRUE
                     #selected = neighborhoodsVector
                     )
       ),
       
       # Main panel 
       # Two tabs - one for the plot and one for the model.
       
     mainPanel(
       tabsetPanel(id = "tabsPortland",
                   tabPanel("Plot",
                            plotOutput("plot2")
                            ),
                   tabPanel("Model"
                            ))
       
     )
    ) 
  ),
   
  # About panel.
  
   tabPanel("About",
    titlePanel("About"),
    
    br(),
    
    p(paste("My aim, in the final project, is to examine the national housing market with a special focus on Portland, Oregon. During the summer after my sophomore year, I spent time working with a company that builds accessory dwelling units in Portland. One of the aims of the company was to help solve the housing stock shortage within the city. As a city in high demand and zoned primarily for single family homes, Portland, like many large American cities, is facing a housing crisis. My hope for this project is to use available data to learn more about how the housing market in Portland has changed over recent years and how that has affected the affordability and accessibility of housing in the metropolitan area.")),
    
    br(),
    
    p(paste("The data I am planning on using for the project is from two main sources. I have data from Zillow to show national trends in urban and suburban housing and rental markets. I also have annual data from the Census Bureau, that gives me more localized data on the frequency with which building permits are issued in the Portland market. This data allows me to analyze both the prices/costs as well as the types of housing that make up the Portland market and how they have changed over the past several decades.")),
    
    br(),
    
    p(paste("I, Pieter Quinton, am a senior at Harvard College studying Government with a secondary focus in Economics. You can access the source code for the project at https://github.com/PGQuinton/gov1005-final-project.")),
    
    br()
    
    )
  )


# Define server logic required to draw plots and models.

server <- function(input, output) {
   
  # First plot.
  # Simple ggplot.
  # Subset the data to just include the cities selected in the pickerInput using a filter funciton.
  
   output$plot1 <- renderPlot({
     
     plot1 <- cities %>%
       filter(RegionName %in% input$selectCities) %>%
       ggplot(aes(x=SizeRank, y=X2019.08)) +
         geom_jitter(alpha = 0.75) +
         scale_y_log10(lim = c(50000, 1000000)) + 
         scale_x_reverse(lim =  c(805, 0)) +
         geom_smooth(method = "lm", se = FALSE) +
         labs(title = "Relationship Between Population and Median House Price",
              subtitle = "Data from the 804 largest cities in the United States as of August, 2019",
              y = "Median House Price ($)",
              x = "Population Ranked",
              caption = "As the plot shows, there is a positive relationship between population and median house price.\nThe scale on the x-axis has been flipped so the most populous cities are on the right and the least populous cities are on the left. \nThe scale on the y-axis has been logged so as to better illustrate the relationship between the two variables. \nSource: Zillow") +
         theme(
           plot.caption = element_text(hjust = 0)
         )
     plot1
    
   })
   
   # Model
   # Used render_gt() so my gt table shows up.
   # Again, subset the data to just include the cities selected.
   
   output$model <- render_gt({
     
     model_data <- cities %>%
       filter(RegionName %in% input$selectCities)
     
     
     
     if(model_data$RegionName > 2){
     
       model <- get_regression_table(lm(X2019.08 ~ rowname, data = model_data))
       
       model %>%
         select(term, estimate, lower_ci, upper_ci) %>%
         mutate(term = c("Intercept", "Population Ranked")) %>%
         gt() %>%
           cols_label(
             term = "",
             estimate = "Coefficient",
             lower_ci = "5th percentile",
             upper_ci = "95th percentile"
           ) %>%
           fmt_number(
             columns = 2:4,
             decimals = 2
           ) %>%
           tab_header(
             title = "City Population and Median Home Price",
             subtitle = "Median single family home prices rise as the city population increases."
           ) %>%
           tab_source_note(
             source_note = "Data from Zillow."
           )
       
       p("Select at least 3 cities from the drop-down menu on the left.")
       
     } else{
       p("Select at least 3 cities from the drop-down menu on the left.")
        }
   })
   
   # Second plot
   # Basically the same as above.
   
   output$plot2 <- renderPlot({
     
    neighborhoods %>%
       filter(RegionName %in% input$selectNeighborhoods) %>%
       group_by(RegionName) %>%
       ggplot(aes(x = as.POSIXct(date), y = median, group = RegionName, color = RegionName)) +
         geom_point() +
         theme(
           legend.position = "none"
         ) +
         scale_y_continuous(lim = c(100000, 900000))
   })
}

# Run the application 

shinyApp(ui = ui, server = server)

