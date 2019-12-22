# Homework #3 Shiny app
# Due November 1, 2019

library(shiny)
library(tidyverse)
library(forcats)
library(dslabs)
library(directlabels)
data(gapminder)

options(width=250) # Keep the columns of the interactive
                   # data frame unwrapped

# Define UI
ui <- fluidPage(
  
  # Change theme to darkly
  theme=shinythemes::shinytheme("darkly"),
  
  # Application title
  titlePanel(strong("ARE WE LIVING LONGER?")),
  
  # Create two tabs
  tabsetPanel(
    
    # First tab
    tabPanel("Life expectancy over the years",
             
             # Use sidebar layout
             sidebarLayout(
               
               # Create sidebar
               sidebarPanel(
                 
               # Brief introduction
               p("Let's take a look at what happens to the life
               expectancy across the world between 1960 and 2016.
                 We use datasets from the",
                 a("Gapminder Foundation",
                   href="http://www.gapminder.org/"),
                   "to examine the trends in each country's life
                   expectancy during this time period. To get started,
                   pick a country from the dropdown menu below. By
                   default, we show you the life expectancy trend of
                   the United States. How is the human race doing?
                 Let's find out."),
               
               # Line break
               br(),
               
               # Dropdown menu to pick a country
               selectInput(inputId="country",
                            label="Choose a country",
                            choice=gapminder$country #,
                            # selected="United States",
                            # selectize=FALSE
                           )
               ),
               
               # Create main panel
               mainPanel(
                 
                 # Line plot
                 plotOutput(outputId="linePlot")
                 )
               )
             ),
    
    # Second tab
    tabPanel("Life expectancy vs. fertility rate",
             
             # Sidebar layout
             sidebarLayout(
               
               # Create sidebar
               sidebarPanel(
                 
               # Brief introduction
               p("Now let's take a look at the relationship
               between life expectancy and fertility, defined
               as the average number of children per woman.
               Thanks to the advent of birth control, women
               worldwide now have the choice to have fewer
               children. Indeed, this seems to be the trend
               over the decades. Here, we visualize the life
               expectancy across the world between 1960 and 2015
               against fertility. To get started, pick a year by
               moving the pointer on the slider below, or you can
               click on the", em("PLAY"), "button to save yourself
               some legwork. By default, we show you the results for 2000.
               How does fertility play into the life expectancy of the
                 masses? Let's find out."),
               
               # Line break
               br(),
               
               # Slider to pick a year
               sliderInput(inputId="year",
                           label="Choose a year",
                           value=2000, min=1960, max=2015, # Default at year 2000
                           step=1, # Show only integer values
                           sep="", # Remove commas
                           ticks=FALSE, # Remove all years in the middle
                           animate=TRUE # Add a PLAY button
                           )
               ),
               
               # Create main panel
               mainPanel(
                 
                 # Scatterplot
                 plotOutput(outputId="pointPlot",
                            click="plot_click"
                            ),
                 
                 # Interactive data frame
                 h4("Click anywhere on the plot to view life expectancy at a glance"),
                 verbatimTextOutput("click_info")
                 )
               )
             )
    )
  )

# Define server
server <- function(input, output) {
  
  # Create reactive expression for country
  country <- reactive(gapminder %>%
                        filter(country==input$country))
  
  # Data for United States only
  us <- gapminder %>%
    filter(country=="United States")
  
  # Create line plot
  output$linePlot <- renderPlot({
    country() %>%
      ggplot(aes(x=year, y=life_expectancy)) + # Line for user-inputted country
      geom_line(color="blue") +
      geom_dl(label=input$country, method=list("last.bumpup", cex=0.7),
              color=ifelse(input$country=="United States", "red", "blue")) +
      geom_line(aes(x=year, y=life_expectancy), data=us,
                color="red") + # Line for United States
      xlab("Year") +
      ylab("Life expectancy (years)") +
      scale_x_continuous(breaks=seq(1960, 2016,4), limits=c(1960, 2020), # Fix the x-axis 
                         expand=expand_scale(mult=c(0, 0.08), # Expand right side to fit labels
                                             add=c(0.5, 0))) + # Longest label is St. Vincent and the Grenadines
      scale_y_continuous(breaks=seq(10, 85, 5), limits=c(10, 85)) + # Fix the y-axis
      ggtitle(
        ifelse(input$country %in% c("", "United States"),
               "Life expectancy from 1960 to 2016 for United States",
               sprintf("Life expectancy from 1960 to 2016 for %s compared to United States", input$country))
        ) + # Default title is United States; for user-inputted countries, compare to the United States
      theme_bw()
      
  })
  
  # Create reactive expression for year
  year <- reactive(gapminder %>%
                     filter(year==input$year))
  
  # Create scatterplot
  output$pointPlot <- renderPlot({
    year() %>%
      ggplot(aes(x=fertility, y=life_expectancy, color=continent)) + # Color-code by continent
      geom_point() +
      xlab("Fertility (average number of children per woman)") +
      ylab("Life expectancy (years)") +
      scale_x_continuous(breaks=seq(0, 9), limits=c(1, 8)) + # Fix the x-axis
      scale_y_continuous(breaks=seq(10, 85, 5), limits=c(10, 85)) + # Fix the y-axis
      scale_color_discrete(name="Continent") + # Change legend title
      ggtitle(sprintf("Life expectancy vs. fertility in %d", input$year)) + # Interactive title according to user-inputted year
      theme_bw()
  })
  
  # Create interactive data frame
  info <- gapminder %>%
    select(life_expectancy, fertility, country, continent)
  
  # Each click gives an interactive data frame
  output$click_info <- renderPrint({
    nearPoints(info, input$plot_click, maxpoints=7) %>% # Only output 7 rows of data frame
      arrange(desc(life_expectancy)) # Arrange by decreasing order in life expectancy
  },
  width=getOption("width") # Keep the columns unwrapped
  )
}

# Run the application
shinyApp(ui = ui, server= server)