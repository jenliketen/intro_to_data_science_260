
library(shiny)

#install.packages("devtools")
library(devtools)
devtools::install_github("dkahle/ggmap")
library(ggmap)
register_google(key="AIzaSyDCEiYC7LHLq3h7BtpMCXNDgUMtBvveUgg")

#loc = c(left = -74.7, bottom = 40.55, right = -73.2, top = 40.9)
nyc = get_map(location = "new york", source = "google", maptype = "terrain", zoom = 11)
map = ggmap(nyc)

ui <- fluidPage(
    titlePanel("Airbnb in New York City"),
    tabsetPanel(
        tabPanel("Find What I Want",
                 fluidRow(
                     column(3,
                            # Input
                            checkboxGroupInput(inputId = "room_type",
                                               label = "Room Type:",
                                               choices = as.list(levels(data$room_type)),
                                               selected=unlist(data$room_type)
                            ),
                            
                            checkboxGroupInput(inputId = "neigh",
                                               label = "Neighborhood:",
                                               choices = as.list(levels(data$neighbourhood_group)),
                                               selected=unlist(data$neighbourhood_group)
                                               
                            ),
                            
                            sliderInput(inputId = "price",
                                        label = "Price Rnage:",
                                        min = 0, 
                                        max = 1000,
                                        value = c(0,1000), 
                                        step = 1,
                                        ticks = FALSE  # don't show tick marks on slider bar
                            ),
                            sliderInput(inputId = "min_night",
                                        label = "Minimum Nights of Stay:",
                                        min = 0, 
                                        max = 365,
                                        value = 3, 
                                        step = 1,
                                        ticks = FALSE  # don't show tick marks on slider bar
                            )
                     ),
                     #output
                     column(9,
                            plotOutput(outputId = "plot1", height=500,
                                       click = "plot1_click")
                     ),
                     fluidRow(
                         column(12,
                                verbatimTextOutput("click_info")
                         )
                     )
                 )),
        
        tabPanel("How Much Should I Pay",    
                 sidebarLayout(
                     sidebarPanel(
                         p("Through forward model selection via linear regression, we have found 5 contributing factors to NYC Airbnb prices: 
                   Neighborhood (group), Availability, Min Nights, Host Listing, and Room type.
                   You can choose Availability, Min Nights, Host Listing, and Room type from below as you like to see how price 
                   changes under different factors. 
                   Note that the blue dot is our expected price, and the boxplot shows prices across different
                   significant neighborhoods in NYC"),
                         
                         # Dropdown menu that allows the user to choose a room_type since it is categorical variable
                         selectInput("room_type", label = "choose a room type", #here room_type is the inputID
                                     choices = data$room_type),
                         
                         #slider bar for availability
                         sliderInput("availability", "Availability throughout the year:", #inputID and label
                                     min=min(data$availability_365), #goes from the min so that the user selection is meaningful
                                     max=max(data$availability_365), #goes up to the max so that the user selection is meaningful
                                     value=median(data$availability_365), # Default is the median
                                     step=1, # Show only integer values
                                     sep="", # keep variable in desired digital format and not 1,960 format i.e. remove commas
                                     ticks=FALSE, # don't show tick marks on slider bar
                                     animate=TRUE), # add play button to animate
                         
                         #slider bar for min nights
                         sliderInput("min_nights", "Minimum Nights:", #inputID and label
                                     min=min(data$minimum_nights), 
                                     max=max(data$minimum_nights), 
                                     value=median(data$minimum_nights), # Default is median
                                     step=1, # Show only integer values
                                     sep="", # remove commas
                                     ticks=FALSE, # don't show tick marks on slider bar
                                     animate=TRUE), # add play button to animate
                         
                         #slider bar for host listing
                         sliderInput("host_listing", "Host Listing:", #inputID and label
                                     min=min(data$calculated_host_listings_count), 
                                     max=max(data$calculated_host_listings_count), 
                                     value=median(data$calculated_host_listings_count), # Default is median
                                     step=1, # Show only integer values
                                     sep="", # remove commas
                                     ticks=FALSE, # don't show tick marks on slider bar
                                     animate=TRUE) # add play button to animate
                     ),
                     
                     
                     mainPanel(
                         plotOutput("boxPlot1"), #here we want a boxplot, and boxPlot is outputID
                         tableOutput('table1'))
                 ))
    )
    
)     


server <- function(input, output) {
    output$plot1 <- renderPlot({
        mydata = data %>% filter(room_type == input$room_type) %>% filter(neighbourhood_group == input$neigh) %>% filter(price >= input$price[1] & price <= input$price[2]) %>% filter(minimum_nights >= input$min_night)
        map + geom_point(data= mydata, aes(x = longitude, y = latitude, colour = room_type), alpha=0.7) + coord_cartesian()
    })
    
    output$click_info <- renderPrint({
        mydat = data %>% filter(room_type == input$room_type) %>% filter(neighbourhood_group == input$neigh) %>% filter(price >= input$price[1] & price <= input$price[2]) %>% filter(minimum_nights >= input$min_night)
        c = nearPoints(mydat, input$plot1_click, maxpoints = 5, xvar = "longitude", yvar = "latitude")
        knitr::kable(c)
    })
    
    
    output$boxPlot1 = renderPlot({
        
        #y1 through y5 is our 5 expected prices for each neighborhood group
        #please alter coefficients based on regression results
        #also take care of room type
        
        y1 <- 0.2*input$availability+0.2*input$min_nights+0.2*input$host_listing+100
        y2 <- 0.2*input$availability+0.2*input$min_nights+0.2*input$host_listing+110
        y3 <- 0.2*input$availability+0.2*input$min_nights+0.2*input$host_listing+120
        y4 <- 0.2*input$availability+0.2*input$min_nights+0.2*input$host_listing+130
        y5 <- 0.2*input$availability+0.2*input$min_nights+0.2*input$host_listing+140
        
        ggplot(data = data, aes(x=neighbourhood_group, y=price)) + geom_boxplot() +
            #boxplot(price~neighbourhood_group, data=below_median) +
            scale_y_continuous(breaks=seq(100, 500, 100), limits=c(100, 500)) +
            xlab("Neighbourhood Groups") +
            ylab("Prices") +
            ggtitle("Price vs Neighbourhood Groups") +
            geom_point(aes(x=1, y=y1), colour="green") +
            geom_point(aes(x=2, y=y2), colour="green") +
            geom_point(aes(x=3, y=y3), colour="green") + 
            geom_point(aes(x=4, y=y4), colour="green") +
            geom_point(aes(x=5, y=y5), colour="green") 
    })
    
    output$table1 = renderTable({
        
        y1 <- 0.2*input$availability+0.2*input$min_nights+0.2*input$host_listing+100
        y2 <- 0.2*input$availability+0.2*input$min_nights+0.2*input$host_listing+110
        y3 <- 0.2*input$availability+0.2*input$min_nights+0.2*input$host_listing+120
        y4 <- 0.2*input$availability+0.2*input$min_nights+0.2*input$host_listing+130
        y5 <- 0.2*input$availability+0.2*input$min_nights+0.2*input$host_listing+140
        
        
        #y= data.frame('Bronx'=y1, 'Brooklyn'=y2,'Manhattan'=y3,'Queens'=y4,'Staten Island'=y5)
        y<-c('Expected Price',
             paste0('Bronx: ',y1),
             paste0('Brooklyn: ',y2),
             paste0('Manhattan: ',y3),
             paste0('Queens: ',y4),
             paste0('Staten Island: ',y5))
        
        # y=as.data.frame(y)
        # 
        # colnames(y) = c("expected price")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
