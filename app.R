
library(shiny)

# Create the ui and server objects

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Sampling from a normal distribution"),

  # Sidebar layout with input and output definitions
  sidebarLayout(position='right',

    # Sidebar panel for inputs ----
    sidebarPanel(
    width=4,
	  sliderInput(inputId = 'ss', label = 'Sample size', min = 5, max = 500, value = 20, round = T) #,
	  # br(),
	  # br(),
      # dataTableOutput('table')

    ),

    # Main panel for displaying outputs
    mainPanel(

	  h5("Collect data on people's height from a large population."),
	  h5("Each time you press the button, a new sample is drawn. You will see the population distribution, followed by a histogram of your sample data below. The red vertical lines show the mean for the whole population and your sample. Use the slider to the right to explore what happens when you change your sample size."),
	  # br(),
	  # br(),
	  
	  actionButton("run", label = "Draw a sample"),
	  br(),
	  br(),
	  br(),


      # Histogram
      plotOutput(outputId = "histplot1", width="30%", height="30%"),
      br(),
      br(),
      plotOutput(outputId = "histplot2", width="30%", height="30%"),
 
	  # Calculate the mean
      # verbatimTextOutput("mycheck"), 
      h4(textOutput(outputId = "mean_res", strong))
      

    )
  )
)




# Define server logic required to draw a histogram ----
server <- function(input, output) {
	
	mu <<- sample(65:75, size=1)
	x <<- (rnorm(100000, mean=mu, sd=4))
	
	xsamp <- reactive({
		round(x[sample(length(x), input$ss)])
		
	})
    
   	output$histplot1 <- renderPlot({
  		par(mar=c(4,6,2,0.2), cex=1)
  		hist(x, col = rgb(0,0,0.5,0.25), las=1, xlim=c(mean(x)-10, mean(x)+10), freq=F,
        		 xlab = "Height (inches)",
        		 ylab = "Probability density",
        		 main = "Population", breaks=10000)
        abline(v=mean(x), lwd=4, col='red')
	
    }, height=200, width=400)
    	
    output$table <- renderDataTable(data.frame('Height (inches) in sample'=xsamp()))
    	
	
  	observeEvent(input$run, {
		xsamp <- reactive({
			round(x[sample(length(x), input$ss)])
		})   	
  		output$histplot2 <- renderPlot({
				par(mar=c(4,6,2,0.2), cex=1)
    			hist(xsamp(), col = rgb(0,0,0.5,0.5), las=1, xlim=c(mean(x)-10, mean(x)+10),
        			 xlab = "Height (inches)",
        			 ylab = "Frequency\n(# of people in sample)",
        		 	main = "Sample")
        		 abline(v=mean(xsamp()), lwd=4, col='red')
	
	    		}, height=200, width=400)
    	
    		output$table <- renderDataTable(data.frame('Height (inches) in sample'=xsamp()))
    	
    	})
    	
}

# 

shinyApp(ui = ui, server = server) # end with a call to the shinyApp function



