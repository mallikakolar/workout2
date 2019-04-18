
library(shiny)
library(ggplot2)
library(reshape2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Investing Modality Variations"),
  
  fluidRow(
    column(3,
           sliderInput("Initial", "Initial Amount",
                       min = 0, max = 100000,
                       value = 1000,
                       step = 500),
           sliderInput("Annual", "Annual Contribution",
                       min = 0, max = 50000,
                       value = 2000,
                       step = 500)
    ), 
    column(3, offset = 1,  
           sliderInput("Return", "Return Rate (%)",
                       min = 0, max = .20,
                       value = .05,
                       step = .001),
           sliderInput("Growth", "Growth Rate (%)",
                       min = 0, max = .20,
                       value = .02,
                       step = .001)
    ), 
    column(3, offset = 1,
           sliderInput("Years", "Years",
                       min = 0, max = 50,
                       value = 20,
                       step = 1),
           selectInput("Facet", h3("Facet?"), 
                       choices = list("Yes" = 1, "No" = 2), selected = 2)
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      hr(),
      h4("Timelines"),
      plotOutput("modePlot"),
      hr(),
      h4("Balances"),
      tableOutput("table")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  future_value <- function(amount, rate, years){
    FV <- amount * ((1 + rate)^years)
    return(FV)
  }
  annuity <- function(contrib, rate, years){
    fraction <- (((1 + rate)^years)-1) / rate
    FVA <-contrib * fraction
    return(FVA)
  }
  growing_annuity <- function(contrib, rate, growth, years){
    fraction <- (((1 + rate)^years)-((1 + growth)^years)) / (rate - growth)
    FVGA <-contrib * fraction
    return(FVGA)
  }
  
  modalities <- reactive({
    mode1 <- c()
    mode2 <- c()
    mode3 <- c()
    
    for(i in 0:input$Years){
      mode1 <- c(mode1, future_value(amount = input$Initial, rate = input$Return, years = i))
      mode2 <- c(mode2, annuity(contrib = input$Annual, rate = input$Return, years = i) + 
                   future_value(amount = input$Initial, rate = input$Return, years = i))
      mode3 <- c(mode3, growing_annuity(contrib = input$Annual, rate = input$Return, growth = input$Growth, years = i) +
                   future_value(amount = input$Initial, rate = input$Return, years = i))
    } 
    
    modes123 <- c(mode1, mode2, mode3)
    mode_names <- c(rep("no_contrib", input$Years+1), rep("fixed_contrib", input$Years+1), rep("growing_contrib", input$Years+1))
    yr <- c(rep(0:input$Years, 3))
    
    modalities <- data.frame('year' = yr, 'Modes' = mode_names, 'Modes123' = modes123)
    return(modalities)
  })
  
  output$modePlot <- renderPlot({
    
    gg <- ggplot(data = modalities(), aes(x = year)) +
      geom_line(aes(y = Modes123, colour = Modes)) +
      labs(x = 'Year', y = 'Amounts', title = 'Modalities by Year') +
      theme_grey() + 
      theme(legend.position = "right") +
      scale_color_discrete(labels = c('fixed_contrib', 'growing_contrib', 'no_contrib'))
    
    gg_facet <- ggplot(data = modalities(), aes(x = year)) +
      geom_line(aes(y = Modes123)) +
      geom_area(aes(y = Modes123, fill = Modes)) +
      labs(x = 'Year', y = 'Amounts', title = 'Modalities by Year') +
      theme_grey() +
      theme(legend.position = "right") +
      scale_color_discrete(labels = c('no_contrib', 'growing_contrib', 'fixed_contrib')) +
      facet_wrap(~Modes)
    
    if(input$Facet == 2){
      return(gg)
    }
    if (input$Facet == 1){
      return(gg_facet)
    }
  }) 
  
  output$table <- renderTable({
    dcast(modalities(), formula = year~Modes)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

