#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library (ggplot2)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Graph Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            
            
            actionButton("lmPlot", "Linear Model"),
            tags$hr(), # Horizontal line
            downloadButton('downloadPlot', 'Download Plot')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
           h4("Rsquared"),
           textOutput("Rsquared"),
           h4("Intercept"),
           textOutput("Intercept"),
           h4("Slope"),
           textOutput("Slope"),
           
          
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    LinearModel <- eventReactive(input$lmPlot, {
        y <- dataInput()$y
        x <- dataInput()$x
        lmPlot <- lm(y ~ x)
    
    })
    
    output$distPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y)
    })

    lmGraph <- function(){
        plot(dataInput()$x,dataInput()$y)
        abline(LinearModel())
        
       
        
    }
        
    
    
    output$lmPlot <- renderPlot({
        # y <- dataInput()$y
        # x <- dataInput()$x
        # lmPlot <- lm(y ~ x)
       # plot(dataInput()$x,dataInput()$y)
        # abline(LinearModel())
        lmGraph()
        
        
    })
     output$summary <- renderText({
    
       paste("R2 = ",signif(summary(LinearModel())$r.squared, 5),
              "Intercept =",signif(LinearModel()$coef[[1]],5 ),
              "Slope =",signif(LinearModel()$coef[[2]], 5))    
                               
    })
     
     output$Rsquared <- renderText({
         
         
         paste("R2 = ",signif(summary(LinearModel())$r.squared, 5))
               
         
     })
     
     output$Intercept <- renderText({
         
         
         paste("Intercept =",signif(LinearModel()$coef[[1]],5))
         
         
     })
    
     output$Slope <- renderText({
         
         
         paste("Slope =",signif(LinearModel()$coef[[2]], 5))
         
     
     })
     
    
             
     output$downloadPlot <- downloadHandler(
         filename = function() { paste(input$file1$datapath, '.png', sep='') },
         content = function(file) {
             png(file)
             lmGraph()
             dev.off()
                 })
            }
        
    
        


# Run the application 
shinyApp(ui = ui, server = server)
