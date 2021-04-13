library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)


# Define server logic ----
server <- function(input, output, session) {
  
  # Auxiliary Variables
  color1 = "#ff0099"
  color2 = "#0099ff"
  
  #Fucntion for calculation of BMI
  indexCalculator <- function(x, y){
    return(10000 * x / (y * y))
  }
  
  # Upload data
  data_set <- read.csv("data_set4.csv", header=TRUE, sep=',')
  
  # Data manipulation
  data <- reactive({
  
    req(input$age_group)
    data_set %>% 
      filter(Age %in% input$age_group)  %>% 
      group_by(Sex) %>% 
      summarise(Weight = mean(Weight), 
                Height = mean(Height), 
                Number = n(),
                )
  })
  
  # Update selection dynamically
  observe({
    updateSelectInput(session, "age_group", choices=data_set$Age)
  })
  
  # Plotting data
  output$plot <- renderPlot({
    
    
      p1 <- ggplot(data(), aes(x=Sex, y=Weight)) + 
            geom_bar(stat="identity", fill=c(color1, color2))+ 
            ggtitle("Average weight \n by gender") +
            xlab("Sex") + ylab("Weight, kg") +
            theme(plot.title = element_text(hjust=0.5))
      p1
      
      p2 <- ggplot(data(), aes(x=Sex, y=Height))+ 
            geom_bar(stat="identity", fill=c(color1, color2)) +
            ggtitle("Average height \n by gender") +
            xlab("Sex") + ylab("Height, cm") +
            theme(plot.title = element_text(hjust=0.5))
      p2
      
      p3 <- ggplot(data(), aes(x=Sex, y=Number)) +
            geom_bar(stat="identity", fill=c(color1, color2)) + 
            ggtitle("A number of people \n by gender") +
            xlab("Sex") + ylab("Number, 1") + 
            theme(plot.title = element_text(hjust=0.5))
      p3
      
      p4 <- ggplot(data(), aes(x=Sex, y=indexCalculator(Weight, Height))) +
            geom_bar(stat="identity", fill=c(color1, color2)) +
            geom_abline(slope=0, intercept=25, col = "red", lty=2, size=1.5) +
            ggtitle("BMI \n by gender") +
            xlab("Sex") + ylab ("BMI, kg/m^2") + 
            theme(plot.title = element_text(hjust=0.5))
      p4
      
      grid.arrange(p1,p2,p3,p4, ncol=4,widths = c(1,1,1,1))
    })
  
  # Calcution of your BMI
  output$output <- renderText({
    indexCalculator(input$num1, input$num2)
  })
}
  
  
# Define UI ----
ui <- fluidPage(
  titlePanel("BMI Calculator"),
  sidebarLayout(
    sidebarPanel(
      h5("This is a very simple example of a Shiny App.
         It shows distribtution of weight, height and body mass index (BMI) of 
         people divided by gender in three age groups.
         You can select the group below:"
         ),
      selectInput( inputId = "age_group",
                   label = "Choose age group",
                   "Age"
                   ),
      h5("If you are tired you can calculate your own BMI!"),
      numericInput("num1", "Enter the weight (kg)", 0),
      numericInput("num2", "Enter the height (cm)", 0),
      h5("Your BMI is"),
      textOutput("output")
      ),
    
    mainPanel(
      plotOutput("plot")
      )
    )
  )


# Run the app ----
shinyApp(ui = ui, server = server)