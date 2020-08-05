## app.R

## Monte Carlo Power Analysis User Application

#Libraries
library(tidyverse)
library(shiny)

#UI
ui <- fluidPage(
  titlePanel("Monte Carlo Power Analysis"),
  br(),
  p("This is an application for graphically representing power as a function of sample size for some anticipated effect size and variance. Input values for number of simulations, minimum relevant effect size (as a percentage of effect), and standard deviation (as a percentage of effect)."),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      
      numericInput(inputId = "n_sims", label = "Number of simulations (more will generate a smoother curve but may take longer)", value = 300, step = 100),
      sliderInput(inputId = "n_reps", label = "Number of independent replicates to consider (more will result in a longer computation time for a given number of simulations)", min = 2, max = 100, value = c(2, 20)),
      numericInput(inputId = "fx", label = "Minimum relevant effect size (as a percentage of the effect)", value = 10, min = 0, max = 500, step = 10),
      numericInput(inputId = "sd", label = "Anticipated standard deviation (as a percentage of the effect)", value = 10, min = 0, max = 500, step = 5),
      
      p(strong("Plot")),
      checkboxInput(inputId = "smoother", label = "Show smoothing line", value = FALSE),
      
      submitButton(text = "Apply Changes")
      
    )
    ,
    mainPanel(
      plotOutput(outputId = "plot"),
      tableOutput(outputId = "table")
    )
  )
)

#Server
server <- function(input, output) {
  
  #Data Randomizers
  data_randomizer <- function(n_reps, fx, sd) {
    
    control <- rnorm(n_reps, 1, sd)
    treat <- rnorm(n_reps, (1+1*fx), sd)
    
    outcomes <- cbind(control, treat)
    return(outcomes)
    
  }
  
  #Set reps
  n_reps = reactive({c(input$n_reps[1]:input$n_reps[2])})
  
  #Table
  df <- reactive({
    data.frame(n_reps(), 
                   pwr = sapply(n_reps(), function(x) {
                     
                     
                     p.val <- replicate(input$n_sims,
                                        {
                                          df <- data_randomizer(x, (input$fx/100), (input$sd/100))
                                          
                                          p.val <- t.test(df[,2], df[,1], alternative = "two.sided", )$p.value
                                          
                                          return(p.val)
                                        }
                     )
                     sum(p.val<0.05)/input$n_sims*100
                   } 
                   )
  )
  })
    
  #Set df to be table
  output$table <- renderTable({df()})

    
  #Plot
  output$plot <- renderPlot({
      gg <- ggplot(df(), aes(n_reps(), pwr))+
      geom_line(size = 2, color = "Red")+
      geom_hline(yintercept = 80, linetype = 2)+
      ggtitle("Cumulative Power Distribution")+
      xlab("Number of Independent Replicates")+
      ylab("Power")+
      theme_bw()
      
      if(input$smoother) {gg + geom_smooth(color = "blue", linetype = 3)}
      else {gg}
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
