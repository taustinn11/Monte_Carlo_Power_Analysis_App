## app.R

## Monte Carlo Power Analysis User Application

#Libraries
library(tidyverse)
library(shiny)

#UI
ui <- fluidPage(
  titlePanel("Monte Carlo Power Analysis"),
  sidebarLayout(
    sidebarPanel(
      p("This is an application for graphically representing power as a function of sample size for some anticipated effect size and variance. Input values for number of simulations, minimum relevant effect size (as a percentage of effect), and standard deviation (as a percentage of effect)."),
      
      numericInput(inputId = "n_sims", label = "Number of simulations (more will generate a smoother curve but may take longer)", value = 100, step = 100),
      numericInput(inputId = "fx", label = "Minimum relevant effect size (as a percentage of the effect)", value = 10, min = 0, max = 500, step = 10),
      numericInput(inputId = "sd", label = "Anticipated standard deviation (as a percentage of the effect)", value = 10, min = 0, max = 500, step = 5),
      
      p(strong("Plot")),
      checkboxInput(inputId = "smoother", label = "Show smoothing line", value = FALSE),
      
      p(strong("Table")),
      checkboxInput(inputId = "table", label = "Display table", value = TRUE),
      
    )
    ,
    mainPanel(
      h3("Cumulative Power Acquisition with Increasing Sample Size"),
      plotOutput(outputId = "plot"),
      tableOutput(outputId = "table")
    )
  )
)

#Server
server <- function(input, output, session) {
  
  #Set the reactivity
#  observe({
#    updateNumericInput(session, inputId = "n_sims")
#    updateNumericInput(session, "fx")
#    updateNumericInput(session, "sd")
#  })
  
  #Set reps
  n_reps = c(2:50)
  
  #Table
  df <- reactive({
    data.frame(n_reps, 
                   pwr = sapply(n_reps, function(x) {
                     
                     
                     p.val <- replicate(input$n_sims,
                                        {
                                          df <- data_randomizer(x, (input$fx/100), (input$sd/100))
                                          
                                          p.val <- t.test(df[,2], df[,1], alternative = "two.sided", )$p.value
                                          
                                          return(p.val)
                                        }
                     )
                     sum(p.val<0.05)/n_sims*100
                   } 
                   )
  )
  })
    
  #Set df to be table
  output$table <- renderTable({df()})

    
  #Plot
  output$plot <- renderPlot({
      gg <- ggplot(df(), aes(n_reps, pwr))+
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
