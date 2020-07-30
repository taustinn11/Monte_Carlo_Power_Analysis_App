## app.R

## Monte Carlo Power Analysis User Application



ui <- fluidPage(
  titlePanel("Monte Carlo Power Analysis"),
  sidebarLayout(
    sidebarPanel(
      
    ),
    mainPanel(
      
    )
  )
)

server <- function(input, output) { }

# Run the application 
shinyApp(ui = ui, server = server)
