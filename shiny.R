library(shiny)

  bs2appObj <- function() {
    shinybootstrap2::withBootstrap2({
      shinyApp(
        ui = fluidPage(
          sidebarPanel(selectInput("n", "n", c(1, 5, 10))),
          mainPanel(plotOutput("plot")),
#           <p class="muted">Fusce dapibus, tellus ac cursus commodo, tortor mauris nibh.</p>
        ),
        server = function(input, output) {
          output$plot <- renderPlot({
            plot(head(cars, as.numeric(input$n)))
          })
        }
      )
    })
  }