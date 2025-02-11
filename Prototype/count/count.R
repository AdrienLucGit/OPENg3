library(lubridate)
library(shiny)
library(beepr)

ui <- fluidPage(
  hr(),
  actionButton('start','Start'),
  actionButton('stop','Stop'),
  actionButton('reset','Reset'),
  numericInput('seconds','Seconds:',value=10,min=0,max=99999,step=1),
  textOutput('timeleft')
)

server <- function(input, output, session) {
  # Initialize the timer, 10 seconds, not active.
  timer <- reactiveVal(10)
  active <- reactiveVal(FALSE)
  
  # Output the time left.
  output$timeleft <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
  })
  
  # Observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(1000, session)
    isolate({
      if(active()) {
        timer(timer()-1)
        if(timer() < 1) {
          active(FALSE)
          beep(sound = 9)  # Ajout du son à la fin du décompte
          showModal(modalDialog(
            title = "Important message",
            "Countdown completed!"
          ))
        }
      }
    })
  })
  
  # Observers for action buttons
  observeEvent(input$start, {active(TRUE)})
  observeEvent(input$stop, {active(FALSE)})
  observeEvent(input$reset, {timer(input$seconds)})
}

shinyApp(ui, server)
