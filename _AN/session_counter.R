library(shiny)

# Create a reactive object here that we can share between all the sessions.
vals <- reactiveValues(count=0)

server<-function(input, output, session) {
  # Increment the number of sessions when one is opened.
  # We use isolate() here to:
  #  a.) Provide a reactive context
  #  b.) Ensure that this expression doesn't take a reactive dependency on
  #      vals$count -- if it did, every time vals$count changed, this expression
  #      would run, leading to an infinite loop.
  isolate(vals$count <- vals$count + 1)
    
  # When a session ends, decrement the counter.
  session$onSessionEnded(function(){
    # We use isolate() here for the same reasons as above.
    isolate(vals$count <- vals$count - 1)
  })
  
  # Reactively update the client.
  output$count <- renderText({
    vals$count
  })
}


ui<-fluidPage(
  
  # Application title
  titlePanel("Session Counter"),
  
  # Offer 1 panel which just displays the count of sessions the app currently
  # has connected.
  mainPanel(
    "There are currently", 
    verbatimTextOutput("count"),
    "session(s) connected to this app."
  )
)

shinyApp(ui = ui, server = server)