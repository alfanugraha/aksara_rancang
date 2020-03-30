library(shiny)
library(shiny.users)

demo_users <- list(
  list(
    username = "demo-appsilon", 
    password_sha256 = "A7574A42198B7D7EEE2C037703A0B95558F195457908D6975E681E2055FD5EB9", 
    roles = list("basic", "admin")
  ),
  list(
    username = "john", 
    password_sha256 = "C2F77349B4D0CDE5A1E865195A9E395E1DF8829BE9D31707BD12F44CEB384A60", 
    roles = list("basic")
  )
)

ui <- shinyUI(fluidPage(
  div(class = "container", style = "padding: 4em",
      login_screen_ui('login_screen'),
      uiOutput("authorized_content")
  )
))

server <- shinyServer(function(input, output) {
  users <- initialize_users(demo_users)
  callModule(login_screen, 'login_screen', users)
  
  output$authorized_content <- renderUI({
    if (!is.null(users$user())) {
      ... # application content
    }
  })
})
  
  shinyApp(ui, server)