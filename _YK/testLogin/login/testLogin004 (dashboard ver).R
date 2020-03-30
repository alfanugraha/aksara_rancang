rm(list = ls())
library(shiny)
library(shinydashboard)
library(readxl)

my_username <- "test"
my_password <- "abc"
# user_list<-read_excel("Yumna/Login/user_list.xlsx")
# my_username<-user_list$user
# my_password<-user_list$pass
###########################/ui.R/##################################

ui1 <- function(){
  tagList(
    div(id = "login",
        wellPanel(textInput("userName", "Username"),
                  passwordInput("passwd", "Password"),
                  br(),
                  actionButton("Login", "Log in"),
                  verbatimTextOutput("dataInfo")
        )
    ),
    tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

ui2 <- function(){
  tagList("You did it!")
  }

header <- dashboardHeader(title = "RED CLUWE")
sidebar <- dashboardSidebar( )
body <- dashboardBody(
  tags$head(tags$style("#dataInfo{color: red")),
  htmlOutput("page")
)

ui <- dashboardPage(header, sidebar, body)

###########################/server.R/##################################

server = (function(input, output,session) {
  
  Logged <- FALSE
  Security <- TRUE
  
  USER <- reactiveValues(Logged = Logged)
  SEC <- reactiveValues(Security = Security)
  
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(my_username == Username & my_password == Password) {
            USER$Logged <- TRUE
          } else {SEC$Security <- FALSE}
        } 
      }
    }    
  })
  
  observe({
    if (USER$Logged == FALSE) {output$page <- renderUI({ui1()})}
    if (USER$Logged == TRUE) {output$page <- renderUI({ui2()})}
  })
  
  observe({
    output$dataInfo <- renderText({
      if (SEC$Security) {""}
      else {"Username atau Password yang dimasukkan salah"}
    })
  })
  
})

runApp(list(ui = ui, server = server))