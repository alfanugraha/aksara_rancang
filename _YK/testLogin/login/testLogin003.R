library("shiny")
library("shinyjs")
library("stringr")


# in case you want to send error messages when login is wrong
# add this to the /www folder in your shiny app (shiny server) as message-handler.js file
#
# // This recieves messages of type "testmessage" from the server.
# Shiny.addCustomMessageHandler("testmessage",
#                               function(message) {
#                                   alert(JSON.stringify(message));
#                               }
# );

shinyApp(
  
  ui = fluidPage(
    
    useShinyjs(),  # Set up shinyjs
    
    # Layout mit Sidebar
    sidebarLayout(
      
      ## Sidebar -----
      shinyjs::hidden(
        div(id = "Sidebar", sidebarPanel(
          
          # > some example input on sidebar -----
          conditionalPanel(
            condition = "input.tabselected > 1",
            dateRangeInput(inputId = "date",
                           label = "Choose date range",
                           start = "2018-06-25", end = "2019-01-01",
                           min = "2018-06-25", max = "2019-01-01",
                           startview = "year")) 
          
        ))), # closes Sidebar-Panel
      
      # Main-Panel ------
      mainPanel(
        
        tabsetPanel(
          
          # > Login -------
          tabPanel("Login",
                   value = 1,
                   br(),
                   textInput("username", "Username"),
                   passwordInput("password", label = "Passwort"),
                   # If you want to add custom javascript messages
                   # tags$head(tags$script(src = "message-handler.js")),
                   actionButton("login", "Login"),
                   textOutput("pwd")
                   
          ), # closes tabPanel
          
          id = "tabselected", type = "pills"
          
        )  # closes tabsetPanel      
        
      )  # closes mainPanel                      
      
    ) # closes sidebarLayout
    
  ), # closes fluidPage
  
  
  # Server ------
  server = function(input, output, session){
    
    user_vec <- c("user123" = "loginpassword1",
                  "user456" = "loginpassword2")
    
    # I usually do run the code below on a real app  on a server
    # user <- reactiveValues(his = readRDS(file = "logs/user_his.rds"),
    #                        log = readRDS(file = "logs/user_log.rds"),
    #                        vec = readRDS(file = "logs/user_vec.rds"))
    #
    # where user_his is defined as follows
    # user_his <- vector(mode = "integer", length = length(user_vec))
    # names(user_his) <- names(user_vec)
    
    
    observeEvent(input$login, {
      
      if (str_to_lower(input$username) %in% names(user_vec)) { # is username in user_vec?
        
        # Alternatively if you want to limit login attempts to "3" using the user_his file
        # if (str_to_lower(input$username) %in% names(user$vec[user$his < 3])) {
        
        if (input$password == unname(user_vec[str_to_lower(input$username)])) {
          
          # nulls the user_his login attempts and saves this on server
          # user$his[str_to_lower(input$username)] <- 0
          # saveRDS(user$his, file = "logs/user_his.rds")
          
          # Saves a temp log file
          # user_log_temp <- data.frame(username = str_to_lower(input$username),
          #                            timestamp = Sys.time())
          
          # saves temp log in reactive value
          # user$log <- rbind(user$log, user_log_temp)
          
          # saves reactive value on server
          # saveRDS(user$log, file = "logs/user_log.rds")
          
          
          # > Add MainPanel and Sidebar----------
          shinyjs::show(id = "Sidebar")
          
          appendTab(inputId = "tabselected",
                    
                    tabPanel("Tab 1",
                             value = 2
                             
                    ) # closes tabPanel,
                    
          )
          
          appendTab(inputId = "tabselected",
                    
                    tabPanel("Tab 2",
                             value = 3
                             
                    ) # closes tabPanel,
          )
          
          appendTab(inputId = "tabselected",
                    
                    tabPanel("Tab 3",
                             
                             value = 4
                             
                    ) # closes tabPanel         
          )
          
          removeTab(inputId = "tabselected",
                    target = "1")
          
        } else { # username correct, password wrong
          
          # adds a login attempt to user_his 
          # user$his[str_to_lower(input$username)] <- user$his[str_to_lower(input$username)] + 1
          
          # saves user_his on server
          # saveRDS(user$his, file = "logs/user_his.rds")
          
          # Messge which shows how many log-in tries are left
          #
          # session$sendCustomMessage(type = 'testmessage',
          #                           message = paste0('Password not correct. ',
          #                                            'Remaining log-in tries: ',
          #                                            3 - user$his[str_to_lower(input$username)]
          #                           )
          # )
          
          
        } # closes if-clause
        
      } else { #  username name wrong or more than 3 log-in failures 
        
        # Send error messages with javascript message handler
        #
        # session$sendCustomMessage(type = 'testmessage',
        #                           message = paste0('Wrong user name or user blocked.')                          
        # )
        
      } # closes second if-clause
      
    }) # closes observeEvent
    
    
  } # Closes server
) # Closes ShinyApp