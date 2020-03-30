
library(shiny)
library(shinydashboard)
library(rintrojs)

ui <- dashboardPage(
  dashboardHeader(title = "rintrojs test",
                  dropdownMenu(type = "tasks",
                               icon = icon("info fa-1g"),
                               badgeStatus = NULL,
                               headerText = "Informasi Tambahan",
                               notificationItem(
                                 text = actionButton('session_info', label="About this session",
                                                     icon = icon("window-maximize")),
                                 icon = icon(""), status = "primary"),
                               notificationItem(
                                 text = actionButton('iSEE_info', label="About iSEE",
                                                     icon = icon("heart")),
                                 icon = icon(""), status = "primary"
                               )),
                  dropdownMenu(type = "tasks",
                               icon = icon("plane"),
                               badgeStatus = NULL,
                               headerText = "Other dropdown",
                               notificationItem(
                                 text = actionButton('openfile', label="Open this file",
                                                     icon = icon("file")),
                                 icon = icon(""), status = "primary"),
                               notificationItem(
                                 text = actionButton('moreinfo', label="Moooore info",
                                                     icon = icon("heart")),
                                 icon = icon(""), status = "primary"
                               ))),
  dashboardSidebar(introjsUI(),
                   sidebarMenu(menuItem("Tab #1", tabName = "tab1"))),
  dashboardBody(tabItems(
    tabItem(
      tabName = "tab1",
      plotOutput("plot1"),
      selectInput("select1", "Select", letters[1:2], letters[1]),
      selectInput("select2", "Select", letters[1:2], letters[1]),
      sliderInput("slider1", "Slide", 0, 1, 0, 0.5),
      sliderInput("slider2", "Slide", 0, 1, 0, 0.5),
      actionButton("help", "Take tour")
    )
  )),
  title = "rintrojs test"
)

server <- shinyServer(function(input, output, session) {
  steps <- reactive(data.frame(
    element = c(
      "tab1",
      "#plot1",
      ### this is the part that matters
      ".js-irs-0",
      ".js-irs-1",
      "#select1 + .selectize-control",
      "#select2 + .selectize-control",
# These selectors make the most sense to me
      "body > div > header > nav > div > ul > li:nth-child(1) > ul",
      "body > div > header > nav > div > ul > li:nth-child(2) > ul"
    ),
    intro = c(
      "tes",
      "This is a plot.",
      "This is a slider.",
      "This is another slider.",
      "This is a select input.",
      "This is another select input.",
      "Text for dropdown 1",
      "Text for dropdown 2"
    ),
    position = c("right", "bottom", "right", "right",
                 "left", "left","bottom", "bottom")
  ))
  observeEvent(input$help, {
    introjs(session, options = list(steps = steps()), events = list(
      # programmatically click dropdown before step
      onbeforechange = I(
        'if (this._currentStep == 5) {
          $("body > div > header > nav > div > ul > li:nth-child(1) > a").click()
        } else if (this._currentStep == 6) {
          $("body > div > header > nav > div > ul > li:nth-child(2) > a").click()
        }'
      )
    ))
  })
  output$plot1 <- renderPlot({
    plot(0, 0)
  })
})

shinyApp(ui = ui, server = server)