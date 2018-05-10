library(shiny)
library(shinyjs)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Decision making 1.0", titleWidth = 270),
  dashboardSidebar(width = 270,
                   sidebarMenu(menuItem("Info", tabName = 'info',icon = icon("info")),
                               menuItem("A decision is a gamble", tabName = 'gamble',icon = icon("info")),
                               menuItem("Framing", tabName = 'framing',icon = icon("info")),
                               menuItem("Ellsbergs paradox", tabName = 'ellsberg',icon = icon("info")))
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "info", 
              h4("This app introduce a way to describe decision making in terms of gambles and 
                 demonstrates two important findings on how people make decisions.")
              ),
      
      tabItem(tabName = "gamble",  titlePanel(title= "Gamble"),
              fluidRow("This page will be updated with other information explaining a decision making as a gamble and a text introducing the betting interpretation of probability"),
              fluidRow(img(src = 'gambles1.png', width = "75%", align = "center"))
      ),
      
      tabItem(tabName = "framing",
              shinyUI(fluidPage(
                
                titlePanel("Framing"),
                
                mainPanel(
                    tabsetPanel(
                      tabPanel("First framing", 
                               fluidRow(h3("A new disease is expected to kill 600 people. Assume you are the decision maker. Which alternative do you prefer?")),
                               fluidRow(radioButtons(inputId = 'fram1',label =NULL,choiceNames = c('.','A','B'),choiceValues= c(1,2,3), selected = NULL, inline = TRUE)),
                               fluidRow(img(src = 'fram1.png', width = "100%"))), 
                      tabPanel("Second framing", 
                               fluidRow(h3("A new disease is expected to kill 600 people. Assume you are the decision maker. Which alternative do you prefer?")),
                               fluidRow(radioButtons(inputId = 'fram2',label =NULL,choiceNames = c('.','C','D'),choiceValues= c(1,2,3), selected = NULL, inline = TRUE)),
                               fluidRow(img(src = 'fram2.png', width = "100%"))),
                      tabPanel("Explanation", 
                                fluidRow(h4("These questions were asked in a famous experiment conducted by Tversky and Kahneman in 1985.")),br(),
                                fluidRow(div(h4("In the first question 72% of people preferred A to B.",uiOutput('f1')))),br(),
                                fluidRow(div(h4("In the second question 22% of people preferred C to D.",uiOutput('f2')))),br(),
                                fluidRow(h4("Even though these two gambles are identical, people tend to change which one they prefer depending on the way the information is presented.")), br(),
                                box(width = 12,align = "left", background = "olive",h4("Framing occurs when a change in presentation influences behaviour surrounding 
                                         the making of a choice, even when the objective characteristics are not changed."),br(),
                                        h4("If you go back you can see that the first framing is that a certain amount will be saved with certainty, whereas the 
                                       second framing is that a certain number will die with certainty."))
)
                )
)
              ))),
      tabItem(tabName = "ellsberg",
              shinyUI(fluidPage(
                
                titlePanel("Ellsberg's paradox"),
                
                mainPanel(
                    tabsetPanel(
                      tabPanel("First gamble", 
                               fluidRow(h3("Select the alternative you prefer")),
                               fluidRow(radioButtons(inputId = 'ells1ch',label =NULL,choiceNames = c('.','I','II'),choiceValues= c(1,2,3), selected = NULL, inline = TRUE)),
                               fluidRow( img(src = 'ellsb1.png', width = "100%"))),  
                      tabPanel("Second gamble", 
                               fluidRow(h3("Select the alternative you prefer")),
                               fluidRow(radioButtons(inputId = 'ells2ch',label =NULL,choiceNames = c('.','III','IV'),choiceValues= c(1,2,3), selected = NULL, inline = TRUE)),
                               fluidRow(img(src = 'ellsb2.png', width = "100%"))), 
                      tabPanel("Explanation",
                               fluidRow(div(h4("A common pattern of response is that people preferred I to II.",uiOutput('e1')))),br(),
                               fluidRow(div(h4("A common pattern of response is that eople preferred III to IV.",uiOutput('e2')))),br(),
                               fluidRow(h4("Adding the same reward to two gambles done for drawing yellow balls, should not change the preference over alternatives (known as the Sure thing principle).")), 
                               fluidRow(h4("In experiments, people tend to change which one of the alternative they prefer depending on the way the information is presented.")), br(),
                               box(h4("Ellberg's paradox shows that the Sure thing principle does not work
                                when people are sensitive to uncertainty they do not like alternatives where there is uncertainty."),br(),
                                   h4("It is therefore important to consider and communicate uncertainty when making decisions."),width = 12,align = 'left', background = "olive"))
                    )
                  )
              ))
          )#end tabitem
      
      )
    )
)

server <- function(input, output) {

observeEvent(input$fram1,{
  output$f1 <- renderText(c("You have not made your choice yet.","You preferred A, like most people did in the experiment.",
                            "You preferred B, which was not like the majority in the experiment.")[as.numeric(input$fram1)])
})
  observeEvent(input$fram2,{
    output$f2 <- renderText(c("You have not made your choice yet.","You preferred C, which was not like the majority in the experiment.",
                              "You preferred D, like most people did in the experiment.")[as.numeric(input$fram2)])
  })
  observeEvent(input$fram1,{
    output$f3 <- renderText(c("You have not made your choice yet.","You preferred A, like most people did in the experiment.",
                              "You preferred B, which was not like the majority in the experiment.")[as.numeric(input$fram1)])
  })
  
  observeEvent(input$ells1ch,{
    output$e1 <- renderText(c("You have not made your choice yet.","You preferred I, like most people did.",
                              "You preferred II, which was not like the majority.")[as.numeric(input$ells1ch)])
  })   
  observeEvent(input$ells2ch,{
    output$e2 <- renderText(c("You have not made your choice yet.","You preferred III, which was not like the majority.",
                              "You preferred IV, like most people did.")[as.numeric(input$ells2ch)])
  })
  observeEvent(input$ells1ch1,{
    output$e3 <- renderText(c("You have not made your choice yet.","You preferred A, like most people did.",
                              "You preferred B, which was not like the majority,")[as.numeric(input$ells1ch1)])
  })
}

shinyApp(ui = ui, server = server)


