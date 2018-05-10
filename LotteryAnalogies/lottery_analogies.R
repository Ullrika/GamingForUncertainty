library(shiny)
library(shinyjs)
library(shinydashboard)

source('lottery_analogies_functions.R')


ui <- pageWithSidebar(
  headerPanel("Probability analogies"),
  sidebarPanel(
    fluidRow('The chance of winning a lottery is 1 / N.',br(),'Let us get a feeling for what this means for different values on N?'),
    numericInput("n", "N:", value = 1000),
    br(),
    actionButton("goButton", "Get lottery analogies!"),
    p("Click the button to update the value displayed in the main panel."),
    br(),br(),
    fluidRow('The first three analogies are borrowed from Teaching Probability by Jenny Gage and David Spiegelhalter, Cambridge Mathematics. https://teachingprobability.org/')
  ),
  mainPanel(
    fluidRow(
    box(title='Grains of rice in a bath',width = 12,
    div(fluidRow('Let the winning ticket be a grain of rice. Fill a bathtub with all the tickets in the lottery.'),
    fluidRow(column(5,img(src = 'Clawfoot_bathtub.jpg', width = "75%", align = "center")),
    column(6,uiOutput(outputId = "r1Text"),
    uiOutput(outputId = "r2Text")))))),
    
    fluidRow(div(
    box(title='Flipping a fair coin',
    fluidRow(img(src = 'coin.jpg', width = "50%", align = "center")),
    uiOutput(outputId = "hText"))
    ),
    box(title='Tossing a fair die',
    fluidRow(img(src = 'six_die.jpg', width = "75%", align = "center")),
    uiOutput(outputId = "sText"))),
    
    fluidRow(box(title='Walking from Paris',width = 12,
        div(column(5,img(src = 'paris.jpg', width = "75%", align = "center")),
        column(6,uiOutput(outputId = "pText"),
        uiOutput(outputId = "cityText"))))
    )
  )
)


server <- function(input, output) {
  r <- eventReactive(input$goButton, {
    grain_of_rice(input$n)
    #Clawfoot_bathtub.jpg
  })
  
  h <- eventReactive(input$goButton, {
    heads_flipped_in_a_row(input$n)
  })
  
  s <- eventReactive(input$goButton, {
    sixs_thrown_in_a_row(input$n)
  })
  p <- eventReactive(input$goButton, {
    walk(input$n)
  })
  city <- eventReactive(input$goButton, {
    distance_from_paris(walk(input$n)$kilometers_walked)
  })
  
  output$r1Text <- renderText({
    paste('The chances of winning the lottery will then corresponds to filling the bathtub with',r()$litres_of_rice,'litres of rice.')
  }) 
  output$r2Text <- renderText({
    paste('This rice will reach',r()$depth_of_rice_in_bath,'cm from the bottom of the tub.')
  })
  output$hText <- renderText({
    paste('The chances of winning the lottery corresponds to getting',h(),'heads flipped in a row with a fair coin.')
  })
  output$sText <- renderText({
    paste('One can also compare it with getting a six',s(),'times in a row with a fair die.')
  })
  output$pText <- renderText({
    paste('If you take two steps per meter and your walking speed is 4 km per hour, the corresponding number of steps would take you',
          p()$kilometers_walked,'kilometers and it would take',p()$time_to_walk,'hours.')
  })
  output$cityText <- renderText({
    paste('This is longer than walking from Paris to', city()$ci[1],' but shorter than from Paris to', city()$ci[2],'.')
  })
}

shinyApp(ui = ui, server = server)


