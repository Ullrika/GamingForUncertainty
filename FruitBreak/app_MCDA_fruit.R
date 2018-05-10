library(shiny)
library(shinyjs)
library(rhandsontable)
library(RColorBrewer)
library(shinydashboard)


source("maximax_grafer.R")
source("maximin_grafer.R")
source("hurwicz_grafer.R")
source("minimaxregret_grafer.R")
source("decision_rules.R")
source('createmat.R')
source('impact_calc.R')

ui <- dashboardPage(
  dashboardHeader(title = "Fruit break", titleWidth = 270),
  
  dashboardSidebar(width = 270,sidebarMenu(
    menuItem("Start", tabName = 'info',icon = icon("info")),
                    menuItem("Select candidate fruits", tabName = "alter", icon = icon("bars")),
                    menuItem("Structure the decision problem", tabName = "strt", icon = icon("table")),
                    menuItem("Enter fruit scores", tabName = "vls", icon = icon("calculator")),
                    menuItem("Compare fruits and select", tabName = "intdom", icon  = icon("bar-chart"))
                   )),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),

    tabItems(
      
      tabItem(tabName = "info",
      fluidRow(column(10,align = "center", titlePanel(title= "Decision making"))),
      fluidRow(column(10,align = "center", h4('This app demonstrates structured decision making using Multi Criteria Decision Analysis.'))),br(),
      fluidRow(column(10,align = "center", h4("You are guided through the steps a decision making process consising of a decision maker and her values, 
                      decision alternatives, certainty or uncertainty in the outcome of decisions on values, an idea of what is a good decision. 
                      The aim is to demonstrate what it could mean to make a decision under uncertainty."))),
      fluidRow(column(10,align = 'center', h3("Your task is to select a fruit to eat at your next fruit break!"))),
fluidRow(column(10,align = 'center',h3("Good luck",col = 'red'))),
               #Choose your favourite fruit!))),
      fluidRow(align = 'center',div(
        img(src = 'apelsin.jpg', height = '100px', width = '100px'),
        img(src = 'banan.jpg', height = '100px', width = '100px'),
        img(src = 'apple.jpg', height = '100px', width = '150px')
      )),
fluidRow(h5("This app is undergoing further development. Check out the project web site https://www.cec.lu.se/research/gaming-for-uncertainty"))
      ),
      
      
      tabItem(tabName = 'alter',
        fluidRow(
          column(10,align = "center", titlePanel("Select which fruits to choose between! ")),
          column(width = 10, align = "center", div(style="display:inline-block", selectInput(inputId = "numAlt",label = "No. of alternatives",c("2", "3", "4", "5", "6", "7", "8"), selected = 3, width = "140px")))),
        fluidRow(column(10, align = "center", uiOutput("namesAlt")))),
        
        tabItem(tabName = "strt",                    
        fluidRow(
          column(12, align="left",titlePanel("Select criteria to help evaluating the fruits")),
          column(8, align = "left", div(style="display:inline-block", selectInput(inputId = "numC",label = "No. of criteria",c("2", "3", "4", "5", "6", "7", "8", "9","10"),selected = 3, width = "140px")))),
        fluidRow(column(4, uiOutput("namesCrit")),column(7,uiOutput("viktning")))),
        
        tabItem(tabName = "vls",## text explaning what to do
                fluidRow(column(12,align = 12, titlePanel("Specify scores on fruit criteria from 0 (bad) to 10 (excellent)!"))),
                fluidRow(box(status = 'primary', width = 12,
                             column(8,radioButtons(inputId = 'intornot',label =NULL,
                              choices = c('use prefilled point estimates - possible to change','get empty table for point estimates - use your own point estimates',
                              'use prefilled estimates with uncertainty - possible to change','get empty table where you can assign your own estimates with uncertainty'))),
                             column(4, align="left",actionButton("fill","Press to get tables")))),
              fluidRow(box(status = "primary", width = 12,
                           column(8,h2(textOutput("ScenTit1")), rHandsontableOutput("valTab1"), 
                                    h2(textOutput("ScenTit2")), rHandsontableOutput("valTab2"), 
                                    h2(textOutput("ScenTit3")), rHandsontableOutput("valTab3"),
                                    h2(textOutput("ScenTit4")), rHandsontableOutput("valTab4"), 
                                    h2(textOutput("ScenTit5")), rHandsontableOutput("valTab5"), 
                                    h2(textOutput("ScenTit6")), rHandsontableOutput("valTab6"),
                                    h2(textOutput("ScenTit7")), rHandsontableOutput("valTab7"),
                                    h2(textOutput("ScenTit8")), rHandsontableOutput("valTab8")),
                           column(4, align="left",actionButton("valdone","Calculate fruit scores!"),
                                  #h3("Go to next page to see results !")
                                  plotOutput("progressbar"))
                                  )
                       )
              ), ## end tabitem vls
      
        tabItem(tabName = "intdom",
              fluidRow(column(12,align = 'center',titlePanel('Apply a decision rule to choose your fruit !'))),
              fluidRow(column(6,uiOutput("decisionrulesbox")),#),
              #fluidRow(
                column(6,plotOutput("dom_plot", width="100%", height = "400px"))),
              br(), br(),
              fluidRow(align = 'center',titlePanel('Sensitivity analysis'),column(4,uiOutput("dec_alt")),column(8,plotOutput("sens_pie", width = "100%", height = "300px")))
        )#end tabitem intdom
    ))
)#DashboardPage end

  
  server <- function(input, output) {
    
      output$fruitbr <- renderImage({
        return(list(
          #src = "C:/Users/Ullrika/Box Sync/MYRESEARCH/höst2017/kic/RobustMCDA/apelsin",
          #contentType = "image/jpeg",
          src = "/C:/Users/Ullrika/Box Sync/MYRESEARCH/höst2017/kic/App/Rplot.png",
          #contentType = "image/png",
          #width = 300,
          #height = 200,
          alt = "Face")
        )
      }, deleteFile = TRUE)
    
    observe({
      fruitcrit = c('sweetness','easy to eat', '  cheap ')
      output$namesCrit <- renderUI({
        CritName <- vector("list", as.integer(input$numC))
        for(i in 1:as.integer(input$numC)){
          CritName[[i]] <- list(textInput(inputId = paste0("NameC",i),
                                          label = paste0("Criteria ", i), 
                                          value = fruitcrit[i]))
        }
        return(CritName)
      })
      
      output$namesAlt <- renderUI({
        AltName <- vector("list", as.integer(input$numAlt))
        fruits = c("apple","banana", "orange")
        for(i in 1:as.integer(input$numAlt)){
          if(i < 4){
          AltName[[i]] <- list(textInput(inputId = paste0("NameA",i),
                                         label = paste0("Fruit ", i),
                                         value = fruits[i]))
          }else{
            AltName[[i]] <- list(textInput(inputId = paste0("NameA",i),
                                           label = paste0("Fruit ", i),
                                           value = paste0("A ",i)))
        }
        }
        return(AltName)
        
      })
      
      output$viktning <- renderUI({
          vikt <- vector("list", as.integer(input$numC))
          for(i in 1:as.integer(input$numC)){
            vikt[[i]] <- list(sliderInput(inputId = paste0("we",i),
                                          label = paste0("Relative weight on criteria ",i),
                                          value = 1,  
                                          min = 0, max = 1, 
                                          round = FALSE, ticks = FALSE, step = 0.1))
          }
       return(vikt)
      })
    })
    
   
    
    observeEvent(input$fill,{
      Stit <- lapply(1,function (i) output[[paste0("ScenTit",i)]] <- renderText({input[[paste0("NameS",i)]]}))
      
      inam = c('use prefilled point estimates - possible to change','get empty table for point estimates - use your own point estimates',
               'use prefilled estimates with uncertainty - possible to change','get empty table where you can assign your own estimates with uncertainty')
    
      if(input$intornot==inam[1]){
      TAB <- lapply(1:as.integer(input$numC), 
                    function (k) output[[paste0("valTab",k)]] <- renderRHandsontable({ 
                      rhandsontable(createmat_smallintervals_k(input,k)$smallmat, selectCallback = TRUE,digits =0)
                    }))
      }else if(input$intornot == inam[2]){
        TAB <- lapply(1:as.integer(input$numC), 
                      function (k) output[[paste0("valTab",k)]] <- renderRHandsontable({ 
                        rhandsontable(createmat_smallintervals_k(input,k)$smallmat*NA, selectCallback = TRUE,digits =0)
                      }))
      }else if(input$intornot == inam[3]){
        TAB <- lapply(1:as.integer(input$numC), 
                    function (k) output[[paste0("valTab",k)]] <- renderRHandsontable({ 
                      rhandsontable(createmat_k(input,k)$mat, selectCallback = TRUE,digits =0)
                    }))
      }else{
        TAB <- lapply(1:as.integer(input$numC), 
                      function (k) output[[paste0("valTab",k)]] <- renderRHandsontable({ 
                        rhandsontable(createmat_k(input,k)$mat*NA, selectCallback = TRUE,digits =0)
                      }))
      }
      
      output$decisionrulesbox <- renderUI({
        inam = c('use prefilled point estimates - possible to change',
                 'get empty table for point estimates - use your own point estimates',
                 'use prefilled estimates with uncertainty - possible to change',
                 'get empty table where you can assign your own estimates with uncertainty')
        
        if(input$intornot %in% inam[1:2]){
          decbox = radioButtons(inputId = "desrul", inline = FALSE, 
                                label = "Pick a decision rule!",
                                choices = c("Choose the fruit with the best score (Maximising)" = 1,
                                            "Choose the fruit with a score good enough, here above 5 (Satisficing)" = 2), selected = 1)
        }else{
          decbox =  radioButtons(inputId = "desrul", inline = FALSE, label = "Pick a suitable decision rule!", 
                                 choices = c("Choose the fruit with the highest worst score (Maximin rule)" = 1, 
                                             "Choose the fruit with the highest best score (Maximax rule)" = 2, 
                                             "Choose the fruit with the highest mid score (Hurwicz rule)" = 3, 
                                             "Choose the fruit for which the difference between its worst score is smallest 
                             in comparison to best possible score seen over all fruits (Minimax regret rule)" = 4), selected = 1)
        }
        return(decbox)
      })
      })
    
    
   
    
    
    observeEvent(input$valdone,{
      output$progressbar <- renderPlot({
        input$valdone # Re-run when button is clicked
        
        withProgress(message = 'calculating... ', value = 0, {
          # Number of times we'll go through the loop
          n <- 10
          
          for (i in 1:n) {
            # Increment the progress bar, and update the detail text.
            incProgress(1/n)#detail = paste("Doing part", i))
            # Pause for 0.1 seconds to simulate a long computation.
            Sys.sleep(0.1)
            if(i != n){plot(c(0,1),c(0,1),axes=FALSE,typ="n",xlab="",ylab="")}else{
              plot(c(0,1),c(0,1),axes=FALSE,typ="n",xlab="",ylab="",main="go to next page")}
          }
          #plot(c(0,1),c(0,1),axes=FALSE,typ="n",xlab="",ylab="")
          #text(0.5,0.5,"go to next page")
        })
     })
    })
    
    observeEvent(input$desrul,{
      output$progressbar <- renderPlot({
        plot(c(0,1),c(0,1),axes=FALSE,typ="n",xlab="",ylab="")
      })
    })
    
    observeEvent(input$intornot,{
      output$progressbar <- renderPlot({
        plot(c(0,1),c(0,1),axes=FALSE,typ="n",xlab="",ylab="")
      })
    })
    
    observeEvent(input$valdone,{
      AltName <- vector("list", as.integer(input$numAlt))
      fruits = c("apple","banana", "orange")
      for(i in 1:as.integer(input$numAlt)){
        if(i < 4){
          AltName[[i]] <- list(textInput(inputId = paste0("NameA",i),
                                         label = paste0("Fruit ", i),
                                         value = fruits[i]))
        }else{
          AltName[[i]] <- list(textInput(inputId = paste0("NameA",i),
                                         label = paste0("Fruit ", i),
                                         value = paste0("A ",i)))
        }
      }
      alt.namn <- sapply(1:as.integer(input$numAlt), function(i) input[[paste0("NameA",i)]])
      
    })
    
    
    observeEvent(input$valdone,{ 
      matr <- vector("list", 1)
      for(i in 1){
        tempmat <- hot_to_r(input[[paste0("valTab",1)]])
        
        if(dim(tempmat)[2]==1){
          tempmat = cbind(tempmat-0.05,tempmat+0.05)
          for(j in 2:as.integer(input$numC)){
            temp2 = hot_to_r(input[[paste0("valTab",j)]])
            tempmat <- cbind(tempmat,temp2-0.05,temp2+0.05)
          }
        }else{
        for(j in 2:as.integer(input$numC)){
          tempmat <- cbind(tempmat,hot_to_r(input[[paste0("valTab",j)]]))
        }
        }
        matr[[i]] <- tempmat
      }
      
      MMx <- c(rep("a",input$numC))
      Wx <- c(rep((1/as.integer(input$numC)), as.integer(input$numC)))
      
     
      Lx <- vector("list", 1)
      for(j in 1){
        Lx[[j]] <- matr[[j]][, seq(1, (as.integer(input$numC)*2-1), by = 2)]
      }
      
      Ux <- vector("list", 1)
      for(j in 1){
        Ux[[j]] <- matr[[j]][, seq(2, as.integer(input$numC)*2, by = 2)]
      }
      
      MMx <- lapply(1:as.integer(input$numC), function(i){input[[paste0("mm",i)]]})
        Wx = 1:as.integer(input$numC)
        for(i in 1:as.integer(input$numC)){
          Wx[i] <- input[[paste0("we",i)]]
        }
        Wx = Wx/sum(Wx)
      
      scen.namn <-  ''
      
      alt.namn <- sapply(1:as.integer(input$numAlt), function(i) input[[paste0("NameA",i)]])
      TOPSIS.list <- lapply(1, function(x) impact_calc(Lx[[x]],Ux[[x]], Wx,input))
      
      transTOPSIS.list <- lapply(1:length(TOPSIS.list), function(x) t(TOPSIS.list[[x]])) 
      names(TOPSIS.list) <- scen.namn
      
      
      maximin.list <- lapply(1:length(TOPSIS.list), function(x) maximin_grafer(TOPSIS.list[[x]],alt.namn))
      maximax.list <- lapply(1:length(TOPSIS.list), function(x) maximax_grafer(TOPSIS.list[[x]], alt.namn))
      hurwicz0.25.list <- lapply(1:length(TOPSIS.list), function(x) hurwicz_grafer(0.25,TOPSIS.list[[x]],alt.namn))
      hurwicz0.5.list <- lapply(1:length(TOPSIS.list), function(x) hurwicz_grafer(0.5,TOPSIS.list[[x]],alt.namn))
      regret.list <- lapply(1:length(TOPSIS.list), function(x) minimaxregret_grafer(TOPSIS.list[[x]],alt.namn))
    
      inam = c('use prefilled point estimates - possible to change',
               'get empty table for point estimates - use your own point estimates',
               'use prefilled estimates with uncertainty - possible to change',
               'get empty table where you can assign your own estimates with uncertainty')
      
      if(input$intornot %in% inam[1:2]){
      
        output$dom_plot <- renderPlot({
        scen.titel <- sapply(1, function(i) input[[paste0("NameS",i)]])
        
        color.dom <- brewer.pal(n=max(3,as.integer(input$numAlt)),name="Dark2")
        borderdefault = rep(0,as.integer(input$numAlt))
        xlimval = range(0,unlist(lapply(TOPSIS.list,range)),10)## 
        par(mfrow=c(1,1),cex.axis = 1.2, cex.lab = 1.2)
        for(i in 1){
        #if(as.numeric(input$desrul) == 1){
            best = maximin_rule(TOPSIS.list[[i]])
        #}else{
        #if(as.numeric(input$desrul) == 2){
              best2 = which(TOPSIS.list[[i]][,1]>5)
        #}
         borderwithblack = borderdefault
         if(length(best)>0){borderwithblack[best] = 1}else{borderwithblack[best2] = 1}
         
          boxplot(t(TOPSIS.list[[i]])[,as.integer(input$numAlt):1],     # Byter plats pa axlarna
                  horizontal = TRUE,       # Liggande boxplot
                  main = 'Good fruit experience score',             # Rubriker 
                  boxfill = color.dom, 
                  medlty = 0,
                  boxwex = 0.3,
                  boxlty = borderwithblack[as.integer(input$numAlt):1],
                  border = "#ff0000",
                  lwd = 2,
                  staplelty = 0,
                  xlab = '',
                  ylim = xlimval)
          grid()
          text(y=as.integer(input$numAlt)+1-best,x=mean(TOPSIS.list[[i]][best,]),pos = 4,'best',col = 'red')
          vcol = c('NA','red')
            abline(v = 5, col = vcol[as.numeric(input$desrul)])
            if(as.numeric(input$desrul)!=1){
          text(y=as.integer(input$numAlt)+1-0.5,x=5,pos=4,labels='good enough',col = 'red')
            }
         } 
            
        }) #end dom_plot
        output$dec_alt <- renderUI({
        #  radioButtons(inputId = "alts", inline = FALSE, label = 'Candidate fruits', choiceNames = alt.namn, choiceValues = 1:length(alt.namn),
        #               selected = 1)
        })
        
        output$sens_pie <- renderPlot({
          SA.list <- lapply(1, function(x) sens_calc(Lx[[x]],Ux[[x]], Wx,input))
          save(SA.list,file='sa.Rdata')
          id = dim(SA.list[1][[1]])[1] #as.numeric(input$alts)
          par(mfrow = c(1,2))
          pie(Wx,col = 1 + 1:length(SA.list[[1]][id,]),main = 'weight on each \n criteria', 
              labels = names(SA.list[[1]][1,]))
          })
      
        }else{
        output$dom_plot <- renderPlot({
        scen.titel <- sapply(1, function(i) input[[paste0("NameS",i)]])
        
        color.dom <- brewer.pal(n=max(3,as.integer(input$numAlt)),name="Dark2")
        borderdefault = rep(0,as.integer(input$numAlt))
        xlimval = range(0,unlist(lapply(TOPSIS.list,range)),10)## 
        
        par(mfrow=c(1,1),cex.axis = 1.2, cex.lab = 1.2) 
        for(i in 1){
        if(input$desrul == "1"){
          best = maximin_rule(TOPSIS.list[[i]])
             }else if(input$desrul == "2"){
               best = maximax_rule(TOPSIS.list[[i]])
           }else if(input$desrul == "3"){
             best = h_rule(TOPSIS.list[[i]],0.5)
           }else{
           best = regret_rule(TOPSIS.list[[i]])
           }
          borderwithblack = borderdefault
          if(length(best)>0){borderwithblack[best] = 1}
          
          boxplot(t(TOPSIS.list[[i]])[,as.integer(input$numAlt):1],     # Byter plats pa axlarna
                  horizontal = TRUE,       # Liggande boxplot
                  main = 'Good fruit experience score',             # Rubriker 
                  boxfill = color.dom, 
                  medlty = 0,
                  boxwex = 0.3,
                  boxlty = borderwithblack[as.integer(input$numAlt):1], 
                  border = "#ff0000",
                  lwd = 2,
                  staplelty = 0,
                  xlab = '',
                  ylim = xlimval)
          grid()
       text(y=as.integer(input$numAlt)+1-best+0.5,x=mean(TOPSIS.list[[i]][best,]),'best',col = 'red') 
        }
      }) #end dom_plot
        output$dec_alt <- renderUI({
          radioButtons(inputId = "alts", inline = FALSE, label = 'Candidate fruits', choiceNames = alt.namn, choiceValues = 1:length(alt.namn),
                       selected = 1)
        })
        
        output$sens_pie <- renderPlot({
          SA.list <- lapply(1, function(x) sens_calc(Lx[[x]],Ux[[x]], Wx,input))
        
          id = as.numeric(input$alts)
          par(mfrow = c(1,2))
          pie(Wx,col = 1 + 1:length(SA.list[[1]][id,]),main = 'weight on each \n criteria', 
              labels = names(SA.list[[1]][1,]))
          pie(SA.list[[1]][id,], col = 1 + 1:length(SA.list[[1]][id,]),
              main = 'relative contribution \n to uncertainty \n by each criteria')
        })
      }
      
  })

}

    
  shinyApp(ui = ui, server = server)
