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
source('createmat_bus.R')
source('impact_calc.R')


ui <- dashboardPage(
  dashboardHeader(title = "Choosing bus technology under Uncertainty", titleWidth = 270),
  
  ## Skapar sideboardmenu med lank till de olika sidorna
  dashboardSidebar(width = 270,sidebarMenu(
    menuItem("Start", tabName = "info", icon = icon("info")),
    menuItem("Structure the decision problem", tabName = "strt", icon = icon("play")),
                     menuItem("Enter your weights on criteria", tabName = "wei", icon = icon("table")),
                     menuItem("Enter impacts on criteria", tabName = "vls", icon = icon("table")),
                     menuItem("Compare weighted impacts on criteria", tabName = "intdom",icon = icon("bar-chart")),
                     menuItem("Make a decision robust to uncertainty", tabName = "robdec", icon = icon("bar-chart"))
                   )),
  dashboardBody(
    shinyjs::useShinyjs(),
    ## Lankar till fil med css kod. Har kan man lagga till olika typsnitt, storlekar, farger osv.
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    
    tabItems(
      tabItem(tabName = "info",
              fluidRow(column(10,align = "center", titlePanel(title= "Decision making"))),
              fluidRow(column(10,align = "center", h4('This app demonstrates structured decision making using Multi Criteria Decision Analysis.'))),br(),
              fluidRow(column(10,align = "center", h4("You are guided through the steps a decision making process consising of a decision maker and her values, 
                                                      decision alternatives, certainty or uncertainty in the outcome of decisions on values, an idea of what is a good decision. 
                                                      The aim is to demonstrate what it could mean to make a decision under uncertainty."))),
              fluidRow(column(10,align = 'center', h3("Your task is to select a bus technology for a municipality!"))),
              fluidRow(column(10,align = 'center',h3("Good luck",col = 'red'))),
              #Choose your favourite fruit!))),
              fluidRow(align = 'center',div(
                img(src = 'bdiesel.jpg', height = '100px', width = '150px'),
                img(src = 'bgas.jpg', height = '100px', width = '150px'),
                img(src = 'bhydrogen.jpg', height = '100px', width = '150px'),
                img(src = 'bel.jpg', height = '100px', width = '150px')
              ))
              ),
      
      tabItem(
        tabName = "strt",                    
        ## Skapar inputs for antal kriterier, alternativ och scenarier 
        fluidRow(column(width = 2, align="center"),
                 column(width = 8, align="center",
                        br(), br(),
                        titlePanel("Decision analysis"), br(), br(),
                        div(style="display:inline-block", selectInput(inputId = "numAlt",label = "No. of alternatives",c( "6"), selected = 6, width= "140px")),
                        div(style="display:inline-block", selectInput(inputId = "numC",label = "No. of criteria",c("9"),selected = 9, width = "140px")),
                        div(style="display:inline-block", selectInput(inputId = "numScen",label = "No. of scenarios",c("1", "2", "3", "4"),selected = 1,width = "140px"))),
                 column(width = 2)),
        fluidRow( column(12, align="center",actionButton("fill",h5("Press when the alternatives, critiera and scenarios are identified!")), 
                   h5(textOutput("fini"))
        )),
        fluidRow(## Skapar inputs for namngivning av krit, alt, scen (skapas i server-koden)   
          column(4, uiOutput("namesAlt")), column(4, uiOutput("namesCrit")),column(3, uiOutput("namesScen"))
        )
      )#end tabitem starts
      , 
      tabItem(tabName = "wei",
            box(status = "success",  title = "Intervals", width = 10,
                fluidRow(column(5,uiOutput("viktning")))## Skapar viktningssliders
            )
      ),
## Skapar innehall pa sidan "enter intervals"
      tabItem(tabName = "vls",## text explaning what to do
              #fluidRow(column(10,textOutput("expl"), 
              fluidRow(column(12,h3("Impact on critera are evaluated using scores on a scale from 0 (worst) to 10 (best)"),
                              br(), br()  
              )),

              ## Skapar tabeller for ingangsvarden 
              fluidRow(column(12,box(status = "primary", width = 12, title = 'Scenario',
                                    h2(textOutput("ScenTit1")), rHandsontableOutput("valTab1"), 
                                    h2(textOutput("ScenTit2")), rHandsontableOutput("valTab2"), 
                                    h2(textOutput("ScenTit3")), rHandsontableOutput("valTab3"),
                                    h2(textOutput("ScenTit4")), rHandsontableOutput("valTab4"), 
                                    h2(textOutput("ScenTit5")), rHandsontableOutput("valTab5"), 
                                    h2(textOutput("ScenTit6")), rHandsontableOutput("valTab6"),
                                    h2(textOutput("ScenTit7")), rHandsontableOutput("valTab7"),
                                    h2(textOutput("ScenTit8")), rHandsontableOutput("valTab8")
              )
              ))
             
               
      ), ## end tabitem vls
      
## Skapar innehall pa sidan "compare utility"
      tabItem(tabName = "intdom",
              ## Skapar checkboxes for beslutsregler
              fluidRow(column(12,box(title = "Decision rules", width = 10, status = "success",
                                    radioButtons(inputId = "desrul", inline = FALSE, label = "Choose which decision rule to apply",
                                                       choices = c("Choose the tech with the highest worst score (Maximin rule)" = 1, 
                                                                   "Choose the tech with the highest best score (Maximax rule)" = 2, 
                                                                   "Choose the tech with the highest 1/4 score (Hurwicz rule 25)" = 3,
                                                                   "Choose the tech with the highest mid score (Hurwicz rule 50)" = 4,
                                                                   "Choose the tech for which the difference between its worst score is smallest 
                                                                   in comparison to best possible score seen over all fruits (Minimax regret rule)" = 5),
                                                 #        "Maximin" = 1, "Maximax" = 2, "Hurwicz a=0.25" = 3,
                                                #                   "Hurwicz a=0.50" = 4, "Minimax regret" = 5)
                                                 selected = 1)))),
          br(),
          fluidRow( column(12, align="center",actionButton("valdone",h5("Calculate weighted scores on critiera!"))#, 
                           #h3(textOutput("Got to compare utility and select decision"))
          )),
              fluidRow(column(12,plotOutput("dom_plot", width="100%", height = "600px"))),
          fluidRow(column(12,div(actionButton("gettip",h4("What to do next?")),h4(textOutput("tip1")))))
        ),#end tabitem intdom
      
      tabItem(tabName = "robdec",
              ## Skapar checkboxes for beslutsregler
              fluidRow(column(12, box(title = "Decision rules", width = 12, status = "success",
                                    #checkboxGroupInput(inputId = "desrulrob", inline = TRUE, label = "Choose which decision rule to apply",
                                    #                   choices = c("Maximin" = 1, "Maximax" = 2, "Hurwicz a=0.25" = 3,
                                    #                               "Hurwicz a=0.50" = 4, "Minimax regret" = 5),selected = c(1))))),
              checkboxGroupInput(inputId = "desrulrob", inline = FALSE, label = "Choose which decision rule to apply",
                                 choices = c("Choose the tech with the highest worst score (Maximin rule)" = 1, 
              "Choose the tech with the highest best score (Maximax rule)" = 2, 
              "Choose the tech with the highest 1/4 score (Hurwicz rule 25)" = 3,
              "Choose the tech with the highest mid score (Hurwicz rule 50)" = 4,
              "Choose the tech for which the difference between its worst score is smallest 
              in comparison to best possible score seen over all fruits (Minimax regret rule)" = 5),selected = c(1))))),
              
              #fluidRow( column(12, align="center", actionButton("calc","Do robust analysis"))), 
              ## Skapar frekvenstabell for hur manga ggr varje alternativ rankas pa viss ranking
              #fluidRow(column(12, #align ="right", 
              box(title = "Frequency of each alternative for each rank",solidHeader = TRUE,  width = 12,
                        status = "success", tableOutput("rankres")),
              ## Plottar grafen for ranking
              fluidRow(column(12,
              box(status = "success", solidHeader = TRUE, 
                                   title = "Ranking under different scenarios and decision rules",
                                   verbatimTextOutput("regerr"),
                                   plotOutput("topsis_plot", width = "700px", height = "570px"), br(), br())))
              
  
     
    ) #end robdec
)))#DashboardPage end

  
  server <- function(input, output) {
    
    observe({
      ## Skapar inputs for kriterienamn
      buscrit = c("Price","Funding","Infrastructure","Environ.Impact","Noise","Climate.Impact","Reach","Energy.Conversion","Market.Readyness")
      output$namesCrit <- renderUI({
        CritName <- vector("list", as.integer(input$numC))
        for(i in 1:as.integer(input$numC)){
          CritName[[i]] <- list(textInput(inputId = paste0("NameC",i),
                                          label = paste0("Name criteria ", i), 
                                          value = buscrit[i]))#paste0("C ",i)))
        }
        return(CritName)
        
      })
      
      ## Skapar inputs for alternativnamn
      bussystem = c("Diesel","CNG","Biogas","Hydrogen","Electricity.night.charge", "Electericity.interim.charge")
      output$namesAlt <- renderUI({
        AltName <- vector("list", as.integer(input$numAlt))
        for(i in 1:as.integer(input$numAlt)){
          AltName[[i]] <- list(textInput(inputId = paste0("NameA",i),
                                         label = paste0("Name alternative ", i),
                                         value = bussystem[i]))#paste0("A ",i)))
        }
        return(AltName)
        
      })
      
      ## Skapar inputs for scenarienamn
      bus_scen = c("Reference scenario","Alternative scenario")
      output$namesScen <- renderUI({
        ScenName <- vector("list", as.integer(input$numScen))
        for(i in 1:as.integer(input$numScen)){
          ScenName[[i]] <- list(textInput(inputId = paste0("NameS",i),
                                          label = paste0("Name scenario ", i),
                                          value = bus_scen[i]))#paste0("S ",i)))
        }
        return(ScenName)
        
      })
      
      ## Skapar inputs för viktningssliders
      output$viktning <- renderUI({
        if(as.integer(input$numC)==2){
          vikt <- sliderInput(inputId = "we", label = "Drag to put weight on criteria",
                              value = 0.5, min = 0, max = 1, round = FALSE)
        }else if(as.integer(input$numC)==3){
          vikt <- sliderInput(inputId = "we", label = "Drag to put weight on criteria",
                               value = c(0.33,0.66), min = 0, max = 1, round = FALSE)
        }else{
          vikt <- vector("list", as.integer(input$numC))
          for(i in 1:as.integer(input$numC)){
            vikt[[i]] <- list(sliderInput(inputId = paste0("we",i),
                                          label = paste0("Relative weight on criteria for ",buscrit[i]),
                                          value = 1, #1/as.integer(input$numC), 
                                          min = 0, max = 1, 
                                          round = FALSE))
          }
        }
       return(vikt)
      })
    })
    
    observeEvent(input$gettip,{
      output$tip1 <- renderText({"Go back and see what happens when you change the relative weights on criteria, you alter scores for impact
        on critera, you add more scenario or uncertainty in scores?"})
    })
    
    
    observeEvent(input$fill,{
      
      ## Text efter man tryckt pa enter pa forsta sidan
      output$fini <- renderText({"Go to the next pages to enter your weights on critera and explore the evaluated impact from bus technology on the criteria!"})
      
      ## Text for att forklara hur man fyller i tabell etc. pa andra sidan
      #output$expl <- renderText({"Add scores on a scale from 0 (worst) to 10 (best)!"})
      
      ## Tar fram tabell for att fylla i ingangsvarden med ratt antal rader och kolumner utefter
      ## de val man gjort pa forsta sidan.
      Stit <- lapply(1:as.integer(input$numScen),
                     function (i) output[[paste0("ScenTit",i)]] <- renderText({
                       input[[paste0("NameS",i)]]
                     }))
      
      TAB <- lapply(1:as.integer(input$numScen), 
                    function (k) output[[paste0("valTab",k)]] <- renderRHandsontable({ 
                      rhandsontable(createmat(input), selectCallback = TRUE)
                      # hot_cols(manualColumnResize = TRUE)
                    }))

    })
    
    observeEvent(input$valdone,{ 
      ## Hamtar varden fran tabellerna och andra val
      ## Lagger in vardena en matris for de nedre intervallgranserna och en matris for de hogre intervallgranserna
      ## U = upper, L = lower, MM = cost/benefit, W = viktning
      
      matr <- vector("list", as.integer(input$numScen))
      for(i in 1:as.integer(input$numScen)){
        matr[[i]] <- hot_to_r(input[[paste0("valTab",i)]])
      }
      
      MMx <- c(rep("a",input$numC))
      Wx <- c(rep((1/as.integer(input$numC)), as.integer(input$numC)))
      
      Lx <- vector("list", as.integer(input$numScen))
      for(j in 1:as.integer(input$numScen)){
        Lx[[j]] <- matr[[j]][, seq(1, (as.integer(input$numC)*2-1), by = 2)]
      }
      
      Ux <- vector("list", input$numScen)
      for(j in 1:as.integer(input$numScen)){
        Ux[[j]] <- matr[[j]][, seq(2, as.integer(input$numC)*2, by = 2)]
      }
      
      MMx <- lapply(1:as.integer(input$numC), function(i){input[[paste0("mm",i)]]})
      if(as.integer(input$numC)<4){
        Wx = c(input$we, 1- sum(input$we))
      }else{
        Wx = 1:as.integer(input$numC)
        for(i in 1:as.integer(input$numC)){
          Wx[i] <- input[[paste0("we",i)]]
        }
        Wx = Wx/sum(Wx)
      }
       # Scenarionamn
      scen.namn <-  sapply(1:as.integer(input$numScen), function(i) input[[paste0("NameS",i)]])
      
      # Altenativnamn
      alt.namn <- sapply(1:as.integer(input$numAlt), function(i) input[[paste0("NameA",i)]])
      
      # Skapa lista med TOPSIS-värden -------------------------------------------
      # TOPSIS.list <- int_topsis(Lx,Ux,Wx,MMx)
      TOPSIS.list <- lapply(1:as.integer(input$numScen), function(x) impact_calc(Lx[[x]],Ux[[x]], Wx, input))
      #TOPSIS.list <- lapply(1:as.integer(input$numScen), function(x) int_topsis(Lx[[x]],Ux[[x]], Wx, MMx,input))
      transTOPSIS.list <- lapply(1:length(TOPSIS.list), function(x) t(TOPSIS.list[[x]])) 
      names(TOPSIS.list) <- scen.namn
      ## Nar man trycker pa Done sa kors TOPSIS-koden och graferna plottas
      
      # Skapar beslutskriterielistor  ------------------------------------------
      maximin.list <- lapply(1:length(TOPSIS.list), function(x) maximin_grafer(TOPSIS.list[[x]],alt.namn))
      maximax.list <- lapply(1:length(TOPSIS.list), function(x) maximax_grafer(TOPSIS.list[[x]], alt.namn))
      hurwicz0.25.list <- lapply(1:length(TOPSIS.list), function(x) hurwicz_grafer(0.25,TOPSIS.list[[x]],alt.namn))
      hurwicz0.5.list <- lapply(1:length(TOPSIS.list), function(x) hurwicz_grafer(0.5,TOPSIS.list[[x]],alt.namn))
      regret.list <- lapply(1:length(TOPSIS.list), function(x) minimaxregret_grafer(TOPSIS.list[[x]],alt.namn))
    
      
      ## Rankingplot --------------------------------------------------------
      output$topsis_plot <- renderPlot({
        ## Ta fram "vinnande" alternativ per beslutskriterie och scenario ----------
        
        ## Skapa resultat-matrisen + namnge
        resultat <- matrix(rep(NA,5*length(TOPSIS.list)*length(alt.namn)),nrow=5*length(TOPSIS.list),ncol=length(alt.namn))
        radnamn <- rep(c("Maximin","Maximax","Hurwicz0.25", "Hurwicz0.5","MMRegret"),length(TOPSIS.list))
        
        colnames(resultat) <- alt.namn
        rownames(resultat) <- radnamn
        
        #Lägg till ranking
        
        count_k <- 1
        for(i in seq(1,5*length(TOPSIS.list),by = 5)){
          
          for(j in 1:length(alt.namn)){ 
            resultat[i,j] <- grep(alt.namn[j], colnames(maximin.list[[count_k]]))
            resultat[i+1,j] <- grep(alt.namn[j], colnames(maximax.list[[count_k]]))
            resultat[i+2,j] <- grep(alt.namn[j], colnames(hurwicz0.25.list[[count_k]]))
            resultat[i+3,j] <- grep(alt.namn[j], colnames(hurwicz0.5.list[[count_k]]))
            resultat[i+4,j] <- grep(alt.namn[j], colnames(regret.list[[count_k]]))
            
          }
          count_k <- count_k + 1
        }
        ## Skapar fargvektor. OBS! Finns ett visst max antal färger men de är fler än 8
        color.res <- brewer.pal(n=max(3,ncol(resultat)),name="Dark2")
        
        ## Valjer ut de beslutsregler som ska anvandas
        if(as.integer(input$numScen > 1)){
          regelval <- append(as.integer(input$desrulrob), sapply(1:(as.integer(input$numScen)-1),function (i) as.integer(input$desrulrob)+5*i, simplify = TRUE))
        }else{
          regelval <- as.integer(input$desrulrob)
        }
        
        beslutres <- resultat[regelval,]
        ## Skapar vektorer far att beskriva axelvarden. 
        regel.namn <- row.names(beslutres)
        plot(1:length(regel.namn), as.integer(input$numAlt)+1-beslutres[,1], 
             ylim = c(0.5,(as.integer(input$numAlt)+1)),     # Hur langt grafen spanner i y-led
             axes = FALSE,             ## Ta bort om vi vill ha ram
             xaxt = "n", 
             yaxt = "n", 
             type = "l", 
             #pch = 19 , # vid type = "o"
             col = color.res[1], 
             lwd = 3,
             lty = 1,
             ylab = "Rank",
             xlab = "Decision rules & Scenario " 
             # main = "Ranking under different scenarios and decision rules"
        )
        
        
        ## lty.count ar en parameter som beskriver 
        lty.count <- 1
        
        for(s in 2:ncol(resultat)){
          # Finns bara upp till 6 linjetyper men 12 farger sa har roteras 
          # linjetyperna vid behov. 
          lty.count <- lty.count + 1
          if(lty.count > 6){
            lty.count <- 1
          }
          
          lines(1:length(regel.namn), as.integer(input$numAlt)+1-beslutres[,s],
                type = "l", 
                # pch = 5,    # vid type = "o"
                col = color.res[s], 
                lwd = 3, 
                lty = 1)#s)
        }
        
        ## Fixa axlar 
        axis(1, at=1:length(regel.namn), regel.namn, las=3, cex.axis = 1.0)
        axis(2, at=1:as.integer(input$numAlt), c(length(alt.namn):1))
        
        ## Drar scenario-linjer och skriver ut scenario
        if(as.integer(input$numScen > 1)){
          scen.lin <- seq(length(input$desrulrob)+0.5, length(regel.namn)-length(input$desrulrob)+0.5,by=length(input$desrulrob))
          text.plats <- seq((length(input$desrulrob)/2+0.5),length(regel.namn),by=length(input$desrulrob))
          abline(v = scen.lin, col = "gray")
          
        }else{
          
          scen.lin <- 0
          text.plats <- length(input$desrulrob)/2+0.5
          abline(v = scen.lin, col = "gray")
        }
        
        for(k in 1:length(TOPSIS.list)){
          text(text.plats[k],as.integer(input$numAlt)+0.3,scen.namn[k],cex=1.0)
        }
        
        
        ## Teckenforklaring 
        legend("top", 
               inset = 0,                  # Marginal till kant
               legend = alt.namn, 
               col = color.res,
               #lty = 1:length(alt.namn),   # vid type = "o"
               lty = rep(1,length(alt.namn)),   # vid type = "o"
               lwd = 2, 
               #horiz = TRUE, 
               cex = 0.7)
        #}
        
        ## Skriver ut hur manga ganger varje alternativ hamnar pa vilken ranking
        output$rankres <- renderTable({
          ranklist <- matrix(0,nrow = as.integer(input$numAlt), ncol = as.integer(input$numAlt))
          ranklist <- apply(beslutres, 2, function(x) table(factor(x, levels=1:as.integer(input$numAlt))))
          
          colnames(ranklist) <- sapply(1:as.integer(input$numAlt), function(i) input[[paste0("NameA",i)]])
          rownames(ranklist) <- sapply(1:as.integer(input$numAlt), function(i) paste0("Rank ",i))
          return(ranklist)
        }, rownames = TRUE, colnames = TRUE)
      
      })
      
      ## Intervalldominansplot --------------------------------------------------------
      output$dom_plot <- renderPlot({
        ## Skapar rutnats-parameter som ar tre plottar brett och anpassar i 
        ## vertikal-led utefter antal scenarier. 
        c <- as.integer(input$numScen)        # Ev. kan man l?ta detta vara n?got anv?ndaren best?mmer sj?lv. 
        r <- ceiling(length(TOPSIS.list)/c)
        ## Skapar vektor med ratt andel scenariorubrik-namn. 
        scen.titel <- sapply(1:as.integer(input$numScen), function(i) input[[paste0("NameS",i)]])
        
        ## Skapar vektor med farger far linjerna. OBS! Finns bara 12 olika farger i setet. 
        color.dom <- brewer.pal(n=max(3,as.integer(input$numAlt)),name="Dark2")
        borderdefault = rep(0,as.integer(input$numAlt))
        xlimval = range(10,0,unlist(lapply(TOPSIS.list,range)))## man skulle kunna skriva en funktion som plottar boxarna och som gör det så att det blir samma skala på xaxeln
        
        par(mfrow=c(r,c),cex.axis = 1.5)                
        for(i in 1:length(TOPSIS.list)){
         if(input$desrul == 1){
          best = maximin_rule(TOPSIS.list[[i]])
            }else if(input$desrul == 2){
              best = maximax_rule(TOPSIS.list[[i]])
          }else if(input$desrul == 3){
            best = h_rule(TOPSIS.list[[i]],0.25)
          }else if(input$desrul == 4){
            best = h_rule(TOPSIS.list[[i]],0.5)
          }else{
          best = regret_rule(TOPSIS.list[[i]])
          }
          borderwithblack = borderdefault
          borderwithblack[best] = 2
          
          
          boxplot(t(TOPSIS.list[[i]])[,as.integer(input$numAlt):1],     # Byter plats pa axlarna
                  horizontal = TRUE,       # Liggande boxplot
                  main = scen.titel[i],             # Rubriker 
                  boxfill = color.dom, 
                  medlty = 0,
                  boxwex = 0.3,
                  boxlty = borderwithblack[as.integer(input$numAlt):1], 
                  names = 6:1,
                  border = "#ff0000",
                  lwd = 2,
                  staplelty = 0,
                  xlab = 'Utility score - the higher the better',
                  ylim = xlimval)
          grid()
          text(rep(1,6),6:1,alt.namn)
          
        }
      }) #end dom_plot
  })
}
  
  shinyApp(ui = ui, server = server)
  