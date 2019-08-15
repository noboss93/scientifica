library(shiny)
library(shinyWidgets)
library(shinyhelper)
library(ggplot2)
library(shinythemes)
library(xtable)

ui <- navbarPage(title = "Scientifica", footer = "Erstellt von Noah Bosshart", theme = shinytheme("flatly"),
                

# Studie UI ------------------------------------------------------------------

                 tabPanel(title = "Enten Studie",
                          h4("Worum geht's?"),
                          p("Seit längerer Zeit sind die hiesigen gelben Gummienten von einer speziellen Krankheit befallen, wodurch
                            sie rote Punkte auf ihren Bäuchen erhalten. Ein grosses Genetiklabor hat nun das genetische Material
                            der gelben Gummienten so verändert, dass sie nun immun gegen den Erreger dieser Krankheit sein sollten. 
                            Allerdings veränderte sich auch die farbe der Gummmienten, so dass diese nun alle blau sind."),
                          p("Es stellt sich die Frage, ob die blauen Gummienten auch aus statistischer Sicht weniger häufig 
                            an dieser Krankheit leiden als die gelben Gummienten? Um die Aussage des Genetiklabors zu überprüfen, 
                            führen wir folgendes Experiment durch:"), 
                          h4("Vorgehen"),
                          p("Aus der Grundgesamtheit aller gelben und blauen Gummienten wurden jeweils 20 Gummienten zufällig 
                            ausgewählt. Ihre Aufgabe ist es nun jeweils nacheinander eine Gummiente aus einem dieser Wasserbecken
                            zu ziehen und zu Überprüfen, ob diese Gummiente an dieser Krankheit leidet. Nach jeder Ziehung
                            erfassen Sie durch einen Klick auf den dazugehörigen Knopf die Gummiente im System."),
                          br(),
                          sidebarLayout(
                              sidebarPanel(
                                tags$head( # Spezifische CSS befehle welche für das gesammte Dokument verwendet wird
                                  tags$style(HTML("
                                                   #yellow_health{background-color:#FBE651; 
                                                                  border-color:#FBE651}
                                                   #yellow_health:hover{background-color:#FEDE00}
                                                   #yellow_ill{background-image: radial-gradient(#FF0000 20%, transparent 20%),
                                                                      radial-gradient(#FF0000 20%, transparent 20%);
                                                                  background-position:0 0, 20px 18px;
                                                                  background-size: 30px 30px;
                                                                  background-color:#FBE651; 
                                                                  border-color:#FBE651}
                                                   #yellow_ill:hover{background-color:#FEDE00}
                                                   #blue_health{background-color:#3353B7; border-color:#3353B7}
                                                   #blue_health:hover{background-color:#0028A5}
                                                   #blue_ill{background-image: radial-gradient(#FF0000 20%, transparent 20%),
                                                                      radial-gradient(#FF0000 20%, transparent 20%);
                                                                  background-position:0 0, 20px 18px;
                                                                  background-size: 30px 30px;
                                                                  background-color:#3353B7; 
                                                                  border-color:#3353B7}
                                                   #blue_ill:hover{background-color:#0028A5}
                                                   .swal-button--confirm{background-color: #2B65EC}
                                                   
                                                   "))
                                ), 
                                  fluidRow( # Zählknöpfe in zwei Reihen und pro Reihe zwei Spalten
                                      fluidRow(
                                          column(width = 6,
                                                 actionButton(inputId = "yellow_health",
                                                              label = "Gelbe gesunde Ente",
                                                              width = "100%",
                                                              style = "color: #000000")),
                                          column(width = 6,
                                                 actionButton(inputId = "blue_health",
                                                              label = "Blaue gesunde Ente",
                                                              width = "100%",
                                                              style = "color: #FFFFFF"))
                                          ),
                                      br(),
                                      fluidRow(
                                          column(width = 6, 
                                                 actionButton(inputId = "yellow_ill",
                                                              label = "Gelbe kranke Ente",
                                                              width = "100%",
                                                              style = "color: #000000")),
                                          column(width = 6,
                                                 actionButton(inputId = "blue_ill",
                                                              label = "Blaue kranke Ente",
                                                              width = "100%",
                                                              style = "color: #FFFFFF"))
                                          )
                                      ),
                                  hr(), # Tabelle und Clear-Button
                                  fluidRow(
                                        helper(
                                          shiny_tag = tableOutput(outputId = "table"),
                                          icon = "question-circle",
                                          colour = "#0028A5",  #0028A5 UZH Blau
                                          type = "inline",
                                          size = "m",
                                          title = "Information zur Stichprobengrösse", 
                                          content = "Um sicherzustellen dass genügend Datenpunkte in die Berechnung der 
                                          Prüfgrösse einfliessen, wird im System jede einzelne Ziehung als fünf 
                                          Ziehungen gewertet."),
                                      br(),
                                      column(width = 6,
                                             actionButton(inputId = "clear", 
                                                             label = "Werte löschen",
                                                             width = "100%")),
                                      column(width = 6,
                                             actionButton(inputId = "random",
                                                          label = "Zufällige Ziehung",
                                                          width = "100%"))
                                      )
                                  ),
                              mainPanel(
                                  helper(
                                    shiny_tag = plotOutput(outputId = "graph"),
                                    icon = "question-circle",
                                    colour = "#0028A5",
                                    type = "markdown",
                                    size = "l",
                                    title = "Informationen für Statistikinteressierte",
                                    content = "chitest")
                                  )
                              )
                          ),

# Simulation UI -----------------------------------------------------------

                 tabPanel(title = "Ergebnisse Simulieren",
                          sidebarLayout(
                              sidebarPanel( # Butten zur Datengenerierung
                                  actionButton(inputId = "gen", 
                                               label = "Daten simulieren",
                                               width = "100%"),
                                  br(),
                                  hr(),
                                  tableOutput(outputId = "counttable"), # Tabelle mit Infos bzgl. Anzahl P-Werte > 0.05
                                  width = 3
                              ),
                              mainPanel(
                                  plotOutput(outputId = "simgraph")
                              )
                          ))
)



# Server ------------------------------------------------------------------
server <- function(input, output, session) {

# Studie Server -----------------------------------------------------------
  observe_helpers(help_dir = "www/helpfiles" , withMathJax = TRUE)
  
    # Definition von zwei reaktiven Werten mit jeweils zwei Abstufungen
    yellow <- reactiveValues(health = 0, ill = 0)
    blue <- reactiveValues(health = 0, ill = 0)
    
    observeEvent(input$yellow_health, {
        yellow$health <- yellow$health + 5
    })
    
    observeEvent(input$yellow_ill, {
        yellow$ill <- yellow$ill + 5
    })
    
    observeEvent(input$blue_health, {
        blue$health <- blue$health + 5
    })
    
    observeEvent(input$blue_ill, {
        blue$ill <- blue$ill + 5
    })
    
    # Funktion zur Löschung aller Werte durch klick auf den "Lösch" Button
    observeEvent(input$clear, {
      confirmSweetAlert(
        session = session,
        inputId = "confirm",
        btn_labels = c("Abbrechen", "Bestätigen"),
        type = "warning",
        title = "Daten wirklich löschen?",
        danger_mode = FALSE
      )
    })
    
    # Bestätigung der Löschung aller reaktiven Werte & Reset der renderPlot Funktion
    observeEvent(input$confirm, {
      if (isTRUE(input$confirm)){
        yellow$health <- 0
        yellow$ill <- 0
        blue$health <- 0
        blue$ill <- 0
        pvals$count <- c(0)
        
        output$graph <- renderPlot({
          plot(1:length(pvals$count), pvals$count, type = "l", ylim = c(0,1), xlim = c(4,40), ylab = "P-Werte", xlab = "Stichprobengrösse",
               main = "Verlauf des P-Wertes")
          abline(h = 0.05, col = "red")
        })
      }
    })
    
    # Matrix, welche Angezeigt wird, mit Total für jede Spalte und Zeile
    m <- reactive({
        temp <- matrix(c(yellow$health/5,
                         yellow$ill/5,
                         sum(c(yellow$health/5, yellow$ill/5)),
                         blue$health/5,
                         blue$ill/5, 
                         sum(c(blue$health/5,blue$ill/5)),
                         sum(c(yellow$health/5, blue$health/5)),
                         sum(c(yellow$ill/5, blue$ill/5)),
                         sum(c(yellow$ill/5, blue$ill/5, yellow$health/5, blue$health/5))), 
                       ncol = 3, nrow = 3)
        colnames(temp) <- c("Gelbe Enten", "Blaue Enten", "Total")
        rownames(temp) <- c("Gesunde", "Kranke", "Total")
        temp
    })
    
    # 2x2 Matrix, welche für die Berechnung des Tests benötigt wird. (Wird NICHT angezeigt)
    mtest <- reactive({
        temp1 <- matrix(c(yellow$health,
                         yellow$ill,
                         blue$health,
                         blue$ill), 
                       ncol = 2, nrow = 2)
        colnames(temp1) <- c("Gelbe Enten", "Blaue Enten")
        rownames(temp1) <- c("Gesunde", "Kranke")
        temp1
    })
    
    # Erstellen eines reaktiven Vektors, welcher verwendet wird um die P-Werte abzuspeichern.
    pvals <- reactiveValues(count = c(0))
    
    # Sobald ein Eingabe Button gedrückt wird, wird die der Test basierend auf der neuen Tabelle berechnet. 
    observeEvent({   
        input$yellow_health
        input$yellow_ill
        input$blue_health
        input$blue_ill}, {

        try(pvals$count[sum(mtest())/5]  <-  prop.test(mtest(), alternative = "less")$p.value)
        #try(pvals$count[sum(mtest())/5]  <-  chisq.test(mtest())$p.value)
    })  
    
    # Plotten der Tabelle und des Graphen
    output$table <- renderTable(m(), rownames = 1, width = "100%", digits = 0)
    output$graph <- renderPlot({
        plot(1:length(pvals$count), pvals$count, type = "l", ylim = c(0,1), xlim = c(4,40),ylab = "P-Werte", xlab = "Stichprobengrösse",
             main = "Verlauf des P-Wertes")
        abline(h = 0.05, col = "red")
    })
    
    # Funktion zur generierung einer zufälligen Ziehung von 40 Enten mit zufälliger Reihenfolge
    observeEvent(input$random, {
      yellow$health <- 50
      yellow$ill <- 50
      blue$health <- 50
      blue$ill <- 50
      
      pvals <- rep(NA, niter)
      dat <- data.frame(gruppe = NULL, status = NULL)
      #set.seed(sample(1:20, 1, replace = FALSE))
      reihenfolge <- sample(1:niter, replace = FALSE)
      for(i in 1:niter){
        a <- ((reihenfolge[i]-1)*nclust)+1
        b <- reihenfolge[i]*nclust
        dat <- rbind(dat, alldat[a:b,])
        tab <- table(dat$gruppe, dat$status)
        try(pvals[i] <- prop.test(tab, alternative = "less")$p.value)
        #try(pvals[i] <- chisq.test(tab)$p.value)
      }
      
      output$graph <- renderPlot({
        plot(1:niter, pvals, type = "l", ylim = c(0,1), xlim = c(4,40), xlab = "Stichprobengrösse", ylab = "P-Werte", main = "Verlauf des P-Wertes")
        abline(h = 0.05, col = "red")
      })
      
    })

# Simulation Server -------------------------------------------------------

    niter <- 40
    nclust <- 5
    
    # Daten = Enten fixiert, nur Reihenfolge random
    gruppe <- as.factor(c(rep(1, (niter/2)*nclust), rep(2, (niter/2)*nclust)))
    levels(gruppe) <- c("gelb", "blau")
    status <- as.factor(c(rep(1, (niter/4)*nclust), rep(2, (niter/4)*nclust), rep(1, (niter/4)*nclust), rep(2, (niter/4)*nclust)))
    levels(status) <- c("gesund", "krank")
    alldat <- data.frame(gruppe = gruppe, status = status)
    results <- data.frame()
    counter_20 <- counter_40 <- counter_any <- 0 
    
    # Erstellung einer Matrix, welche später Angezeigt wird
    countM <- matrix(c(counter_20, counter_40, counter_any), ncol = 1)
    colnames(countM) <- c("Anzahl P-Werte < 0.05")
    rownames(countM) <- c("20 gezogene Enten", "40 gezogene Enten", "Beliebiger Anzahl gezogener Enten")
    
    output$counttable <- renderTable(countM, width = "100%", rownames = 1)
    
    # Generierung von 25 unterschiedlichen durchgängen, bei jenen jeweils 40 Enten gezogen werden.
    observeEvent(input$gen, {
            for(s in 1001:1025){
                pvals <- rep(NA, niter)
                dat <- data.frame(gruppe = NULL, status = NULL)
                #set.seed(s)
                reihenfolge <- sample(1:niter, replace = FALSE)
                for(i in 1:niter){
                    a <- ((reihenfolge[i]-1)*nclust)+1
                    b <- reihenfolge[i]*nclust
                    dat <- rbind(dat, alldat[a:b,])
                    tab <- table(dat$gruppe, dat$status)
                    try(pvals[i] <- prop.test(tab, alternative = "less")$p.value)
                    #try(pvals[i] <- chisq.test(tab)$p.value)
                }
                results <- rbind(results, pvals)
                counter_20 <- counter_20 + ifelse(pvals[20] < 0.05, 1, 0)
                counter_40 <- counter_40 + ifelse(pvals[40] < 0.05, 1, 0)
                counter_any <- counter_any + ifelse(min(pvals, na.rm = TRUE) < 0.05, 1, 0)
            }
        colnames(results) <- c(paste(rep("t", 40), c(1:40), sep = "."))
        results <- cbind(results, count = c(1:25))
        results_reshape <- reshape(data = results,
                                   varying = c(1:40),
                                   idvar = "count",
                                   direction = "long")
        colnames(results_reshape) <- c("count", "samplesize", "pvalues")
        
       output$simgraph <- renderPlot({
           ggplot(data = results_reshape, aes(y = pvalues, x = samplesize))+
            geom_line()+
            ylim(c(0,1))+
            geom_abline(intercept = 0.05, slope = 0, color = "red")+
            facet_wrap( ~ count)+
            ggtitle("Verlauf der P-Werte")+
            theme_classic()
       })
       countM <- matrix(c(counter_20, counter_40, counter_any), ncol = 1)
       colnames(countM) <- c("Anzahl P-Werte < 0.05")
       rownames(countM) <- c("20 gezogene Enten", "40 gezogene Enten", "Beliebiger Anzahl gezogener Enten")
      
       output$counttable <- renderTable(countM, width = "100%", rownames = 1)
          
    })
    
    
}


shinyApp(ui = ui, server = server)
