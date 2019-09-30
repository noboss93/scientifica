library(shiny)
library(shinyWidgets)
library(shinyhelper)
library(shinyjs)
library(ggplot2)
library(shinythemes)
library(xtable)

ui <- navbarPage(title = "Scientifica", theme = shinytheme("spacelab"), footer = "Erstellt von Noah Bosshart",
                

# Studie UI ------------------------------------------------------------------
tabPanel(title = "Enten Studie", useShinyjs(),
         sidebarLayout(
           sidebarPanel(
             tags$head( # Spezifische CSS befehle welche für das gesammte Dokument verwendet wird
               tags$style(HTML("hr{border-top: 1px solid #95a5a6}
                               #healthy{background-image: radial-gradient(#753f3f00 20%, transparent 20%),                                                                       
                                                          radial-gradient(#ff000000 20%, transparent 20%);
                                                          background-color:#3353B7;
                                                          border-color:#3353B7;
                                                          margin:auto;
                                                          display:block}
                               #healthy:hover{background-color:#0028A5}
                               #ill{background-image: radial-gradient(#FF0000 20%, transparent 20%),
                                                      radial-gradient(#FF0000 20%, transparent 20%);
                                                      background-position:0 0, 14px 13px;
                                                      background-size: 30px 30px;
                                                      background-color:#3353B7;
                                                      border-color:#3353B7;
                                                      background-repeat: repeat;
                                                      margin:auto;
                                                      display:block
                                                      }
                               #ill:hover{background-color:#0028A5}
                               #addfive{margin:auto; 
                                        display:block}
                               #clear{margin:auto;
                                      display:block}
                               #clear2{margin:auto;
                                      display:block}
                               #clear3{margin:auto;
                                      display:block}
                               #clear:hover{background-color:#D10000}
                               .swal-button--cancel{background-color:#95a5a6;
                                                    border-color:#95a5a6;
                                                    color:#FFFFFF}
                               .swal-button--confirm{background-color:#D10000;
                                                    border-color:D10000}
                               .swal-footer{text-align:center}
                               table.table.shiny-table.table-.spacing-s{margin:auto} 
                               .shiny-table.spacing-s > tbody > tr > td{padding-bottom:20px}
                               .shiny-notification {
                                              width: 300px;
                                              position: fixed;
                                              top: 32% ;
                                              left: 48%;
                                              opacity: 1}
                               .shiny-notification-close{display:none}"))),
             fluidRow(
               column(width = 6,
                      actionButton(inputId = "healthy", 
                                   label = "Blaue gesunde Ente",
                                   width = "100%", 
                                   style = "color: #FFFFFF; height:75px"),
                      br(),
                      actionButton(inputId = "ill",
                                   label = "Blaue kranke Ente",
                                   width = "100%",
                                   style = "color: #FFFFFF; height:75px")),
               column(width = 6, tableOutput(outputId = "percent"))),
             hr(),
             fluidRow(
               actionButton(inputId = "addfive",
                            label = "Um 5 ergänzen",
                            width = "93%"),
               br(),
               column(width = 6,
                      actionButton(inputId = "random", 
                                   label = "Zufälliger Pfad",
                                   width = "100%")),
               column(width = 6,
                      actionButton(inputId = "critical",
                                   label = "Kritischer Fall",
                                   width = "100%"))),
             hr(),
             fluidRow(
               actionButton(inputId = "clear",
                            label = "Löschen",
                            width = "40%"))#, 
                                # nicht Anzuzeigen, kann genutzt werden um einen Seed zu 
                                # finden der "interessant" ist.
                                #fluidRow( 
                                #  sliderInput(inputId = "seed",
                                #              label = "choose seed",
                                #              min = 1,
                                #              max = 2000,
                                #              value = 106)
                                #)
             ),
           mainPanel(plotOutput(outputId = "graph"))
           )
         ),

# Simulation UI -----------------------------------------------------------
navbarMenu(title = "Simulationen",
           tabPanel(title = "Daten neu simulieren",
                    sidebarLayout(
                      sidebarPanel( # Butten zur Datengenerierung
                        selectInput(inputId = "nobs",
                                    label = "Wähle Stichprobengrösse",
                                    choices = c(50, 100, 200, 500, 1000),
                                    multiple = FALSE),
                        actionButton(inputId = "gen", 
                                     label = "Daten simulieren",
                                     width = "100%"),
                        br(),
                        hr(),
                        tableOutput(outputId = "counttable"),
                        hr(),
                        fluidRow(
                          actionButton(inputId = "clear2",
                                       label = "Löschen",
                                       width = "40%")),
                        width = 3),
                      mainPanel(plotOutput(outputId = "simgraph"))
                      )),
           tabPanel(title = "Datensatz verwenden",
                    sidebarLayout(
                      sidebarPanel( # Butten zur Datengenerierung
                        selectInput(inputId = "nobs2",
                                    label = "Wähle Stichprobengrösse",
                                    choices = c(50, 100, 200, 500, 1000),
                                    multiple = FALSE),
                        actionButton(inputId = "gen2",
                                     label = "Daten simulieren",
                                     width = "100%"),
                        br(),
                        hr(),
                        tableOutput(outputId = "counttable2"),
                        hr(),
                        fluidRow(
                          actionButton(inputId = "clear3",
                                       label = "Löschen",
                                       width = "40%")),
                        width = 3),
                      mainPanel(plotOutput(outputId = "simgraph2"))
                      )
                    )
           )
)


# Server ------------------------------------------------------------------
server <- function(input, output, session) {

# Studie Server -----------------------------------------------------------
  
  # Erstellen eines reaktiven Werten
  nobs = 60
  prob <- 0.5
  pvals <- reactiveValues(count = c(NA)) 
  sample <- reactiveValues(healthy = rep(0, nobs/2), ill = rep(1, nobs/2))
  click <- reactiveValues(count = 0, clear = 0, clear2 = 0)
  blue <- reactiveValues(healthy = 0, ill = 0)
    
  # Plotten des Graphen
  output$graph <- renderPlot({
    plot(1:length(pvals$count), pvals$count, type = "b", ylim = c(0,1), 
         xlim = c(1,60), ylab = "p-Werte", xlab = "Stichprobengrösse",
         main = "Verlauf des p-Wertes")
    abline(h = 0.05, col = "red")
    })
   
  # Plotten der Tabelle, welche die Prozentzahlen angibt
  m <- reactive({
    if(blue$healthy == 0 & blue$ill == 0){
    temp <- matrix(c(paste(0,"%"),
                     paste(0,"%")), nrow = 2) # if else ()
    colnames(temp) <- c("Verhältnis der gezogenen Enten")
    temp
    } else {
      temp <- matrix(c(paste(round(blue$healthy / (blue$healthy + blue$ill) * 100, digits = 0),"%"),
                       paste(round(blue$ill / (blue$ill + blue$healthy) * 100, digits = 0),"%")), nrow = 2)
      colnames(temp) <- c("Verhältnis der gezogenen Enten")
      temp
      }
    })
  output$percent <- renderTable(m(), digits = 0, align = "c")
    
  # Bei jedem Klick auf einen "BlueButton" wird die dazugehörige Variable +1 gerechnet
  observeEvent(input$healthy, {
    if(click$count < 60){
      blue$healthy <- blue$healthy + 1
      sample$healthy <- sample$healthy[-1]
      }
    })
    
  observeEvent(input$ill, {
    if(click$count < 60){
    blue$ill <- blue$ill + 1
    sample$ill <- sample$ill[-1]
    }
  })
    
  # Funktion bei der für jede einzelne Ziehung den p-Wert berechent wird
  observeEvent(input$ill,{
    if(click$count < 60){
       click$count <- click$count + 1
       pvals$count[click$count] <- binom.test(blue$ill, blue$ill + blue$healthy,
                                              p = prob, alternative = "less")$p.value
      }
    })
    
  observeEvent(input$healthy,{
    if(click$count < 60){
       click$count <- click$count + 1
       pvals$count[click$count] <- binom.test(blue$ill, blue$ill + blue$healthy,
                                              p = prob, alternative = "less")$p.value
       }
    })
    
    # Funktion bei der von den noch vorhandenen Enten 5 ausgewählt werden und 
    # jeweils jedesmal den p-Wert berechnet wird.
    observeEvent(input$addfive, {
      fullsample <- c(sample$ill, sample$healthy)
      observation <- fullsample[sample(length(sample$ill) + length(sample$healthy))]
      if(click$count < 60){
      
      if(length(fullsample) != 0){
        
      if(length(fullsample)<5){
        temp <- length(fullsample)
      } else {
        temp <- 5
      }
      
      for (i in 1:temp){
        draw <- observation[1]
        click$count <- click$count + 1
        
        if(draw == 0) {
          blue$healthy <- blue$healthy + 1
          sample$healthy <- sample$healthy[-1]
          } else {
          blue$ill <- blue$ill + 1
          sample$ill <- sample$ill[-1]
          }
        
        fullsample <- c(sample$ill, sample$healthy)
        observation <- fullsample[sample(length(sample$ill) + length(sample$healthy))]
        
        pvals$count[click$count] <- binom.test(blue$ill, blue$ill + blue$healthy,
                                               p = prob, alternative = "less")$p.value
      }
      }
      }
    })
    
    # Funktion zur erstellung eines Zufälligen Pfades mit 40 Ziehungen
    observeEvent(input$random, {
      blue$healthy <- 0
      blue$ill <- 0
      pvals$count <- c(0)
      click$count <- 0
      sample$healthy = rep(0, nobs/2) 
      sample$ill = rep(1, nobs/2)
      fullsample <- c(sample$ill, sample$healthy)
      observation <- fullsample[sample(length(sample$ill) + length(sample$healthy))]
      
      for (i in 1:length(fullsample)){
        draw <- observation[i]
        click$count <- click$count + 1
        if(draw == 0){
          blue$healthy <- blue$healthy + 1
        } else {
          blue$ill <- blue$ill + 1
        }
        pvals$count[click$count] <- binom.test(blue$ill, blue$ill + blue$healthy,
                                               p = prob, alternative = "less")$p.value
      }
    })
    
    # Funktion zur erstellung eines kritischen Pfades mit 60 Ziehungen
    observeEvent(input$critical, {
      
      blue$healthy <- 0
      blue$ill <- 0
      pvals$count <- c(0)
      click$count <- 0
      sample$healthy = rep(0, nobs/2) 
      sample$ill = rep(1, nobs/2)
      fullsample <- c(sample$ill, sample$healthy)
      set.seed(17)
      observation <- fullsample[sample(length(sample$ill) + length(sample$healthy))]
      
      for (i in 1:length(fullsample)){
        draw <- observation[i]
        click$count <- click$count + 1
        if(draw == 0){
          blue$healthy <- blue$healthy + 1
        } else {
          blue$ill <- blue$ill + 1
        }
        pvals$count[click$count] <- binom.test(blue$ill, blue$ill + blue$healthy,
                                               p = prob, alternative = "less")$p.value
      }
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
        blue$healthy <- 0
        blue$ill <- 0
        pvals$count <- c(NA)
        click$count <- 0
        sample$healthy = rep(0, nobs/2) 
        sample$ill = rep(1, nobs/2)
      }
    })
    
# Simulation Server -------------------------------------------------------
    
    
    # Definition von (reaktiven) Werten
    prob <- 0.5
    niter <- 1000
    percent <- reactiveValues(sample = c(0,0))
    
    
    # Tabelle mit den Angaben wie viele p-Werte unter 0.05 sind
    msim <- reactive({if(click$clear == 0){
      temp <- matrix(c(paste(0, "%"),paste(0, "%")), ncol = 1)
                    colnames(temp) <- c("Anz. p < 5%")
                    rownames(temp) <- c("Irgendwo im Verlauf:", "Bei gesamter Stichprobe:")
                    temp
    } else {
      temp <- matrix(c(paste(percent$sample[1]*100, "%"),paste(percent$sample[2]*100, "%")), ncol = 1)
                      colnames(temp) <- c("Anz. p < 5%")
                      rownames(temp) <- c("Irgendwo im Verlauf:", "Bei gesamter Stichprobe:")
                      temp
      
    }
                      })
    
    
    output$counttable <- renderTable(msim(), rownames = TRUE, digits = 1)
    
    # Funktion zur Berechnung von p-Werten von EINER Stichprobe mit Grösse "nobs"
    makepath_random_enten <- function(nobs){
      
      health <- ill <- 0
      counter_any <- counter_end <- 0
      
      prob <- 0.5
      
      pvals <- numeric(nobs)
      for (i in 1:nobs){
        draw <- rbinom(1,1,prob=prob)
        if(draw == 0) {
          health <- health + 1
        } else { 
          ill <- ill + 1 
        }
        pvals[i] <- binom.test(ill, ill+health, p = prob, alternative = "less")$p.value
      }
      return(pvals)
    }
    
    # Erstellung eines Dataframes mit "niter" Zeilen und "n" Spalten, wobei jede Zeile 
    # den p-Werten einer Ziehung entspricht
    compute_dataframe_random_enten <- function(niter, n, seed){
      
      counter_any <- counter_end <- 0
      dataset <- data.frame()
  
      withProgress(message = "Stichproben werden simuliert:", value = 0, min = 0, max = niter, {
      set.seed(seed)
      for (i in 1:niter){
        dataset <- data.frame(rbind(c(makepath_random_enten(n)), dataset))
        incProgress(amount = 1, detail = paste(i, " / 1000"))
      }
      return(dataset)
      })
    }
    
    # Defaultplot der Simulation
    output$simgraph <- renderPlot({
      par(oma=c(4,4,3,.1))
      par(mfrow = c(3,3), mai=c(0,0,0,0))
      for (i in 1:9){
        plot(0, 0, type = "l", ylim = c(-.1,1.1), xlim = c(-1,55),
             yaxt=ifelse(i%in%c(1,4,7),'s','n'),xaxt=ifelse(i>6,'s','n'), las = 1)
        abline(h = 0.05, col = "red")
      }
      mtext(side=2,line=2.8, outer=T, "p-Wert")
      mtext(side=1,line=2.8, outer=T, "Stichprobengrösse")
      mtext(side=3,line=1.2, outer=T, cex=1.2, "Verlauf des p-Wertes")
    })
    
    
    # Plotten von x Graphen unter berücksichtigung, dass genügend "interessante" darunter sind.
    # Angaben der Prozentzahlen wie häufig ein p-Wert < 5% vorkommt.
   
    observeEvent(input$gen, {
      disable("gen")
      click$clear <- click$clear + 1
      nobs <- as.numeric(input$nobs)
      
      seed <- sample(1000:2000, 1)
      counter_any <- 0
      counter_end <- 0
      
      #Simulieren der Daten und speichern in einem Dataframe
      dataframe <- compute_dataframe_random_enten(niter, nobs, seed)
      colnames(dataframe) <- c(1:input$nobs)
      
      # Prüft in jeder Zeile des Dataframe, ob irgendwo ein Wert < 0.05 ist
      critical <- c()
      critical <- apply(dataframe, 1, function(x){
                    if (min(x, na.rm = TRUE) < 0.05){
                      return(TRUE)
                    } else {
                      return(FALSE)
                    }
                  })
      
      # Prüft in jeder Zeile, ob irgendwo ein Wert < 0.05 ist und zählt diese
      counter_any <- sum(apply(dataframe, 1, function(x){
                        if (min(x, na.rm = TRUE) < 0.05){
                          return(1)
                        } else {
                          return(0)
                        }
        }))
      
      # Prüft in jeder Zeile, ob am ENDE ein Wert von < 0.05 ist und zählt diese
      counter_end <- sum(apply(dataframe, 1, function(x){
        if (x[nobs] < 0.05){
          return(1)
        } else {
          return(0)
        }
        
        
      }))
      
      # Jeder Stelle an der TRUE steht wird ihre Position in einen Vektor gespeichert 
      # (Position = Kritische Stichprobe in Dataframe)
      critrows <- which(critical == TRUE)
      
      # Zufälliges ziehen von 7 normalen Stichproben und 2 kritischen Stichproben und 
      # anschliessendes randomisieren der Reihenfolge
      rows_to_plot <- dataframe[sample(1:niter,7),]
      critrows <- dataframe[sample(critrows,2),]
      rows_to_plot <- rbind(rows_to_plot,critrows)
      rows_to_plot <- rows_to_plot[sample(1:9,9),]
      
      # Berechnung der Auftretenshäufigkeiten von p < 0.05 irgendwo und am Ende der Stichproben
      percent$sample <- c(counter_any/niter, counter_end/niter)
      
      output$simgraph <- renderPlot({
        par(oma=c(4,4,3,.1))
        par(mfrow = c(3,3), mai=c(0,0,0,0))
        for (i in 1:9){
          plot(1:nobs, rows_to_plot[i,], type = "l", ylim = c(-.1,1.1), xlim = c(-1,as.numeric(nobs)+5),
               yaxt=ifelse(i%in%c(1,4,7),'s','n'),xaxt=ifelse(i>6,'s','n'), las = 1)
          abline(h = 0.05, col = "red")
        }
        mtext(side=2,line=2.8, outer=T, "p-Wert")
        mtext(side=1,line=2.8, outer=T, "Stichprobengrösse")
        mtext(side=3,line=1.2, outer=T, cex=1.2, "Verlauf des p-Wertes")
      })
      
      enable("gen")
    })
    
    
# Funktion zur Löschung aller Werte durch klick auf den "Lösch" Button
observeEvent(input$clear2, {
  confirmSweetAlert(
    session = session,
    inputId = "confirm2",
    btn_labels = c("Abbrechen", "Bestätigen"),
    type = "warning",
    title = "Daten wirklich löschen?",
    danger_mode = FALSE
  )
})


# Bestätigung der Löschung aller reaktiven Werte & Reset der renderPlot Funktion
observeEvent(input$confirm2, {
  if (isTRUE(input$confirm2)){
    click$clear <- 0
    
    output$simgraph <- renderPlot({
      par(oma=c(4,4,3,.1))
      par(mfrow = c(3,3), mai=c(0,0,0,0))
      for (i in 1:9){
        plot(0, 0, type = "l", ylim = c(-.1,1.1), xlim = c(-1,55),
             yaxt=ifelse(i%in%c(1,4,7),'s','n'),xaxt=ifelse(i>6,'s','n'), las = 1)
        abline(h = 0.05, col = "red")
      }
      mtext(side=2,line=2.8, outer=T, "p-Werte")
      mtext(side=1,line=2.8, outer=T, "Stichprobengrösse")
      mtext(side=3,line=1.2, outer=T, cex=1.2, "Verlauf des p-Wertes")
    })
  }
})




# Simaltion mit Datensatz -------------------------------------------------


# Laden aller nötigen Datensätze
sample50 <- readRDS(file = "sample50.rds")
sample100 <- readRDS(file = "sample100.rds")
sample200 <- readRDS(file = "sample200.rds")
sample500 <- readRDS(file = "sample500.rds")
sample1000 <- readRDS(file = "sample1000.rds")
percentvalues <- readRDS(file ="percentvalues.rds")
critrow50 <- readRDS("critrow50.rds")
critrow100 <- readRDS("critrow100.rds")
critrow200 <- readRDS("critrow200.rds")
critrow500 <- readRDS("critrow500.rds")
critrow1000 <- readRDS("critrow1000.rds")

# Definition von (reaktiven) Werten
prob <- 0.5
niter <- 1000
percent <- reactiveValues(sample = c(0,0))

# Tabelle mit den Angaben wie viele p-Werte unter 0.05 sind
msim2 <- reactive({if(click$clear2 == 0){
  temp <- matrix(c(paste(0, "%"),paste(0, "%")), ncol = 1)
  colnames(temp) <- c("Anz. p < 5%")
  rownames(temp) <- c("Irgendwo im Verlauf:", "Bei gesamter Stichprobe:")
  temp
} else {
  temp <- matrix(c(paste(percent$sample[1]*100, "%"),paste(percent$sample[2]*100, "%")), ncol = 1)
  colnames(temp) <- c("Anz. p < 5%")
  rownames(temp) <- c("Irgendwo im Verlauf:", "Bei gesamter Stichprobe:")
  temp
  
}
})


output$counttable2 <- renderTable(msim2(), rownames = TRUE, digits = 1)

# Erstellen der Prozentzahlen
compute_stats_random_enten <- function(niter, n, df){
  
  counter_any <- counter_end <- 0
  
  for (i in 1:niter){
    counter_any <- counter_any + ifelse(min(df[i,], na.rm = TRUE) < 0.05, 1, 0)
    counter_end <- counter_end + ifelse(df[i,n] < 0.05, 1, 0)
  }
  return(c(counter_any/niter, counter_end/niter))
}

# Defaultplot der Simulation
output$simgraph2 <- renderPlot({
  par(oma=c(4,4,3,.1))
  par(mfrow = c(3,3), mai=c(0,0,0,0))
  for (i in 1:9){
    plot(0, 0, type = "l", ylim = c(-.1,1.1), xlim = c(-1,55),
         yaxt=ifelse(i%in%c(1,4,7),'s','n'),xaxt=ifelse(i>6,'s','n'), las = 1)
    abline(h = 0.05, col = "red")
  }
  mtext(side=2,line=2.8, outer=T, "p-Werte")
  mtext(side=1,line=2.8, outer=T, "Stichprobengrösse")
  mtext(side=3,line=1.2, outer=T, cex=1.2, "Verlauf des p-Wertes")
})

# Plotten von x Graphen unter berücksichtigung, dass genügend "interessante" darunter sind.
# Angaben der Prozentzahlen wie häufig ein p-Wert < 5% vorkommt.

# Checkt welche Stichprobengrösse ausgewählt ist und setzt entsprechendes Dataset fest
observeEvent(input$gen2, {
  
  click$clear2 <- click$clear2 + 1
  nobs <- as.numeric(input$nobs2)
  
  dataframe <- switch(as.character(nobs),
                      "50" = sample50,
                      "100" = sample100,
                      "200" = sample200,
                      "500" = sample500,
                      "1000" = sample1000)
  
  # Checkt welche Stichprobengrösse ausgewählt ist und setzt entsprechende Prozente fest
  percentvalues <- switch(as.character(nobs),
                          "50" = percentvalues[1,],
                          "100" = percentvalues[2,],
                          "200" = percentvalues[3,],
                          "500" = percentvalues[4,],
                          "1000" = percentvalues[5,])
  
  percent$sample <- percentvalues
  
  # Checkt welche Stichprobengrösseausgewählt ist und setzt die entsprechenden kritischen Zeilen fest.
  critrows <- switch(as.character(nobs),
                     "50" = critrow50,
                     "100" = critrow100,
                     "200" = critrow200,
                     "500" = critrow500,
                     "1000" = critrow1000)
  
  # Plotten der Graphen
  
  rows_to_plot <- dataframe[sample(1:niter,7),]
  rows_crit <- dataframe[sample(critrows,2),]
  
  rows_to_plot <- rbind(rows_to_plot,rows_crit)
  rows_to_plot <- rows_to_plot[sample(1:9,9),]
  
  output$simgraph2 <- renderPlot({
    par(oma=c(4,4,3,.1))
    par(mfrow = c(3,3), mai=c(0,0,0,0))
    for (i in 1:9){
      plot(1:nobs, rows_to_plot[i,], type = "l", ylim = c(-.1,1.1), xlim = c(-1,as.numeric(nobs)+5),
           yaxt=ifelse(i%in%c(1,4,7),'s','n'),xaxt=ifelse(i>6,'s','n'), las = 1)
      abline(h = 0.05, col = "red")
    }
    mtext(side=2,line=2.8, outer=T, "p-Werte")
    mtext(side=1,line=2.8, outer=T, "Stichprobengrösse")
    mtext(side=3,line=1.2, outer=T, cex=1.2, "Verlauf des p-Wertes")
  })
})

# Funktion zur Löschung aller Werte durch klick auf den "Lösch" Button
observeEvent(input$clear3, {
  confirmSweetAlert(
    session = session,
    inputId = "confirm3",
    btn_labels = c("Abbrechen", "Bestätigen"),
    type = "warning",
    title = "Daten wirklich löschen?",
    danger_mode = FALSE
  )
})

# Bestätigung der Löschung aller reaktiven Werte & Reset der renderPlot Funktion
observeEvent(input$confirm3, {
  if (isTRUE(input$confirm3)){
    click$clear2 <- 0
    
    output$simgraph2 <- renderPlot({
      par(oma=c(4,4,3,.1))
      par(mfrow = c(3,3), mai=c(0,0,0,0))
      for (i in 1:9){
        plot(0, 0, type = "l", ylim = c(-.1,1.1), xlim = c(-1,55),
             yaxt=ifelse(i%in%c(1,4,7),'s','n'),xaxt=ifelse(i>6,'s','n'), las = 1)
        abline(h = 0.05, col = "red")
      }
      mtext(side=2,line=2.8, outer=T, "p-Werte")
      mtext(side=1,line=2.8, outer=T, "Stichprobengrösse")
      mtext(side=3,line=1.2, outer=T, cex=1.2, "Verlauf des p-Wertes")
    })
  }
})


}

shinyApp(ui = ui, server = server)
