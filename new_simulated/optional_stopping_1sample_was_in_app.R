############################
# App Tab 1 Entenergebnisse
############################

# aus Eingaben bzw. Simulation p-Wert berechnen

makepath_enten <- function(n){
 seeds <- c()
 health <- ill <- 0
 counter_any <- counter_end <- 0

 nobs <- 60
 fullSample <- rep(c(0,1), nobs/2)
 
 prob <- 0.5

 pvals <- numeric(nobs)
 
 for (s in 1:n){
 set.seed(s)
 observations <- fullSample[sample(nobs)] 
 # nicht zufaellig ziehen sondern nur Reihenfolge zufaellig!
 
 for (i in 1:nobs){
   draw <- observations[i]
   if(draw == 0) health <- health + 1
   else ill <- ill + 1 
   pvals[i] <- binom.test(ill, ill+health, p = prob, alternative = "less")$p.value
   ifelse(min(pvals, na.rm = TRUE) < 0.05, seeds[i] <- s, 0 )
 }
 
}
}



plotpath_stepwise_enten  <- function(myseed){
  
  health <- ill <- 0
  counter_any <- counter_end <- 0
  
  nobs <- 40
  fullSample <- c(rep(1,nobs/2),rep(0, nobs/2))
  
  prob <- 0.5
  
  pvals <- numeric(nobs)
  
  set.seed(myseed)
  observations <- fullSample[sample(nobs)] 
  # nicht zufaellig ziehen sondern nur Reihenfolge zufaellig!
  
  for (i in 1:nobs){
    draw <- observations[i]
    if(draw == 0) health <- health + 1
    else ill <- ill + 1 
    pvals[i] <- binom.test(ill, ill+health, p = prob, alternative = "less")$p.value
    
    plot(1:i, pvals[1:i], type = "l", ylim = c(0,1))
    abline(h = 0.05, col = "red")
    # wie kann man movie machen bzw. langsamer naechsten Wert plotten?
  }
  
}


# in App Tab 1:
# per Hand eingeben statt draw 
# 5 zusätzliche simulieren  
# ganzen Pfad simulieren
# kritischen Pfad zeigen, idealerweise als movie

par(mfrow=c(4,4))

for(i in 131:146){ 
  print(i)
  makepath_enten(i)
}

par(mfrow=c(1,1))
plotpath_stepwise_enten(106)



##################################
# App Tab 2 Simulationsergebnisse
##################################

makepath_random_enten <- function(nobs){
  
  health <- ill <- 0
  counter_any <- counter_end <- 0

  prob <- 0.5
  
  pvals <- numeric(nobs)
  
  #set.seed(myseed)
  
  for (i in 1:nobs){
    draw <- rbinom(1,1,prob=prob)
    if(draw == 0) {
      health <- health + 1
    } else { 
      ill <- ill + 1 
    }
      pvals[i] <- binom.test(ill, ill+health, p = prob, alternative = "less")$p.value
  }
    # plot(1:nobs, pvals, type = "l", ylim = c(0,1))
    # abline(h = 0.05, col = "red")
  return(pvals)
}

makepath_random_enten(60, 1231)

niter <- 1000
seeds <- sample(1:niter,1)

compute_stats_random_enten <- function(niter, n, seeds){
  
  counter_any <- counter_end <- 0
  
  for (i in 1:niter){
   pvals <- makepath_random_enten(n) 
   #print(pvals)
   counter_any <- counter_any + ifelse(min(pvals, na.rm = TRUE) < 0.05, 1, 0)
   counter_end <- counter_end + ifelse(pvals[n] < 0.05, 1, 0)
  }
  return(c(counter_any/niter, counter_end/niter))
}

compute_stats_random_enten(1000, 50, 5323)

n <- as.matrix(c(40, 100, 200),ncol=1)

apply(n,1,FUN=function(x)compute_stats_random_enten(1000,x,seeds))

# 40
# any 15%
# end 5%
# 100 
# any 22% 
# end 5%
# 200 
# any 25%
# end 4%
# 500
# any 30%
# end 4%
# 1000
# any 35%
# end 5%

# nochmal laufen lassen mit niter 10.000 fuer bessere Schaetzer der Wahrsch.

compute_dataframe_random_enten <- function(niter, n, seed){
  
  counter_any <- counter_end <- 0
  dataset <- data.frame()
  for (i in 1:niter){
    pvals <- makepath_random_enten(n) 
    dataset <- data.frame(rbind(c(makepath_random_enten(n, seed)), dataset))
    colnames(dataset) <- c(1:n)
  }
  return(dataset)
  
}

counter_any <- counter_any + ifelse(min(pvals, na.rm = TRUE) < 0.05, 1, 0)
counter_end <- counter_end + ifelse(pvals[n] < 0.05, 1, 0)

dataframe <- compute_dataframe_random_enten(1000, 50, 123123)

sample50 <- data.frame(compute_dataframe_random_enten(1000, 50))
sample100 <- data.frame(compute_dataframe_random_enten(1000, 100))
sample200 <- data.frame(compute_dataframe_random_enten(1000, 200))
sample500 <- data.frame(compute_dataframe_random_enten(1000, 500))
sample1000 <- data.frame(compute_dataframe_random_enten(1000, 1000))

saveRDS(sample40, "sample40.rds")
saveRDS(sample50, "sample50.rds")
saveRDS(sample100, "sample100.rds")
saveRDS(sample200, "sample200.rds")
saveRDS(sample500, "sample500.rds")
saveRDS(sample1000, "sample1000.rds")

