
# Eine Stichprobe simulieren ----------------------------------------------

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


# Datensatz simulieren ----------------------------------------------------

compute_dataframe_random_enten <- function(niter, n, seed){
  
  counter_any <- counter_end <- 0
  dataset <- data.frame()
  set.seed(seed)
  for (i in 1:niter){
    dataset <- data.frame(rbind(c(makepath_random_enten(n)), dataset))
    colnames(dataset) <- c(1:n)
  }
  return(dataset)
}

sample50 <- data.frame(compute_dataframe_random_enten(1000, 50, 116))
sample100 <- data.frame(compute_dataframe_random_enten(1000, 100, 116))
sample200 <- data.frame(compute_dataframe_random_enten(1000, 200, 119))
sample500 <- data.frame(compute_dataframe_random_enten(1000, 500, 116))
sample1000 <- data.frame(compute_dataframe_random_enten(1000, 1000, 116))

saveRDS(sample50, "sample50.rds")
saveRDS(sample100, "sample100.rds")
saveRDS(sample200, "sample200.rds")
saveRDS(sample500, "sample500.rds")
saveRDS(sample1000, "sample1000.rds")


# Prozente ----------------------------------------------------------------

compute_stats_random_enten <- function(niter, n, df){
  
  counter_any <- counter_end <- 0
  
  for (i in 1:niter){
    counter_any <- counter_any + ifelse(min(df[i,], na.rm = TRUE) < 0.05, 1, 0)
    counter_end <- counter_end + ifelse(df[i,n] < 0.05, 1, 0)
  }
  return(c(counter_any/niter, counter_end/niter))
}

value50 <- compute_stats_random_enten(1000, 50, sample50)
value100 <- compute_stats_random_enten(1000, 100, sample100)
value200 <- compute_stats_random_enten(1000, 200, sample200)
value500 <- compute_stats_random_enten(1000, 500, sample500)
value1000 <- compute_stats_random_enten(1000, 1000, sample1000)

percentvalues <- data.frame(rbind(value50, value100, value200, value500, value1000))

saveRDS(percentvalues,"percentvalues.rds")


# Kritische Werte ---------------------------------------------------------
kritische_werte <- function(df){
critical <- c()

for (i in 1:1000){
  if (min(df[i,], na.rm = TRUE) < 0.05){
    critical[i] <- TRUE
  } else {
    critical[i] <- FALSE
  }
}
critrows <- which(critical == TRUE)
return(critrows)
}

critrow50 <- kritische_werte(sample50)
critrow100 <- kritische_werte(sample100)
critrow200 <- kritische_werte(sample200)
critrow500 <- kritische_werte(sample500)
critrow1000 <- kritische_werte(sample1000)

saveRDS(critrow50, "critrow50.rds")
saveRDS(critrow100, "critrow100.rds")
saveRDS(critrow200, "critrow200.rds")
saveRDS(critrow500, "critrow500.rds")
saveRDS(critrow1000, "critrow1000.rds")






