seeds <- c(115:200)

crit_seed_finder <- function(niter, nobs,seeds){
   tempseeds <- c()
   count <- 0
   for (i in 1:length(seeds)){
   data <- compute_dataframe_random_enten(niter, nobs, seeds[i]) # Funktion muss aus Datengenerierung.R geladen werden
   count <- sum(apply(data, 1, function(x){
      if (x[nobs] < 0.05){
         return(1)
         } else {
         return(0)
         }
      }))
   if ((count / niter) >= 0.04){
      tempseeds[i] <- seeds[i]
       }
   }
   return(tempseeds)
}

crit_seeds 


critseed500 <- c(115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130)
critseed200 <- c(119, 121, 123, 125, 126, 130)
critseed100 <- c(115, 116, 117, 118 ,119, 121, 123, 124, 125, 126, 127, 128, 130)
critseed50  <- c(116, 131, 151, 157, 175, 177)

critcheck <- function(dataframe){
   value <- (sum(apply(dataframe, 1, function(x){
      if (x[50] < 0.05){
         return(1)
      } else {
         return(0)
      }
   })))/1000
   return(value)
}

critcheck_any <- function(dataframe){
   value <- (sum(apply(dataframe, 1, function(x){
      if (min(x, na.rm = TRUE) < 0.05){
         return(1)
      } else {
         return(0)
      }
   })))/1000
   return(value)
}

crit_50_1 <- compute_dataframe_random_enten(1000, 50, critseed50[1])
crit_50_2 <- compute_dataframe_random_enten(1000, 50, critseed50[2])
crit_50_3 <- compute_dataframe_random_enten(1000, 50, critseed50[3])
crit_50_4 <- compute_dataframe_random_enten(1000, 50, critseed50[4])
crit_50_5 <- compute_dataframe_random_enten(1000, 50, critseed50[5])
crit_50_6 <- compute_dataframe_random_enten(1000, 50, critseed50[6])
crit_50_7 <- compute_dataframe_random_enten(1000, 50, critseed50[7])
crit_50_8 <- compute_dataframe_random_enten(1000, 50, critseed50[8])


value50_1 <- critcheck_any(crit_50_1)
value50_2 <- critcheck_any(crit_50_2)
value50_3 <- critcheck_any(crit_50_3)
value50_4 <- critcheck_any(crit_50_4)
value50_5 <- critcheck_any(crit_50_5)
value50_6 <- critcheck_any(crit_50_6)
value50_7 <- critcheck_any(crit_50_7)
value50_8 <- critcheck_any(crit_50_8)

value50_1end <- critcheck(crit_50_1)
value50_2end <- critcheck(crit_50_2)
value50_3end <- critcheck(crit_50_3)
value50_4end <- critcheck(crit_50_4)
value50_5end <- critcheck(crit_50_5)
value50_6end <- critcheck(crit_50_6)
value50_7end <- critcheck(crit_50_7)
value50_8end <- critcheck(crit_50_8)



