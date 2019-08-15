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
par(mfrow = c(5,5))
for(s in 1001:1025){
  pvals <- rep(NA, niter)
  dat <- data.frame(gruppe = NULL, status = NULL)
  set.seed(s)
  reihenfolge <- sample(1:niter, replace = FALSE)
  for(i in 1:niter){
    a <- ((reihenfolge[i]-1)*nclust)+1
    b <- reihenfolge[i]*nclust
    dat <- rbind(dat, alldat[a:b,])
    tab <- table(dat$gruppe, dat$status)
    try(pvals[i] <- prop.test(tab, alternative = "less")$p.value)
    #try(pvals[i] <- chisq.test(tab)$p.value)
  }
  
  pvals
  
  results <- rbind(results, pvals)
  
  counter_20 <- counter_20 + ifelse(pvals[20] < 0.05, 1, 0)
  counter_40 <- counter_40 + ifelse(pvals[40] < 0.05, 1, 0)
  counter_any <- counter_any + ifelse(min(pvals, na.rm = TRUE) < 0.05, 1, 0)
  #plot(1:niter, pvals, type = "l", ylim = c(0,1))
  #abline(h = 0.05, col = "red")
  
}

colnames(results) <- c(paste(rep("t", 40), c(1:40), sep = "."))
results <- cbind(results, inter = c(1:25))
results_reshape <- reshape(data = results,
                           varying = c(1:40),
                           idvar = "inter",
                           direction = "long")

ggplot(data = results_reshape, aes(y = t, x = time))+
  geom_line()+
  ylim(c(0,1))+
  geom_abline(intercept = 0.05, slope = 0, color = "red")+
  facet_wrap( ~ inter)+
  theme_classic()




counter_20 
counter_40 
counter_any

