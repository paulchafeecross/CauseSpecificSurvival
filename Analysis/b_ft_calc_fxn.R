b_ft_calc <- function(model){
  HazAb.mat <- model$BUGSoutput$sims.list$Haz_a
  HazB.mat <- model$BUGSoutput$sims.list$Haz_b
  HazL.mat <- model$BUGSoutput$sims.list$Haz_l
  
  ft.mat <- matrix(NA,  nrow = dim(HazL.mat)[1],  ncol = dim(HazL.mat)[2])
  
  ft.mat[,1]<- HazL.mat[,1]
  
  D <- dim(HazL.mat)[2]
  for(i in 2:D){
    ft.mat[,i] <- HazL.mat[,i]*exp(-apply(HazB.mat[,1:i], 1, sum)) 
  }
  
  ftstats <- apply(ft.mat, 2, quantile, probs = c(0.025, 0.25, 0.75, 0.975))
  ftmedian <- apply(ft.mat, 2, quantile, probs = c(0.5))
  ftmean <-  apply(ft.mat, 2, mean)
  return(list(median = ftmedian,  mean = ftmean, stats = ftstats))
}

ft_cum_calc <- function(ft_vec){
  n <- length(ft_vec)
  cum <- rep(NA, n)
  cum[1]<- ft_vec[1]
  for(i in 2:n){
    cum[i]<- sum(ft_vec[1:i])
  }
  per <- cum/cum[n]
  row5 <- which.min(abs(per - 0.05)) # index for the one closest to 0.05
  row25 <- which.min(abs(per - 0.25)) # index for the one closest to 0.25
  row75 <- which.min(abs(per - 0.75)) # index for the one closest to 0.75
  row95 <- which.min(abs(per - 0.95)) # index for the one closest to 0.95
  
  return(list(cum = cum, per = per, row5 = row5, row95 = row95,
              row25 = row25,row75 = row75))
}