# FUnctions to plot some of the results from the VIT models 
# using the BUGS output

# Plot the baseline hazard 
plot_baseline <- function(model, MainLab, offset, LWD, Ylim, Xlim, legendflag, Ylab, FmoTixs){
 
  if(missing(MainLab)) {MainLab <- " "}
  if(missing(LWD)) {LWD <- 1}
  if(missing(Ylab)) {"Total hazard h(t)"}
  if(missing(Xlim)) {Xlim <-  Xlim = as.Date(c('2013/02/01', '2013/07/01'))}
  
  if(missing(offset)) {
    t <- seq(1, dim(model$BUGSoutput$sims.list$Haz_b)[2], 1)
  }
  
  t <- as.Date('01/01/2013', '%d/%m/%Y') + seq(1,dim(model$BUGSoutput$sims.list$Haz_b)[2],1) + offset 
  
  #INPUT = Bugs model output with "Haz_b" as an output
  Haz_b.mat <- model$BUGSoutput$sims.list$Haz_b
  bstats <- apply(Haz_b.mat, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  
  if(missing(Ylim)) {Ylim <- c(0, max(bstats[4,]))}
  
  bmean <-  model$BUGSoutput$mean$Haz_b
  maxb <- max(bstats)
  plot(t, bmean, xlab = '', xaxt = "n", ylab = Ylab, bty = "n",
    main = MainLab, type = 'l', col = 'grey', lwd = 2, ylim = Ylim, xlim = Xlim, tck = 0.02)
  lines(t, bstats[3,], col = "black", lwd = LWD)#median
  lines(t, bstats[1,], col = "grey", lwd = LWD, lty = 2)
  lines(t, bstats[5,], col = "grey", lwd = LWD, lty = 2)
  # axis labels
  if(missing(FmoTixs)) {axis(1, tck = 0.02)}
  else {
    if(FmoTixs == TRUE){
      labs <- c("1 Jan", "1 Feb", "1 Mar", "1 Apr", "1 May", "1 Jun", "1 Jul")
      m <- as.Date(c('01/01/2013', '01/02/2013', "01/03/2013", "01/04/2013","01/05/2013",
                     "01/06/2013", "01/07/2013"), '%d/%m/%Y')
      axis(1, at = m, labels = labs, tck = 0.02)
    }
    if(FmoTixs == FALSE){axis(1, tck = 0.02)}
  }
  

  if(legendflag == TRUE) {
    legend("topright", c("mean", "median", "2.5%", "97.5%"), lty = c(1,1,2,2), 
           col = c("grey", "black", "grey", "grey"), bty = "n")
  }
  
}

# Plot the probability of abortion 
plot_Pabort <- function(model, MainLab, offset, LWD, Ylim, Xlim, legendflag, Ylab){
  if(missing(MainLab)) {MainLab <- " "}
  if(missing(LWD)) {LWD <- 1}
  if(missing(Ylab)) {Ylab <- 'Probability of abortion'}
  if(missing(Xlim)) {Xlim <-  Xlim = as.Date(c('2013/02/01', '2013/07/01'))}
  
  if(missing(offset)) {
    t <- seq(1, dim(model$BUGSoutput$sims.list$Haz_b)[2], 1)
  }
  t <- as.Date('01/01/2013', '%d/%m/%Y') + seq(1,dim(model$BUGSoutput$sims.list$Haz_b)[2],1) + offset 
  
  #INPUT:
  #Bugs model output with "prob.ab" as an output
  prob.ab.mat <- model$BUGSoutput$sims.list$prob.ab
  pstats <- apply(prob.ab.mat, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  
  if(missing(Ylim)) {Ylim <- c(0, max(pstats[4,]))}
  
  # axis labels
  m <- as.Date(c('01/01/2013', '01/02/2013', "01/03/2013", "01/04/2013","01/05/2013",
                 "01/06/2013", "01/07/2013"), '%d/%m/%Y')
  labs <- c("1 Jan", "1 Feb", "1 Mar", "1 Apr", "1 May", "1 Jun", "1 Jul")
  
  pmean <-  model$BUGSoutput$mean$prob.ab
  plot(t, pmean, xlab = '', ylab = Ylab, type = 'l', col = 'grey', lwd = 2, 
       main = MainLab, ylim = Ylim, xlim = Xlim, xaxt = "n", bty = "n", tck = 0.02)
  lines(t, pstats[3,], col = "black", lwd = LWD)#median
  lines(t, pstats[1,], col = "grey", lwd = LWD, lty = 2)
  lines(t, pstats[5,], col = "grey", lwd = LWD, lty = 2)
  axis(1, at = m, labels = labs, tck = 0.02)
  if(legendflag == TRUE) {
    legend("topright", c("mean", "median", "2.5%", "97.5%"), lty = c(1,1,2,2), 
           col = c("grey", "black", "grey", "grey"), bty = "n")
  }
}

# Plot the birth hazard
plot_hazLB <- function(model, MainLab, offset, LWD, Ylim, Xlim, legendflag, Ylab, FmoTixs){
  if(missing(MainLab)) {MainLab <- " "}
  if(missing(LWD)) {LWD <- 1}
  if(missing(Ylab)) {Ylab <- expression("Birth rate" ~ h[c==2](t))}
  if(missing(offset)) {t <- seq(1, dim(model$BUGSoutput$sims.list$Haz_b)[2], 1)}
  if(missing(Xlim)) {Xlim <-  Xlim = as.Date(c('2013/02/01', '2013/07/01'))}
  
  t <- as.Date('01/01/2013', '%d/%m/%Y') + seq(1,dim(model$BUGSoutput$sims.list$Haz_b)[2],1) + offset 
  
  #INPUT = Bugs model output with "Haz_l" as an output
  HazLB.mat <- model$BUGSoutput$sims.list$Haz_l
  LBstats <- apply(HazLB.mat, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  
  if(missing(Ylim)) {Ylim <- c(0, max(LBstats[4,]))}
  
  HazLBmean <-  model$BUGSoutput$mean$Haz_l
  
  # PLOT
  plot(t, HazLBmean, xlab = '', ylab = Ylab, xaxt = "n", bty = "n", 
       type = 'l', col = 'grey', lwd = 2, ylim = Ylim, xlim = Xlim, main = MainLab, tck = 0.02)
  
  lines(t, LBstats[3,], col = "black", lwd = LWD)#median
  lines(t, LBstats[1,], col = "grey", lwd = LWD, lty = 2)
  lines(t, LBstats[5,], col = "grey", lwd = LWD, lty = 2)
  # axis labels
  if(missing(FmoTixs)) {axis(1, tck = 0.02)}
    else {
      if(FmoTixs == TRUE){
        labs <- c("1 Jan", "1 Feb", "1 Mar", "1 Apr", "1 May", "1 Jun", "1 Jul")
        m <- as.Date(c('01/01/2013', '01/02/2013', "01/03/2013", "01/04/2013","01/05/2013",
                       "01/06/2013", "01/07/2013"), '%d/%m/%Y')
        axis(1, at = m, labels = labs, tck = 0.02)
      }
      if(FmoTixs == FALSE){axis(1, tck = 0.02)}
    }
  
  #legend
  if(legendflag == TRUE) {
    legend("topright", c("mean", "median", "2.5%", "97.5%"), lty = c(1,1,2,2), 
           col = c("grey", "black", "grey", "grey"), bty = "n")
  }
}

# Plot the abortion hazard
plot_hazAb <- function(model, MainLab, offset, LWD, Ylim, Xlim, legendflag, Ylab, FmoTixs){
  if(missing(MainLab)) {MainLab <- " "}
  if(missing(LWD)) {LWD <- 1}
  if(missing(Ylab)) {Ylab = expression("Abortion rate" ~ h[c==1](t))}
  if(missing(Xlim)) {Xlim <-  Xlim = as.Date(c('2013/02/01', '2013/07/01'))}
  if(missing(offset)) {
    t <- seq(1, dim(model$BUGSoutput$sims.list$Haz_b)[2], 1)
  }
  t <- as.Date('01/01/2013', '%d/%m/%Y') + seq(1,dim(model$BUGSoutput$sims.list$Haz_b)[2],1) + offset 
  
  #INPUT = Bugs model output with "Haz_l" as an output
  HazAb.mat <- model$BUGSoutput$sims.list$Haz_a
  Abstats <- apply(HazAb.mat, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  
  if(missing(Ylim)) {Ylim <- c(0, max(Abstats[4,]))}
  
  HazAbmean <-  model$BUGSoutput$mean$Haz_a
  plot(t, HazAbmean, xlab = '', xaxt = "n", bty = "n", ylab = Ylab, xlim = Xlim, 
       type = 'l', col = 'grey', lwd = 2, ylim = Ylim, main = MainLab, tck = 0.02)
  lines(t, Abstats[3,], col = "black", lwd = LWD)#median
  lines(t, Abstats[1,], col = "grey", lwd = LWD, lty = 2)
  lines(t, Abstats[5,], col = "grey", lwd = LWD, lty = 2)
  # axis labels
  if(missing(FmoTixs)) {axis(1, tck = 0.02)}
  else {
    if(FmoTixs == TRUE){
      labs <- c("1 Jan", "1 Feb", "1 Mar", " 1 Apr", "1 May", "1 Jun", "1 Jul")
      m <- as.Date(c('01/01/2013', '01/02/2013', "01/03/2013", "01/04/2013","01/05/2013",
                     "01/06/2013", "01/07/2013"), '%d/%m/%Y')
      axis(1, at = m, labels = labs, tck = 0.02)
    }
    if(FmoTixs == FALSE){axis(1, tck = 0.02)}
  }
  
  if(legendflag == TRUE) {
    legend("topright", c("mean", "median", "2.5%", "97.5%"), lty = c(1,1,2,2), 
           col = c("grey", "black", "grey", "grey"), bty = "n")
  }
}

# Plot the abortion density
plot_abort_ft <- function(model, MainLab, offset, LWD, Ylim, Xlim, legendflag, Ylab, FmoTixs){
  if(missing(MainLab)) {MainLab <- " "}
  if(missing(LWD)) {LWD <- 1}
  if(missing(Ylab)) {Ylab <- 'Abortion event density f(t)'}
  if(missing(Xlim)) {Xlim <- as.Date(c('2013/01/01','2013/07/01'))}
  
  if(missing(offset)) {
    t <- seq(1, dim(model$BUGSoutput$sims.list$Haz_b)[2], 1)
  }
  t <- as.Date('01/01/2013', '%d/%m/%Y') + seq(1,dim(model$BUGSoutput$sims.list$Haz_b)[2],1) + offset 
  
  HazAb.mat <- model$BUGSoutput$sims.list$Haz_a # abortion hazard
  HazB.mat <- model$BUGSoutput$sims.list$Haz_b # overall hazard
  ft.mat <- matrix(NA,  nrow = dim(HazAb.mat)[1],  ncol = dim(HazAb.mat)[2])
  ft.mat[,1]<- HazAb.mat[,1] # nobody has been removed on day 1
  
  D <- dim(HazAb.mat)[2]
  for(i in 2:D){ft.mat[,i] <- HazAb.mat[,i]*exp(-apply(HazB.mat[,1:i], 1, sum))}
  ftstats <- apply(ft.mat, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  ftmean <-  apply(ft.mat, 2, mean)
  
  if(missing(Ylim)) {Ylim <- c(0, max(ftstats[4,]))}
    
  plot(t, ftmean, xlab = '', xaxt = "n", ylab = Ylab, type = 'l', col = 'grey', 
       lwd = 2, ylim = Ylim, xlim = Xlim, main = MainLab, bty = "n", tck = 0.02)
  lines(t, ftstats[3,], col = "black", lwd = LWD)#median
  lines(t, ftstats[1,], col = "grey", lwd = LWD, lty = 2)
  lines(t, ftstats[5,], col = "grey", lwd = LWD, lty = 2)
  # axis labels
  if(missing(FmoTixs)) {axis(1, tck = 0.02)}
  else {
    if(FmoTixs == TRUE){
      labs <- c("1 Jan", "1 Feb", "1 Mar", "1 Apr", "1 May", "1 Jun", "1 Jul")
      m <- as.Date(c('01/01/2013', '01/02/2013', "01/03/2013", "01/04/2013","01/05/2013",
                     "01/06/2013", "01/07/2013"), '%d/%m/%Y')
      axis(1, at = m, labels = labs, tck = 0.02)
    }
    if(FmoTixs == FALSE){axis(1, tck = 0.02)}
  }
  
  if(legendflag == TRUE) {
    legend("topright", c("mean", "median", "2.5%", "97.5%"), lty = c(1,1,2,2), 
           col = c("grey", "black", "grey", "grey"), bty = "n")
  }
}

# Plot the birth density
plot_birth_ft <- function(model, MainLab, offset, LWD, Ylim, Xlim, legendflag, Ylab, FmoTixs){
  if(missing(MainLab)) {MainLab <- " "}
  if(missing(LWD)) {LWD <- 1}
  if(missing(Ylab)) {Ylab <- 'Birth event density f(t)'}
  if(missing(Xlim)) {Xlim <- as.Date(c('2013/01/01','2013/07/01'))}
  
  if(missing(offset)) {
    t <- seq(1, dim(model$BUGSoutput$sims.list$Haz_b)[2], 1)
  }
  t <- as.Date('01/01/2013', '%d/%m/%Y') + 
        seq(1,dim(model$BUGSoutput$sims.list$Haz_b)[2],1) + offset 
  
  HazLB.mat <- model$BUGSoutput$sims.list$Haz_l
  HazB.mat <- model$BUGSoutput$sims.list$Haz_b
  ft.mat <- matrix(NA,  nrow = dim(HazLB.mat)[1],  ncol = dim(HazLB.mat)[2])
  ft.mat[,1]<- HazLB.mat[,1]
  D <- dim(HazLB.mat)[2]
  for(i in 2:D){ft.mat[,i] <- HazLB.mat[,i]*exp(-apply(HazB.mat[,1:i], 1, sum))}
  ftstats <- apply(ft.mat, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  ftmean <-  apply(ft.mat, 2, mean)
  
  if(missing(Ylim)) {Ylim <- c(0, max(ftstats[4,]))}
    
  plot(t, ftmean, xlab = '', xaxt = 'n', ylab = Ylab, bty = "n", tck = 0.02, 
       type = 'l', col = 'grey', lwd = 2, ylim = Ylim, xlim = Xlim, main = MainLab)
  lines(t, ftstats[3,], col = "black", lwd = LWD)#median
  lines(t, ftstats[1,], col = "grey", lwd = LWD, lty = 2)
  lines(t, ftstats[5,], col = "grey", lwd = LWD, lty = 2)
  # axis labels
  if(missing(FmoTixs)) {axis(1, tck = 0.02)}
  else {
    if(FmoTixs == TRUE){
      labs <- c("1 Jan", "1 Feb", "1 Mar", "1 Apr", "1 May", "1 Jun", "1 Jul")
      m <- as.Date(c('01/01/2013', '01/02/2013', "01/03/2013", "01/04/2013","01/05/2013",
                     "01/06/2013", "01/07/2013"), '%d/%m/%Y')
      axis(1, at = m, labels = labs, tck = 0.02)
    }
    if(FmoTixs == FALSE){axis(1, tck = 0.02)}
  }
  
  if(legendflag == TRUE) {
    legend("topright", c("mean", "median", "2.5%", "97.5%"), lty = c(1,1,2,2), 
           col = c("grey", "black", "grey", "grey"), bty = "n")
  }
}

# Plot the total density
plot_total_ft <- function(model, MainLab, offset, LWD, Ylim, Xlim, legendflag, Ylab, FmoTixs){
  if(missing(MainLab)) {MainLab <- " "}
  if(missing(LWD)) {LWD <- 1}
  if(missing(Ylab)) {Ylab <- 'Event density f(t)'}
  if(missing(Xlim)) {Xlim <- as.Date(c('2013/01/01','2013/07/01'))}
  
  if(missing(offset)) {
    t <- seq(1, dim(model$BUGSoutput$sims.list$Haz_b)[2], 1)
  }
  t <- as.Date('01/01/2013', '%d/%m/%Y') + 
    seq(1,dim(model$BUGSoutput$sims.list$Haz_b)[2],1) + offset 
  
  HazB.mat <- model$BUGSoutput$sims.list$Haz_b
  ft.mat <- matrix(NA,  nrow = dim(HazB.mat)[1],  ncol = dim(HazB.mat)[2])
  ft.mat[,1]<- HazB.mat[,1]
  D <- dim(HazB.mat)[2]
  for(i in 2:D){ft.mat[,i] <- HazB.mat[,i]*exp(-apply(HazB.mat[,1:i], 1, sum))}
  ftstats <- apply(ft.mat, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  ftmean <-  apply(ft.mat, 2, mean)
  
  if(missing(Ylim)) {Ylim <- c(0, max(ftstats[4,]))}
  
  plot(t, ftmean, xlab = '', xaxt = 'n', ylab = Ylab, bty = "n", tck = 0.02, 
       type = 'l', col = 'grey', lwd = 2, ylim = Ylim, xlim = Xlim, main = MainLab)
  lines(t, ftstats[3,], col = "black", lwd = LWD)#median
  lines(t, ftstats[1,], col = "grey", lwd = LWD, lty = 2)
  lines(t, ftstats[5,], col = "grey", lwd = LWD, lty = 2)
  # axis labels
  if(missing(FmoTixs)) {axis(1, tck = 0.02)}
  else {
    if(FmoTixs == TRUE){
      labs <- c("1 Jan", "1 Feb", "1 Mar", "1 Apr", "1 May", "1 Jun", "1 Jul")
      m <- as.Date(c('01/01/2013', '01/02/2013', "01/03/2013", "01/04/2013","01/05/2013",
                     "01/06/2013", "01/07/2013"), '%d/%m/%Y')
      axis(1, at = m, labels = labs, tck = 0.02)
    }
    if(FmoTixs == FALSE){axis(1, tck = 0.02)}
  }
  
  if(legendflag == TRUE) {
    legend("topright", c("mean", "median", "2.5%", "97.5%"), lty = c(1,1,2,2), 
           col = c("grey", "black", "grey", "grey"), bty = "n")
  }
}

# all of them together as function
plot_VITmodel <- function(model, offset, LWD, legendflag){
  if(missing(LWD)) {LWD <- 1}
  
  if(missing(offset)) {
    t <- seq(1, dim(model$BUGSoutput$sims.list$Haz_b)[2], 1)
  }
  t <- as.Date('01/01/2013', '%d/%m/%Y') + seq(1,dim(model$BUGSoutput$sims.list$Haz_b)[2],1) + offset 
  
  prob.ab.mat <- model$BUGSoutput$sims.list$prob.ab
  pstats <- apply(prob.ab.mat, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  pmean <-  model$BUGSoutput$mean$prob.ab
  
  # axis labels
  m <- as.Date(c('01/01/2013', '01/02/2013', "01/03/2013", "01/04/2013","01/05/2013",
                 "01/06/2013", "01/07/2013"), '%d/%m/%Y')
  labs <- c("Jan 1", "Feb 1", "Mar 1", "Apr 1", "May 1", "Jun 1", "Jul 1")
  
  par(mfrow = c(1,3))
  plot(t, pmean, xlab = 'Day', ylab = 'P(abortion)', type = 'l', xaxt = "n", 
       lwd = 2, col ='grey', tck = 0.02)
  lines(t, pstats[3,], col = "black", lwd = LWD)#median
  lines(t, pstats[1,], col = "grey", lwd = LWD, lty = 2)
  lines(t, pstats[5,], col = "grey", lwd = LWD, lty = 2)
  axis(1, at = m, labels = labs, tck = 0.02)
    
  if(legendflag == TRUE) {
    legend("topright", c("mean", "median", "2.5%", "97.5%"), lty = c(1,1,2,2), 
           col = c("grey", "black", "grey", "grey"), bty = "n")
  }
  
  HazAb.mat <- model$BUGSoutput$sims.list$Haz_a
  Abstats <- apply(HazAb.mat, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  HazAbmean <-  model$BUGSoutput$mean$Haz_a
  plot(t, HazAbmean, xlab = 'Day', ylab = 'Abortion hazard', col ='grey', xaxt = "n",
       type = 'l', lwd = 2, ylim = c(0, max(Abstats), tck = 0.02))
  lines(t, Abstats[3,], col = "black", lwd = LWD)#median
  lines(t, Abstats[1,], col = "grey", lwd = LWD, lty = 2)
  lines(t, Abstats[5,], col = "grey", lwd = LWD, lty = 2)
  axis(1, at = m, labels = labs, tck = 0.02)
  
  HazLB.mat <- model$BUGSoutput$sims.list$Haz_l
  LBstats <- apply(HazLB.mat, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  HazLBmean <-  model$BUGSoutput$mean$Haz_l
  plot(t, HazLBmean, xlab = 'Day', ylab = 'Live Birth Rate', col ='grey', xaxt = "n",
       type = 'l', lwd = 2, ylim = c(0, max(LBstats)), tck = 0.02)
  lines(t, LBstats[3,], col = "black", lwd = LWD)#median
  lines(t, LBstats[1,], col = "grey", lwd = LWD, lty = 2)
  lines(t, LBstats[5,], col = "grey", lwd = LWD, lty = 2)
  axis(1, at = m, labels = labs, tck = 0.02)
  
}  
  
