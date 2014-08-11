# script to plot some of the VIT statistical results
rm(list = ls())
library(R2jags)
setwd("~/BitBucket/VIT2014/Analysis")
seroYes <- 1 # flag for the use of seropos only

if (seroYes == 1){
  load('Result_mod10k1.RData')
  #load('Result_mod20k1.RData')
  load('Result_mod40k1.RData')
  load('VIT_Data2014_Full_POS.RData')
}

if(seroYes !=1){
  load('Result_mod10k0.RData')
  load('Result_mod20k0.RData')
  load('Result_mod40k0.RData')
  load('Result_icar0.RData')
  load('VIT_Data2014_Full.RData')
}

#Compare the DIC and pDs
mod10k$BUGSoutput$DIC
mod20k$BUGSoutput$DIC
mod40k$BUGSoutput$DIC
icarmod$BUGSoutput$DIC

mod10k$BUGSoutput$pD
mod20k$BUGSoutput$pD
mod40k$BUGSoutput$pD
icarmod$BUGSoutput$pD

start.offset <- min(Data$CapDOY)

source("plot_VITresults_fxns.R")
plot_baseline(mod10k, start.offset)
plot_VITmodel(mod10k, start.offset)
par(mfrow = c(1,1))
plot_abort_ft(mod10k, "Event density", start.offset)

# check the worst convergence
which.max(mod10k$BUGSoutput$summary[,8])
mod10k$BUGSoutput$summary[which.max(mod10k$BUGSoutput$summary[,8]),]
which.max(mod20k$BUGSoutput$summary[,8])
mod20k$BUGSoutput$summary[which.max(mod20k$BUGSoutput$summary[,8]),]

plot(mod10k)
plot(mod20k)

model <- mod10k

plot(model)

#*******************************************************************************
# PLOT OF Daily HAZARDS Total Model
#*******************************************************************************
start.offset <- min(Data$CapDOY)
xax <- as.Date('01/01/2013', '%d/%m/%Y') + seq(1,D,1) + start.offset - 1

CI.b <- matrix(NA, nrow = D, ncol = 4)
par(mfrow = c(1,1))
for(i in 1:D){CI.b[i,] <- quantile(model$BUGSoutput$sims.list$Haz_b[,i], prob = c(0.025, 0.975, 0.25, 0.75))}
plot(xax, model$BUGSoutput$mean$Haz_b, pch = 19, ylab = 'Daily hazard', xlab = '', ylim = c(-0.01,.3), lwd = 3, col = 'black')
for(i in 1:D){lines(c(xax[i],xax[i]), CI.b[i, 1:2], col = 'red')} 

Abort <- subset(Type, Type$b.type == 1)
Live <- subset(Type, Type$b.type == 0)
points(as.Date(Abort$right-start.offset, origin = "2013-01-01"), rep(-0.01, dim(Abort)[1]), pch = 'o',  col = 'red')
points(as.Date(jitter(Live$right)-start.offset, origin = "2013-01-01"), rep(0, dim(Live)[1]), pch = 'o',  col = 'blue')
#*******************************************************************************

#*******************************************************************************
# plot the hazards by chain
#*******************************************************************************
model <- icarmod
Haz_b <- model$BUGSoutput$sims.array[,,1:D]
Haz_b.avg <- matrix(data = NA, nrow = D, ncol = 3)
Haz_b.lo <- matrix(data = NA, nrow = D, ncol = 3)
Haz_b.hi <- matrix(data = NA, nrow = D, ncol = 3)
for(i in 1:D){
  Haz_b.avg[i,] <- apply(Haz_b[,,i], 2, mean)
  Haz_b.lo[i,] <- apply(Haz_b[,,i], 2, quantile, probs = 0.10)
  Haz_b.hi[i,] <- apply(Haz_b[,,i], 2, quantile, probs = 0.90)
}
par(mfrow = c(1,1))
plot(seq(1:D), Haz_b.avg[,1], pch = 20, col = 'red', ylim = c(0,.01), ylab = 'Hazard', xlab = 'timestep')
points(seq(1:D), Haz_b.avg[,2], pch = 20, col = 'blue')
points(seq(1:D), Haz_b.avg[,3], pch = 20, col = 'green')
points(seq(1:D), Haz_b.lo[,1], col = 'red')
points(seq(1:D), Haz_b.lo[,2], col = 'blue')
points(seq(1:D), Haz_b.lo[,3], col = 'green')
points(seq(1:D), Haz_b.hi[,1], col = 'red')
points(seq(1:D), Haz_b.hi[,2], col = 'blue')
points(seq(1:D), Haz_b.hi[,3], col = 'green')
#*******************************************************************************

#*******************************************************************************
# PLOT OF Daily HAZARDS Partitioned model
#*******************************************************************************
CI.b <- matrix(NA, nrow = D, ncol = 4)
CI.a <- matrix(NA, nrow = D, ncol = 4)
CI.l <- matrix(NA, nrow = D, ncol = 4)
CI.p <- matrix(NA, nrow = D, ncol = 4)

for(i in 1:D){
  CI.b[i,] <- quantile(model$BUGSoutput$sims.list$Haz_b[,i], prob = c(0.025, 0.975, 0.25, 0.75))
  CI.a[i,] <- quantile(model$BUGSoutput$sims.list$Haz_a[,i], prob = c(0.025, 0.975, 0.25, 0.75))
  CI.l[i,] <- quantile(model$BUGSoutput$sims.list$Haz_l[,i], prob = c(0.025, 0.975, 0.25, 0.75))
  CI.p[i,] <- quantile(model$BUGSoutput$sims.list$prob.ab[,i], prob = c(0.025, 0.975, 0.25, 0.75))
}

plot(xax, model$BUGSoutput$mean$Haz_b, type = 'n', ylab = 'Daily hazard', 
     ylim = c(-.0003,.013), lwd = 3, col = 'black', xlab = '',
     xlim = as.Date(c('01/02/13', '1/07/13'), '%d/%m/%y'))
lines(xax, model$BUGSoutput$mean$Haz_l, col = 'blue', lwd = 2)
lines(xax, model$BUGSoutput$mean$Haz_a, col = 'red', lwd = 2)
lines(xax, CI.a[, 1], lty = 2, col = 'red')
lines(xax, CI.a[, 2], lty = 2, col = 'red')
lines(xax, CI.l[, 1], lty = 2, col = 'blue')
lines(xax, CI.l[, 2], lty = 2, col = 'blue')

Abort <- subset(Type, Type$b.type == 1)
Live <- subset(Type, Type$b.type == 0)
points(as.Date(Abort$right-start.offset, origin = "2013-01-01"), rep(-0.0002, dim(Abort)[1]), pch = 'o',  col = 'red')
points(as.Date(jitter(Live$right)-start.offset, origin = "2013-01-01"), rep(-0.0003, dim(Live)[1]), pch = 'o',  col = 'blue')
#*******************************************************************************

#*******************************************************************************
# Plot probability of abortion
#*******************************************************************************
plot(xax, model$BUGSoutput$median$prob.ab, 
     ylab = 'Probability the event is an Abortion', xlab = '', type = 'l', lwd = 2)
#points(xax, CI.p[, 3], col = 'blue')  #change this
#points(xax, CI.p[, 4], col = 'blue')


#*******************************************************************************
# Plot Cumulative hazard
#*******************************************************************************
CumProb.a <- rep(NA, D) # cumulative probability of having an abortion
CI.CumProb.a <- matrix(NA, nrow = D, ncol = 4) 

CumProb.a[1] <- model$BUGSoutput$mean$Haz_a[1]
CI.CumProb.a[1,] <- CI.a[1,]

for(i in 2:D){
  CumProb.a[i] <- 1 - exp(-sum(model$BUGSoutput$mean$Haz_a[1:i])) #aka lifetime distribution function (complement of survival)
  CI.CumProb.a[i,] <- 1 - exp(-apply(CI.a[1:i,],2,sum))
}

plot(xax, CumProb.a, type = 'l', ylab = 'Cumulative Prob. of Abortion', 
     ylim = c(-.0003,.5), lwd = 3, col = 'red', xlab = '',
     xlim = as.Date(c('01/02/13', '1/07/13'), '%d/%m/%y'))
for(i in 1:D){lines(c(xax[i],xax[i]), CI.CumProb.a[i, 1:2], col = 'red')} 
#*******************************************************************************

#*******************************************************************************
# Plot the event density
#*******************************************************************************
ft <- CumProb.a[2:D] - CumProb.a[1:D-1]
plot(ft)
lines(model$BUGSoutput$mean$Haz_a[1:D-1])
#*******************************************************************************



