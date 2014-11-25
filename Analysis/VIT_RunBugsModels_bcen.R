# Code to run the vit hazard spline models
setwd("~/BitBucket/VIT2014/Analysis")
#setwd("~/Dropbox/Brucella/Analysis/VIT")
rm(list = ls())
library(R2WinBUGS)
library(R2jags)
source("Z_foo.r") # fxn for calculating Z on the splines
source("plot_VITresults_fxns.r") # fxns for plotting model results
source("Write_VITSplineInterval.r") # code for the BUGS models

seroyes <- 1
load('VIT_Data2014_BirthsCensored.RData')
niter <- 300000

#*******************************************************************************
# Run the hazard spline 10knots model for the abortions only with the births censored
#*******************************************************************************
# Code to run the vit hazard spline models
num.knots <- 10
Z <- Z_foo(num.knots, min(left), max(right))
data <- list('left', 'right', 'noevent', 'D', 'Z', 'num.knots', 'records')
params <- c('Haz_b', 'gamma0', 'b', 'sigmab')
mod10k_bcen <- jags(data, inits = NULL , params, HazSplLog10k_ab, n.chains=3, n.iter=niter)
save(mod10k_bcen, file = paste('Result_mod10k_bcen', seroyes, '.RData', sep = ''))
model <- mod10k_bcen
plot(model)

# 40 knot:  goes to INF values and blows up (probably because one part of the spline is all 1s or 0s (or nothing)
num.knots <- 40
Z <- Z_foo(num.knots, min(left), max(right))
data <- list('left', 'right', 'noevent', 'D', 'Z', 'num.knots', 'records')
params <- c('Haz_b', 'gamma0', 'b', 'sigmab')
mod40k_bcen <- jags(data, inits = NULL , params, HazSplLog40k_ab, n.chains=3, n.iter=niter)
save(mod40k_bcen, file = paste('Result_mod40k_bcen', seroyes, '.RData', sep = ''))
model <- mod40k_bcen
plot(model)

#Run a CAR model too
data <- list('left', 'right', 'noevent', 'D', 'records')
params <- c('Haz_b', 'gamma0','sd.rho', 'rho')
icarmod_bcen <- jags(data, inits = NULL , params, icar_jag, n.chains=3, n.iter=niter)
save(icarmod_bcen, file = paste('Result_icar_bcen', seroyes, '.RData', sep = ''))
model <- icarmod_bcen
plot(model)
#*******************************************************************************



# 20 knot
num.knots <- 20
Z <- Z_foo(num.knots, min(left), max(right))
data <- list('left', 'right', 'noevent', 'D', 'Z', 'num.knots', 'records')
params <- c('Haz_b', 'gamma0', 'b', 'sigmab')
mod20k_bcen <- jags(data, inits = NULL , params, HazSplLog20k_ab, n.chains=3, n.iter=niter)
save(mod20k_bcen, file = paste('Result_mod20k_bcen', seroyes, '.RData', sep = ''))
model <- mod20k_bcen
plot(model)


#*******************************************************************************
# LOAD past results and take a look
#*******************************************************************************
load("Result_mod10k_bcen1.RData")
load("Result_mod20k_bcen1.RData")
load("Result_mod40k_bcen1.RData")
load("Result_icar_bcen1.RData")
start.offset <- min(Data$CapDOY)

#10knots
plot(mod10k_bcen)
print(mod10k_bcen)
max(mod10k_bcen$BUGSoutput$summary[,8])

#20knots
plot(mod20k_bcen)
print(mod20k_bcen)
max(mod20k_bcen$BUGSoutput$summary[,8])

#40knots
plot(mod40k_bcen)
print(mod40k_bcen)
max(mod40k_bcen$BUGSoutput$summary[,8])

#icar
plot(icarmod_bcen)
print(icarmod_bcen)
max(icarmod_bcen$BUGSoutput$summary[1:201,8])

# comparison
par(mfrow = c(2,2), mar = c(2,3,1,1), cex = 1, mgp = c(1.5,.25,0))
plot_baseline(mod10k_bcen, MainLab = "10knots", start.offset, 2, Ylim = c(0,0.008), 
              Xlim = as.Date(c('2013/01/01', '2013/07/01')), 
              legendflag=FALSE, Ylab = "Abortion hazard (prob. of event per day)")
plot_baseline(icarmod_bcen, MainLab = "ICAR", start.offset, 2, Ylim = c(0,0.008), 
              Xlim = as.Date(c('2013/01/01', '2013/07/01')), 
              legendflag=FALSE, Ylab = "")
plot_baseline(mod20k_bcen, MainLab = "20knots", start.offset, 2, Ylim = c(0,0.008), 
              Xlim = as.Date(c('2013/01/01', '2013/07/01')), 
              legendflag=FALSE, Ylab = "Abortion hazard (prob. of event per day)")
plot_baseline(mod40k_bcen, MainLab = "40knots ", start.offset, 2, Ylim = c(0,0.008), 
              Xlim = as.Date(c('2013/01/01', '2013/07/01')), 
              legendflag=FALSE, Ylab = "")

