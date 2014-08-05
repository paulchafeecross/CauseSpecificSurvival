# Code to run the vit hazard spline models
setwd("~/BitBucket/VIT2014/Analysis")
rm(list = ls())
library(R2WinBUGS)
library(R2jags)
source("Z_foo.r") # fxn for calculating Z on the splines
source("plot_VITresults_fxns.r") # fxns for plotting model results
source("Write_VITSplineInterval.r") # code for the BUGS models

seroyes <- 0
if(seroyes == 1){load('VIT_Data2014_Full_POS.RData')}
if(seroyes != 1){load('VIT_Data2014_Full.RData')}

#*******************************************************************************
# RUN STATISTICAL MODELS
#*******************************************************************************
#*******************************************************************************
# Partitioned HAZARD MODEL w/ logistic
#*******************************************************************************
niter <- 500000
num.knots <- 10
Z <- Z_foo(num.knots, min(left), max(right))
data <- list('left', 'right', 'noevent', 'D', 'Z', 'num.knots', 'records', 'b.type', 
             'event.day', 'n.events')
params <- c('Haz_b', 'Haz_a', 'Haz_l', 'prob.ab', 'gamma0','beta', 'sigmab', 'b', 'alpha', "K")
mod10k <- jags(data, inits=NULL , params, HazSplLog10k, n.chains=3, n.iter=niter)
save(mod10k, file = paste('Result_mod10k', seroyes, '.RData', sep = ''))
model <- mod10k 
plot(model)
print(model, digits = 3)
#*******************************************************************************

#*******************************************************************************
#40 KNOTS
#*******************************************************************************
num.knots <- 40
Z <- Z_foo(num.knots, min(left), max(right))
data <- list('left', 'right', 'noevent', 'D', 'Z', 'num.knots', 'records', 'b.type', 
             'event.day', 'n.events')
mod40k <- jags(data, inits = NULL, params, HazSplLog40k, n.chains=3, n.iter=niter)
save(mod40k, file = paste('Result_mod40k', seroyes, '.RData', sep = ''))
model <- mod40k 
plot(model)
plot_VITmodel(model)
print(model, digits = 3)
#*******************************************************************************

#*******************************************************************************
# Daily Icar time model Partitioning Event Type
#*******************************************************************************
data <- list('left', 'right', 'noevent', 'D', 'records', 'b.type', 
             'event.day', 'n.events')
params <- c('Haz_b', 'Haz_a', 'Haz_l', 'prob.ab', 'gamma0','beta', 'sd.rho', 'rho', 'alpha', "K")
icarmod <- jags(data, inits = NULL, params, icar_partjag, n.chains=3, n.iter=niter)
save(icarmod, file = paste('Result_icar', seroyes, '.RData', sep = ''))
model <- icarmod 
plot(model)
plot_VITmodel(model)
#*******************************************************************************

#*******************************************************************************
#20 KNOTS
#*******************************************************************************
num.knots <- 20
Z <- Z_foo(num.knots, min(left), max(right))
data <- list('left', 'right', 'noevent', 'D', 'Z', 'num.knots', 'records', 'b.type', 
             'event.day', 'n.events')

mod20k <- jags(data, inits = jags.inits, params, HazSplLog20k, n.chains=3, n.iter=niter)
save(mod20k, file = paste('Result_mod20k', seroyes, '.RData', sep = ''))
model <- mod20k 
plot(model)
plot_VITmodel(model)
print(model, digits = 3)
#*******************************************************************************

#*******************************************************************************
# LOAD past results and take a look
#*******************************************************************************
rm(list = ls())
seroyes <- 1
if(seroyes == 1){
  load('VIT_Data2013_Full_POS.RData')
  load("Result_mod10k1.RData")
  load("Result_mod20k1.RData")
  load("Result_mod40k1.RData")
  load("Result_icar1.RData")
  
}
if(seroyes != 1){
  load('VIT_Data2013_Full.RData')
  load("Result_mod10k0.RData")
  load("Result_mod20k0.RData")
  load("Result_mod40k0.RData")
  load("Result_icar0.RData")
}
source("plot_VITresults_fxns.r") # fxns for plotting model results
start.offset <- min(Data$CapDOY)

#10knots
plot(mod10k)
print(mod10k, digits = 3)
sort(mod10k$BUGSoutput$summary[,8])

#seems like this has trouble converging for prob.ab in March-April.  Let's look at the different MCMC chains
chain1 <- apply(mod10k$BUGSoutput$sims.array[,1,646:(646+D-1)], 2, stats::quantile, c(0.025,.5, 0.0975))
chain2 <- apply(mod10k$BUGSoutput$sims.array[,2,646:(646+D-1)], 2, stats::quantile, c(0.025,.5, 0.0975))
chain3 <- apply(mod10k$BUGSoutput$sims.array[,3,646:(646+D-1)], 2, stats::quantile, c(0.025,.5, 0.0975))
plot(chain1[2,], col = "blue", type = "l")
lines(chain1[1,], col = "blue", lty = 2)
lines(chain1[3,], col = "blue", lty = 2)
lines(chain2[2,], col = "red")
lines(chain2[1,], col = "red", lty = 2)
lines(chain2[3,], col = "red", lty = 2)
lines(chain3[2,], col = "green")
lines(chain3[1,], col = "green", lty = 2)
lines(chain3[3,], col = "green", lty = 2)
# so although the R-metric indicates a lack of convergence I think the inference is fine


#20knots
plot(mod20k)
print(mod20k, digits = 3)
sort(mod20k$BUGSoutput$summary[,8])
#40knots
plot(mod40k)
print(mod40k, digits = 3)
sort(mod40k$BUGSoutput$summary[,8])

#icar
plot(icarmod)
print(icarmod)
sort(icarmod$BUGSoutput$summary[1:(D*3),8])

# comparison of abortion hazards
par(mfrow = c(2,2), mar = c(2,3,1,1), cex = 1, mgp = c(1.5,.25,0))
plot_hazAb(mod10k, "10knot", start.offset, 2, Ylim = c(0, 0.008), 
           Ylab = "Cumulative daily abortion hazard",
           Xlim = as.Date(c('2013/01/15', '2013/07/01')), 
           legendflag=FALSE, FmoTixs = T)
plot_hazAb(mod20k, "20knot", start.offset, 2, Ylim = c(0, 0.008), 
           Ylab = "Cumulative daily abortion hazard",
           Xlim = as.Date(c('2013/01/15', '2013/07/01')), 
           legendflag=FALSE, FmoTixs = T)
plot_hazAb(mod40k, "40knot", start.offset, 2, Ylim = c(0, 0.008), 
           Ylab = "Cumulative daily abortion hazard",
           Xlim = as.Date(c('2013/01/15', '2013/07/01')), 
           legendflag=FALSE, FmoTixs = T)
plot_hazAb(icarmod, "ICAR", start.offset, 2, Ylim = c(0, 0.008), 
           Ylab = "Cumulative daily abortion hazard",
           Xlim = as.Date(c('2013/01/15', '2013/07/01')), 
           legendflag=FALSE, FmoTixs = T)

# PLOT birth and abortion hazards and event densities
model <- mod40k
par(mfrow = c(2,2), mar = c(2,3.5,1,1), cex = 1, mgp = c(2,.5,0))
#Top row
plot_hazAb(model, "", start.offset, 2, Ylim = c(0, 0.008), 
           Ylab = "Abortion hazard (events per elk / day)",
           Xlim = as.Date(c('2013/02/01', '2013/07/01')), 
           legendflag=FALSE, FmoTixs = T)
text(as.Date("2013/01/26"), 0.008, "A)", pos = 4, cex = 1)

plot_abort_ft(model, "", start.offset, 2, Ylim = c(0, 0.008), 
              Ylab = "Abortion density (probability / day)",
              Xlim = as.Date(c('2013/02/01', '2013/07/01')), 
              legendflag=FALSE, FmoTixs = T)
text(as.Date("2013/01/26"), 0.008, "B)", pos = 4, cex = 1)

#Bottom row
plot_hazLB(model, "", start.offset, 2, Ylim = c(0, 0.2),   
           Xlim = as.Date(c('2013/02/01', '2013/07/01')), 
           Ylab = "Birth hazard (events per elk / day)",
           legendflag=FALSE, FmoTixs = T)
text(as.Date("2013/01/26"), 0.15, "C)", pos = 4, cex = 1)
plot_birth_ft(model, "", start.offset, 2, Ylim = c(0, 0.06),  
              Xlim = as.Date(c('2013/02/01', '2013/07/01')),
              Ylab = "Birth density (probability / day)",
              legendflag=FALSE, FmoTixs = T)
text(as.Date("2013/01/26"), 0.04, "D)", pos = 4, cex = 1)

# Plot baselines
par(mfrow = c(2,2), mar = c(2,3.5,1,1), cex = 1, mgp = c(2,.5,0))
plot_baseline(mod10k, "10 knots", start.offset, 2, Ylim = c(0, 0.02),   
              Xlim = as.Date(c('2013/02/01', '2013/07/01')), 
              legendflag==F, Ylab = "total hazard")
plot_baseline(mod40k, "40 knots", start.offset, 2, Ylim = c(0, 0.02),   
              Xlim = as.Date(c('2013/02/01', '2013/07/01')), 
              legendflag==F, Ylab = "total hazard")
plot_baseline(icarmod, "ICAR", start.offset, 2, Ylim = c(0, 0.02),   
              Xlim = as.Date(c('2013/02/01', '2013/07/01')), 
              legendflag==F, Ylab = "total hazard")
plot_Pabort(icarmod, "", start.offset, 2,  Xlim = as.Date(c('2013/02/01', '2013/07/01')),
            legendflag=FALSE)



# PLOT prob of abortion
par(mfrow = c(1,1), mar = c(2,3,1,1), cex = 1, mgp = c(1.5,.25,0))
plot_Pabort(model, "", start.offset, 2,  Xlim = as.Date(c('2013/02/01', '2013/07/01')),
            legendflag=FALSE)

# Plot the data
source('VITplot_interval2.R')
par(mfrow = c(1,1), mar = c(2,3,1,1), cex = 1, mgp = c(1.5,.25,0))
VITplot_interval2(DataPOS, 2) 
text(as.Date("2013/01/01"), 137, "A)", pos = 4, cex = 1)

