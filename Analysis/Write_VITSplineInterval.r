# Write the bugs models for VIT hazard
library(R2WinBUGS)
setwd("~/BitBucket/VIT2014/Analysis/")

#*******************************************************************************
# Hazard Spline model with not partitioned by event type (10knots)
#*******************************************************************************
HazSplLog10k_ab <- function(){
  # Priors
  gamma0  ~ dnorm(0, 1.0E-6)
  sigmab ~ dunif(0,3)
  taub <- pow(sigmab, -2)
  
  for (l in 1:num.knots){
    b[l]  ~ dnorm(0,taub)
  }
  
  for (i in 1:D) {
    Haz_b[i] <- exp(gamma0 + mre110[i]) # baseline hazard
    
    mre110[i] <- b[1]*Z[i,1]+b[2]*Z[i,2]+b[3]*Z[i,3]+b[4]*Z[i,4]+b[5]*Z[i,5]+
      b[6]*Z[i,6]+b[7]*Z[i,7]+b[8]*Z[i,8]+b[9]*Z[i,9]+b[10]*Z[i,10]
    
  }               
  
  # Likelihood for the total hazard
  for (j in 1:records) {
    for (k in left[j]:(right[j]-1)) {
      UCH[j,k] <- exp(gamma0 + mre110[k])
    }   	
    
    SLR[j] <- exp(-sum(UCH[j,left[j]:(right[j]-1)])) # total prob of not having an event 
    noevent[j] ~ dbern(SLR[j])      
  }
}
write.model(HazSplLog10k_ab, 'HazSplLog10k_ab.txt')
#*******************************************************************************

#*******************************************************************************
# Hazard Spline model with not partitioned by event type (20knots)
 #*******************************************************************************
HazSplLog20k_ab <- function(){
  # Priors
  gamma0  ~ dnorm(0, 1.0E-6)
  sigmab ~ dunif(0,3)
  taub <- pow(sigmab, -2)
  
  for (l in 1:num.knots){
    b[l]  ~ dnorm(0,taub)
  }
  
  for (i in 1:D) {
    Haz_b[i] <- exp(gamma0 + mre110[i] + mre1120[i]) # baseline hazard
    
    mre110[i] <- b[1]*Z[i,1]+b[2]*Z[i,2]+b[3]*Z[i,3]+b[4]*Z[i,4]+b[5]*Z[i,5]+
      b[6]*Z[i,6]+b[7]*Z[i,7]+b[8]*Z[i,8]+b[9]*Z[i,9]+b[10]*Z[i,10]
    
    mre1120[i] <- b[11]*Z[i,11]+b[12]*Z[i,12]+b[13]*Z[i,13]+b[14]*Z[i,14]+b[15]*Z[i,15]+
      b[16]*Z[i,16]+b[17]*Z[i,17]+b[18]*Z[i,18]+b[19]*Z[i,19]+b[20]*Z[i,20]
 
  }               
  
  # Likelihood for the total hazard
  for (j in 1:records) {
    for (k in left[j]:(right[j]-1)) {
      UCH[j,k] <- exp(gamma0 + mre110[k]+ mre1120[k])
    }   	
    
    SLR[j] <- exp(-sum(UCH[j,left[j]:(right[j]-1)])) # total prob of not having an event 
    noevent[j] ~ dbern(SLR[j])      
  }
}
write.model(HazSplLog20k_ab, 'HazSplLog20k_ab.txt')
#*******************************************************************************


#*******************************************************************************
# Hazard Spline model with not partitioned by event type (40knots)
#*******************************************************************************
HazSplLog40k_ab <- function(){
  # Priors
  gamma0  ~ dnorm(0, 1.0E-6)
  sigmab ~ dunif(0,3)
  taub <- pow(sigmab, -2)
  
  for (l in 1:num.knots){
    b[l]  ~ dnorm(0,taub)
  }
  
  for (i in 1:D) {
    Haz_b[i] <- exp(gamma0 + mre110[i] + mre1120[i] + mre2130[i] + mre3140[i]) # baseline hazard
    
    mre110[i] <- b[1]*Z[i,1]+b[2]*Z[i,2]+b[3]*Z[i,3]+b[4]*Z[i,4]+b[5]*Z[i,5]+
      b[6]*Z[i,6]+b[7]*Z[i,7]+b[8]*Z[i,8]+b[9]*Z[i,9]+b[10]*Z[i,10]
    
    mre1120[i] <- b[11]*Z[i,11]+b[12]*Z[i,12]+b[13]*Z[i,13]+b[14]*Z[i,14]+b[15]*Z[i,15]+
      b[16]*Z[i,16]+b[17]*Z[i,17]+b[18]*Z[i,18]+b[19]*Z[i,19]+b[20]*Z[i,20]
    
    mre2130[i] <- b[21]*Z[i,21]+b[22]*Z[i,22]+b[23]*Z[i,23]+b[24]*Z[i,24]+b[25]*Z[i,25]+
      b[26]*Z[i,26]+b[27]*Z[i,27]+b[28]*Z[i,28]+b[29]*Z[i,29]+b[30]*Z[i,30]
    
    mre3140[i] <- b[31]*Z[i,31]+b[32]*Z[i,32]+b[33]*Z[i,33]+b[34]*Z[i,34]+b[35]*Z[i,35]+
      b[36]*Z[i,36]+b[37]*Z[i,37]+b[38]*Z[i,38]+b[39]*Z[i,39]+b[40]*Z[i,40]
    
  }               
  
  # Likelihood for the total hazard
  for (j in 1:records) {
    for (k in left[j]:(right[j]-1)) {
      UCH[j,k] <- exp(gamma0 + mre110[k] + mre1120[k] + mre2130[k] + mre3140[k])
    }     
    
    SLR[j] <- exp(-sum(UCH[j,left[j]:(right[j]-1)])) # total prob of not having an event 
    noevent[j] ~ dbern(SLR[j])      
  }
}
write.model(HazSplLog40k_ab, 'HazSplLog40k_ab.txt')
#*******************************************************************************

#*******************************************************************************
# Partitioned Hazard Spline model with logistic probability of abortion (10knots)
#*******************************************************************************
HazSplLog10k <- function(){
  # Priors
  sigmab ~ dunif(0,3)
  taub <- pow(sigmab, -2)
  gamma0  ~ dnorm(0, 1.0E-6)
  beta    ~ dunif(0,2) # slope for the probability of an abortion
  alpha   ~ dunif(50, 200) # offset for the transition from abortion to birth
  K ~ dunif(0,0.5) # lower asymptote for the prob. of an abortion
  for (l in 1:num.knots){b[l]  ~ dnorm(0,taub)}
  
  #Derived parameters
  for (i in 1:D) {
    Haz_b[i] <- exp(gamma0 + mre110[i]) # baseline hazard
    
    mre110[i] <- b[1]*Z[i,1]+b[2]*Z[i,2]+b[3]*Z[i,3]+b[4]*Z[i,4]+b[5]*Z[i,5]+
      b[6]*Z[i,6]+b[7]*Z[i,7]+b[8]*Z[i,8]+b[9]*Z[i,9]+b[10]*Z[i,10]

    prob.ab[i] <-  1 + (K - 1) / (1 + exp(-beta*(i - alpha))) # probability of abortion 
    Haz_a[i] <- prob.ab[i] * Haz_b[i]     # abortion hazard
    Haz_l[i] <- (1-prob.ab[i]) * Haz_b[i] # live birth hazard
  }             	
  
  # Likelihood for the total hazard
  for (j in 1:records) {
    for (k in left[j]:(right[j]-1)) {UCH[j,k] <- exp(gamma0 + mre110[k])}   	
    SLR[j] <- exp(-sum(UCH[j,left[j]:(right[j]-1)])) # total prob of not having an event 
    noevent[j] ~ dbern(SLR[j])      
  }
  
  # Likelihood on partitioning the event types
  for (m in 1:n.events) {
    b.type[m] ~ dbern(p.a[m])
    p.a[m] <- 1 + (K - 1) / (1 + exp(-beta*(event.day[m] - alpha)))
  }  
}
write.model(HazSplLog10k, 'HazSplLog10k.txt')
#*******************************************************************************



#*******************************************************************************
# Partitioned Hazard Spline model with logistic probability of abortion
#*******************************************************************************
HazSplLog20k <- function(){
  # Priors
  sigmab ~ dunif(0,3)
  taub <- pow(sigmab, -2)
  
  for (l in 1:num.knots){
    b[l]  ~ dnorm(0,taub)
  }
  
  gamma0  ~ dnorm(0, 1.0E-6)
  beta    ~ dunif(0,2) # slope for the probability of an abortion
  alpha   ~ dunif(50, 200) # offset for the transition from abortion to birth
  K ~ dunif(0,0.5) # lower asymptote for the prob. of an abortion
  
  for (i in 1:D) {
    Haz_b[i] <- exp(gamma0 + mre110[i] + mre1120[i]) # baseline hazard
    
    mre110[i] <- b[1]*Z[i,1]+b[2]*Z[i,2]+b[3]*Z[i,3]+b[4]*Z[i,4]+b[5]*Z[i,5]+
      b[6]*Z[i,6]+b[7]*Z[i,7]+b[8]*Z[i,8]+b[9]*Z[i,9]+b[10]*Z[i,10]
    
    mre1120[i] <- b[11]*Z[i,11]+b[12]*Z[i,12]+b[13]*Z[i,13]+b[14]*Z[i,14]+b[15]*Z[i,15]+
      b[16]*Z[i,16]+b[17]*Z[i,17]+b[18]*Z[i,18]+b[19]*Z[i,19]+b[20]*Z[i,20]
    
    prob.ab[i] <-  1 + (K - 1) / (1 + exp(-beta*(i - alpha))) # probability of abortion 
    Haz_a[i] <- prob.ab[i] * Haz_b[i]     # abortion hazard
    Haz_l[i] <- (1-prob.ab[i]) * Haz_b[i] # live birth hazard
  }         			
  
  # Likelihood for the total hazard
  for (j in 1:records) {
    for (k in left[j]:(right[j]-1)) {
      UCH[j,k] <- exp(gamma0 + mre110[k] + mre1120[k])
    }   	
    
    SLR[j] <- exp(-sum(UCH[j,left[j]:(right[j]-1)])) # total prob of not having an event 
    noevent[j] ~ dbern(SLR[j])      
  }
  
  # Likelihood on partitioning the event types
  for (m in 1:n.events) {
    b.type[m] ~ dbern(p.a[m])
    p.a[m] <- 1 + (K - 1) / (1 + exp(-beta*(event.day[m] - alpha)))
  }  
}
write.model(HazSplLog20k, 'HazSplLog20k.txt')
#*******************************************************************************

#*******************************************************************************
# Partitioned Hazard Spline model with logistic probability of abortion (40knots)
#*******************************************************************************
HazSplLog40k <- function(){
  # Priors
  sigmab ~ dunif(0,3)
  taub <- pow(sigmab, -2)
  
  for (l in 1:num.knots){
    b[l]  ~ dnorm(0,taub)
  }
  
  gamma0  ~ dnorm(0, 1.0E-6)
  beta    ~ dunif(0,2) # slope for the probability of an abortion
  alpha   ~ dunif(50, 200) # offset for the transition from abortion to birth
  K ~ dunif(0,0.5) # lower asymptote for the prob. of an abortion
  
  for (i in 1:D) {
    Haz_b[i] <- exp(gamma0 + mre110[i] + mre1120[i] + mre2130[i] + mre3140[i]) # baseline hazard
    
    mre110[i] <- b[1]*Z[i,1]+b[2]*Z[i,2]+b[3]*Z[i,3]+b[4]*Z[i,4]+b[5]*Z[i,5]+
      b[6]*Z[i,6]+b[7]*Z[i,7]+b[8]*Z[i,8]+b[9]*Z[i,9]+b[10]*Z[i,10]
    
    mre1120[i] <- b[11]*Z[i,11]+b[12]*Z[i,12]+b[13]*Z[i,13]+b[14]*Z[i,14]+b[15]*Z[i,15]+
      b[16]*Z[i,16]+b[17]*Z[i,17]+b[18]*Z[i,18]+b[19]*Z[i,19]+b[20]*Z[i,20]
    
    mre2130[i] <- b[21]*Z[i,21]+b[22]*Z[i,22]+b[23]*Z[i,23]+b[24]*Z[i,24]+b[25]*Z[i,25]+
      b[26]*Z[i,26]+b[27]*Z[i,27]+b[28]*Z[i,28]+b[29]*Z[i,29]+b[30]*Z[i,30]
    
    mre3140[i] <- b[31]*Z[i,31]+b[32]*Z[i,32]+b[33]*Z[i,33]+b[34]*Z[i,34]+b[35]*Z[i,35]+
      b[36]*Z[i,36]+b[37]*Z[i,37]+b[38]*Z[i,38]+b[39]*Z[i,39]+b[40]*Z[i,40]
    
    prob.ab[i] <-  1 + (K - 1) / (1 + exp(-beta*(i - alpha))) # probability of abortion 
    Haz_a[i] <- prob.ab[i] * Haz_b[i]     # abortion hazard
    Haz_l[i] <- (1-prob.ab[i]) * Haz_b[i] # live birth hazard
  }           		
  
  # Likelihood for the total hazard
  for (j in 1:records) {
    for (k in left[j]:(right[j]-1)) {
      UCH[j,k] <- exp(gamma0 + mre110[k] + mre1120[k] + mre2130[k] + mre3140[k])
    }   	
    
    SLR[j] <- exp(-sum(UCH[j,left[j]:(right[j]-1)])) # total prob of not having an event 
    noevent[j] ~ dbern(SLR[j])      
  }
  
  # Likelihood on partitioning the event types
  for (m in 1:n.events) {
    b.type[m] ~ dbern(p.a[m])
    p.a[m] <- 1 + (K - 1) / (1 + exp(-beta*(event.day[m] - alpha)))
  }  
}
write.model(HazSplLog40k, 'HazSplLog40k.txt')
#*******************************************************************************

#*******************************************************************************
# Daily Icar time model Partitioning Event Type alt formulation of CAR
#*******************************************************************************
icar_partjag <- function(){
  # Priors
  gamma0  ~ dnorm(0, 1.0E-6)
  beta    ~ dunif(0,2) # slope for the probability of an abortion
  alpha   ~ dunif(50, 200) # offset for the transition from abortion to birth
  K ~ dunif(0,0.5) # lower asymptote for the prob. of an abortion
  sd.rho ~ dunif(0,10)  # hyperprior
  tau.rho <- pow(sd.rho, -2)  
  rho[1] ~ dnorm(0, 1.0E-6)
  for (i in 2:D) {rho[i]~dnorm(rho[i-1],tau.rho)}
    
  # Likelihood
  for (j in 1:records) {
    for (k in left[j]:(right[j]-1)) {
      UCH[j,k] <- exp(gamma0 + rho[k]) # unit hazard over the interval
    }     
    SLR[j] <- exp(-sum(UCH[j,left[j]:(right[j]-1)]))# total prob of not having an event 
    noevent[j] ~ dbern(SLR[j])      
  }
  
  # Likelihood on partitioning the event types
  for (m in 1:n.events) {
    b.type[m] ~ dbern(p.a[m])
    p.a[m] <- 1 + (K - 1) / (1 + exp(-beta*(event.day[m] - alpha)))
  }  
  
  #Derived quantities
  for (i in 1:D) {
    Haz_b[i] <- exp(gamma0 + rho[i]) # overall baseline
    prob.ab[i] <-  1 + (K - 1) / (1 + exp(-beta*(i - alpha))) # probability of abortion 
    Haz_a[i] <- prob.ab[i] * Haz_b[i]     # abortion hazard
    Haz_l[i] <- (1-prob.ab[i]) * Haz_b[i] # live birth hazard
  }   	
  CumHaz_b <- sum(Haz_b[1:D])
  CumHaz_a <- sum(Haz_a[1:D])
  CumHaz_l <- sum(Haz_l[1:D])
}
write.model(icar_partjag,'icar_partjag.txt')
#*******************************************************************************


#*******************************************************************************
# Daily Icar time model alt formulation of CAR
#*******************************************************************************
icar_jag <- function(){
  # Priors
  gamma0  ~ dnorm(0, 1.0E-6)
  sd.rho ~ dunif(0,10)  # hyperprior
  tau.rho <- pow(sd.rho, -2)  
  rho[1] ~ dnorm(0, 1.0E-6)
  for (i in 2:D) {rho[i] ~ dnorm(rho[i-1], tau.rho)}
  
  # Likelihood
  for (j in 1:records) {
    for (k in left[j]:(right[j]-1)) {
      UCH[j,k] <- exp(gamma0 + rho[k]) # unit hazard over the interval
    }     
    SLR[j] <- exp(-sum(UCH[j,left[j]:(right[j]-1)]))# total prob of not having an event 
    noevent[j] ~ dbern(SLR[j])      
  }
  
  #Derived quantities
  for (i in 1:D) {Haz_b[i] <- exp(gamma0 + rho[i])} # overall baseline
}
write.model(icar_jag,'icar_jag.txt')
#*******************************************************************************




#*******************************************************************************
# Daily Icar time model Partitioning Event Type Car.Normal Fxn
#*******************************************************************************
icar_part <- function(){
  # Priors
  gamma0  ~ dnorm(0, 1.0E-6)
  beta    ~ dunif(0,2) # slope for the probability of an abortion
  alpha   ~ dunif(50, 200) # offset for the transition from abortion to birth
  K ~ dunif(0,0.5) # lower asymptote for the prob. of an abortion
  sd.rho ~ dunif(0,10)  # hyperprior
  tau.rho <- pow(sd.rho, -2)  
  rho[1:D] ~ car.normal(adj[], wts[], num[], tau.rho) # for the baseline hazard
  
  # Likelihood
  for (j in 1:records) {
    for (k in left[j]:(right[j]-1)) {
      UCH[j,k] <- exp(gamma0 + rho[k]) # unit hazard over the interval
    }   	
    SLR[j] <- exp(-sum(UCH[j,left[j]:(right[j]-1)]))# total prob of not having an event 
    noevent[j] ~ dbern(SLR[j])      
  }
  
    # Likelihood on partitioning the event types
  for (m in 1:n.events) {
    b.type[m] ~ dbern(p.a[m])
    p.a[m] <- 1 + (K - 1) / (1 + exp(-beta*(event.day[m] - alpha)))
  }  
    
  #Derived quantities
  for (i in 1:D) {
    Haz_b[i] <- exp(gamma0 + rho[i]) # overall baseline
    prob.ab[i] <-  1 + (K - 1) / (1 + exp(-beta*(i - alpha))) # probability of abortion 
    Haz_a[i] <- prob.ab[i] * Haz_b[i]     # abortion hazard
    Haz_l[i] <- (1-prob.ab[i]) * Haz_b[i] # live birth hazard
  }   	
  CumHaz_b <- sum(Haz_b[1:D])
  CumHaz_a <- sum(Haz_a[1:D])
  CumHaz_l <- sum(Haz_l[1:D])
  
  # for the ICAR portions 
  wts[1] <- 1
  for(t in 2:(D-1)) {wts[2+(t-2)*2] <- 1; wts[3+(t-2)*2] <- 1}  
  wts[(D-2)*2 + 2] <- 1
  adj[1] <- 2
  for(t in 2:(D-1)) {adj[2+(t-2)*2] <- t-1; adj[3+(t-2)*2] <- t+1}  
  adj[(D-2)*2 + 2] <- D-1	
  num[1] <- 1
  for(t in 2:(D-1)) {num[t] <- 2}	
  num[D] <- 1
}
write.model(icar_part,'icar_part.bug')
#*******************************************************************************
