# functions to plot the logistic function of abortion

# works on raw parameters
plot_logit <- function(a, b, t){
  prob <- exp(a + b*t) / (1+ exp(a + b*t))
  plot(t, prob, xlab = 'time', ylab = 'probability', type = 'l')
}

# uses input CIs (doesn't account for the correlation between alpha and beta, so not good to use)
plot_logit_CI <- function(alo, amid, ahi, blo, bmid, bhi, t){
  #INPUTS:
  #alo: lower CI for alpha
  #amid: median or mean of alpha
  #ahi: higher CI for alpha
  #blo: lower CI for beta
  #bmid: median or mean of beta
  #bhi: higher CI of beta
  # t  = sequence of numbers over which to compute the probability (single covariate)
  
  prob <- 1 / (1+ exp(-(amid + bmid*t)))
  problo <- 1 / (1+ exp(-(alo + blo*t)))
  probhi <- 1 / (1+ exp(-(ahi + bhi*t)))
  plot(t, prob, xlab = 'time', ylab = 'probability', type = 'l', lwd = 2, col = "black")
  lines(t, problo, col = "blue")
  lines(t, probhi, col = "blue")
}