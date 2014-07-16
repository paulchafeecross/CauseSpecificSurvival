#Function to create the Z vector for the spline knots
Z_foo <- function(num.knots, start, end){
  #Input: 
  # num.knots = number of knots
  # start = minimum of the covariate
  # end = maximum of the covariate
  #Define knots used for the p-spline at equal spaced quantiles of the covariate
  covariate <- seq(start, end)

  #Obtain the design matrix for random coefficients using a radial basis
  knots<-quantile(unique(covariate), seq(0,1,length=(num.knots+2))[-c(1,(num.knots+2))])
  Z_K<-(abs(outer(covariate,knots,"-")))^3
  OMEGA_all<-(abs(outer(knots,knots,"-")))^3
  svd.OMEGA_all<-svd(OMEGA_all)
  sqrt.OMEGA_all<-t(svd.OMEGA_all$v %*% (t(svd.OMEGA_all$u)*sqrt(svd.OMEGA_all$d)))
  Z <- t(solve(sqrt.OMEGA_all,t(Z_K)))
  return(Z)
}