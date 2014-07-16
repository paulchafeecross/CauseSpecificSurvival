# function to plot the interval censored VIT Data, excluding the unknowns
VITplot_interval2 <- function(Data, LWD){
  require(lubridate)
  if(missing(LWD)){LWD <- 2}
  
  Data <- subset(Data, Data$VIT_Status != "Unknown")
  Data <- subset(Data, is.na(Data$StartDOY) == F)
  
  Data <- Data[order(Data$VIT_Status, Data$CapDOY),] #order the data by event and capture date
  n.elk <- length(unique(Data$Elk_ID))
  Data$ElkYrID  <- as.factor(paste(Data$Elk_ID, year(Data$CaptureDate)))
  Data$ElkYrID2 <- as.numeric(reorder(Data$ElkYrID, seq(1:dim(Data)[1])))
  Data$CapDOY   <- Data$CapDOY + as.Date("2013/01/01")
  Data$StartDOY <- Data$StartDOY + as.Date("2013/01/01")
  Data$EndDOY   <- Data$EndDOY + as.Date("2013/01/01")
  
  # axis labels
  m <- as.Date(c('01/01/2013', '01/02/2013', "01/03/2013", "01/04/2013",
                 "01/05/2013", "01/06/2013", "01/07/2013"), '%d/%m/%Y')
  labs <- c("Jan 1", "Feb 1", "Mar 1", "Apr 1", "May 1", "Jun 1", "Jul 1")
    
  plot(Data$CapDOY, Data$ElkYrID2, xaxt = "n", type = 'n', 
       xlim = c(min(Data$CapDOY), max(Data$EndDOY, na.rm = T)), 
       xlab = '', ylab = 'Individual elk', bty = 'n', tck = 0.02)
   
  for(i in 1:dim(Data)[1]){
    j <- Data$ElkYrID2[i]
    lines(c(Data$CapDOY[i], Data$StartDOY[i]), c(j,j), lwd = LWD, col = 'grey')

    if(Data$VIT_Status[i] == "Abortion"){
      lines(c(Data$StartDOY[i], Data$EndDOY[i]), c(j,j), lwd = LWD, col = 'red')
      }
    
    if(Data$VIT_Status[i] == "Parturition"){
      lines(c(Data$StartDOY[i], Data$EndDOY[i]), c(j,j), lwd = LWD, col = 'blue')
    }
  }
  axis(1, at = m, labels = labs, tck = 0.02)
  
  # legend('topleft', c('abortion', 'birth'), 
  #      lwd = c(2,2,2), col = c('red', 'blue'), bty = 'n')
}