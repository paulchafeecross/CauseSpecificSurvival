# script to look at the feeder fetus recovery data
setwd("~/BitBucket/VIT/Analysis")
rm(list = ls())
library(lubridate)

#read in the data
Fetus <- read.table('FetusRecoveryField.txt', header = T)

# reformat the dates
Fetus$Date2 <- mdy(Fetus$Date)
Fetus$DOY <- yday(Fetus$Date2)
Fetus <- Fetus[order(Fetus$DOY),] # sort by doy
Fetus <- subset(Fetus, is.na(Fetus$Date2) == F) # remove the NAs
#Fetus$DOY2 <- as.Date("01/01/2013", "%m/%d/%Y") + Fetus$DOY - 1

t <- c( 1, 32, 60, 91, 121, 152) # axis labels on the 1st of the month
labs <- c("Jan 1", "Feb 1", "Mar 1", "Apr 1", "May 1", "June 1")

# PLOT
par(mfrow = c(1,2))
hist(Fetus$DOY, breaks = 10, main = '', xlab = "Day of Year", xaxt = "n", col = 'grey')
axis(1, at = t, labels = labs)

d <- density(Fetus$DOY)
plot(d, main = '', xlab = "Day of Year", xaxt = "n", bg = 'grey', bty = "none")
axis(1, at = t, labels = labs)
polygon(d, col = 'grey')
