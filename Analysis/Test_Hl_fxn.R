rm(list = ls())
source("Ts_Hl_fxn.r")
H <- Heatloss(Ta = 0, Ts = 10, Ws = 1, SA = 1, VertDim = 1, d = 1, region = "TR")
H <- Heatloss(Ta = c(0,2), Ts = c(10,2), Ws = c(1,0), SA = c(1,1), VertDim = c(1,1), d = c(1,1), region = c("TR", "NK"))

test <- head(read.table("ThermalData2014.txt", header = T))

Htest <- Heatloss(Ta = test$Ta, Ts = test$meanTs, Ws = test$Ws, SA = test$SA, VertDim = test$VertDim, region = test$Region)

(test.out <- test[,17:24])
Htest
(Dif <- test.out - Htest)
#NOT YET WORKING
