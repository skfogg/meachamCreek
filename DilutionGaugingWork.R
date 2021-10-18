
#### Dilution Gauging ####
library(RODBC)
library(stringr)
library(lubridate)
library(zoo)
library(xts)

bcol <- c("red", "orange", "gold", "chartreuse3", "cyan3") 

meacham <- odbcConnect("meacham_heat")

hand <- read.csv("d:/Users/sarah.fogg/Desktop/MeachamCreekData/HandMeasurements_Summer2015.csv")
hand$datetime <- mdy_hm(paste(as.character(hand$date), as.character(hand$time)))
spCondHand <- subset(hand, measurement == "Sp Conductivity")[,c(3,4,5,6,8)]

summerstreamtempdeploymentIDX <- sqlQuery(meacham, "SELECT deploymentIDX FROM deployments WHERE deployDate = '2015-07-30' AND infrastructureIDX IN (8, 9, 10, 11, 12)")$deploymentIDX
fallstreamtempdeploymentIDX <- sqlQuery(meacham, "SELECT deploymentIDX FROM deployments WHERE deployDate = '2015-10-04' AND infrastructureIDX IN (8, 9, 10, 11, 12)")$deploymentIDX

for(i in 1:5){
  assign(c("sb1c", "sb2c", "sb3c", "sb4c", "sb5c")[i], 
         sqlQuery(meacham, paste0("SELECT * FROM observations WHERE metricIDX = 5 AND deploymentIDX IN (", summerstreamtempdeploymentIDX[i], ") ")
         ))
}

for(i in 1:5){
  assign(c("fb1c", "fb2c", "fb3c", "fb4c", "fb5c")[i], 
         sqlQuery(meacham, paste0("SELECT * FROM observations WHERE metricIDX = 5 AND deploymentIDX IN (", fallstreamtempdeploymentIDX[i], ") ")
         ))
}

plot(sb1c$value, type = "l", lwd = 2, col = bcol[1], main = "summer stream specific conductance", ylab = "specific conductance")
lines(sb2c$value, lwd = 2, col = bcol[2])
lines(sb3c$value, lwd = 2, col = bcol[3])
lines(sb4c$value, lwd = 2, col = bcol[4])
lines(sb5c$value, lwd = 2, col = bcol[5])
legend("topleft", c("b1", "b2", "b3", "b4", "b5"), col = bcol, lty = 1, lwd =2)

# measurements taken every 30 seconds #
cb1s <- xts(zoo(sb1c$value, order.by = ymd_hms(sb1c$observationTime)))
cb2s <- xts(zoo(sb2c$value, order.by = ymd_hms(sb2c$observationTime)))
cb3s <- xts(zoo(sb3c$value, order.by = ymd_hms(sb3c$observationTime)))
cb4s <- xts(zoo(sb4c$value, order.by = ymd_hms(sb4c$observationTime)))
cb5s <- xts(zoo(sb5c$value, order.by = ymd_hms(sb5c$observationTime)))


spCondHand1 <- xts(zoo(subset(spCondHand, location == "Boundary 1")$value, order.by = subset(spCondHand, location == "Boundary 1")$datetime))
spCondHand2 <- xts(zoo(subset(spCondHand, location == "Boundary 2")$value, order.by = subset(spCondHand, location == "Boundary 2")$datetime))
spCondHand3 <- xts(zoo(subset(spCondHand, location == "Boundary 3")$value, order.by = subset(spCondHand, location == "Boundary 3")$datetime))
spCondHand4 <- xts(zoo(subset(spCondHand, location == "Boundary 4")$value, order.by = subset(spCondHand, location == "Boundary 4")$datetime))
spCondHand5 <- xts(zoo(subset(spCondHand, location == "Boundary 5")$value, order.by = subset(spCondHand, location == "Boundary 5")$datetime))

plot(cb1s, main = "Boundary 1 Logger")
points(spCondHand1, col ="red", pch = 16)

plot(cb2s, main = "Boundary 2 Logger")
points(spCondHand2, col ="red", pch = 16)

plot(cb3s, main = "Boundary 3 Logger")
points(spCondHand3, col ="red", pch = 16)

plot(cb4s, main = "Boundary 4 Logger", ylim = c(72,85))
points(spCondHand4, col ="red", pch = 16)

plot(cb5s, main = "Boundary 5 Logger", ylim = c(70,max(cb5s)))
points(spCondHand5, col ="red", pch = 16)




