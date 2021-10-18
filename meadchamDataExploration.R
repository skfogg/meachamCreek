library(RODBC)
library(stringr)
library(lubridate)
library(zoo)
library(xts)

bcol <- c("red", "orange", "gold", "chartreuse3", "cyan3") 

meacham <- odbcConnect("meacham_heat")

sqlColumns(meacham, "observations")$COLUMN_NAME
sqlQuery(meacham, "SELECT * FROM metrics")
sqlQuery(meacham, "SELECT * FROM infrastructures")


# In-stream water temperatures
summerstreamtempdeploymentIDX <- sqlQuery(meacham, "SELECT deploymentIDX FROM deployments WHERE deployDate = '2015-07-30' AND infrastructureIDX IN (8, 9, 10, 11, 12)")$deploymentIDX
fallstreamtempdeploymentIDX <- sqlQuery(meacham, "SELECT deploymentIDX FROM deployments WHERE deployDate = '2015-10-04' AND infrastructureIDX IN (8, 9, 10, 11, 12)")$deploymentIDX

for(i in 1:5){
  assign(c("sb1", "sb2", "sb3", "sb4", "sb5")[i], 
         sqlQuery(meacham, paste0("SELECT * FROM observations WHERE metricIDX = 1 AND deploymentIDX IN (", summerstreamtempdeploymentIDX[i], ") ")
  ))
}
for(i in 1:5){
  assign(c("fb1", "fb2", "fb3", "fb4", "fb5")[i], 
         sqlQuery(meacham, paste0("SELECT * FROM observations WHERE metricIDX = 1 AND deploymentIDX IN (", fallstreamtempdeploymentIDX[i], ") ")
  ))
}

sb1x <- xts(zoo(sb1$value, order.by = sb1$observationTime))
sb2x <- xts(zoo(sb2$value, order.by = sb2$observationTime))
sb3x <- xts(zoo(sb3$value, order.by = sb3$observationTime))
sb4x <- xts(zoo(sb4$value, order.by = sb4$observationTime))
sb5x <- xts(zoo(sb5$value, order.by = sb5$observationTime))

png(filename = "d:/Users/sarah.fogg/Desktop/MeachamDataExploration/waterTempsSummer.png",
    width = 900, height = 500)
plot(sb1x, ylim = c(15, 24), main = "Water Temperatures: Summer Campaign", ylab = expression(paste("Temperature (", degree, "C)")))
lines(sb1x, lwd = 2, col = bcol[1])
lines(sb2x, lwd = 2, col = bcol[2])
lines(sb3x, lwd = 2, col = bcol[3])
lines(sb4x, lwd = 2, col = bcol[4])
lines(sb5x, lwd = 2, col = bcol[5])
legend("topleft", c("b1", "b2", "b3", "b4", "b5"), col = bcol, lty = 1, lwd =2)
dev.off()

fb1x <- xts(zoo(fb1$value, order.by = fb1$observationTime))
fb2x <- xts(zoo(fb2$value, order.by = fb2$observationTime))
fb3x <- xts(zoo(fb3$value, order.by = fb3$observationTime))
fb4x <- xts(zoo(fb4$value, order.by = fb4$observationTime))
fb5x <- xts(zoo(fb5$value, order.by = fb5$observationTime))

png(filename = "d:/Users/sarah.fogg/Desktop/MeachamDataExploration/waterTempsFall.png",
    width = 900, height = 500)
plot(fb1x, ylim = c(11, 20), main = "Water Temperatures: Fall Campaign", ylab = expression(paste("Temperature (", degree, "C)")))
lines(fb1x, lwd = 2, col = bcol[1])
lines(fb2x, lwd = 2, col = bcol[2])
lines(fb3x, lwd = 2, col = bcol[3])
lines(fb4x, lwd = 2, col = bcol[4])
lines(fb5x, lwd = 2, col = bcol[5])
legend("topleft", c("b1", "b2", "b3", "b4", "b5"), col = bcol, lty = 1, lwd =2)
dev.off()




# plot(sb1$value, type = "l", lwd = 2, col = bcol[1], ylim = c(15, 24), main = "summer stream temperatures", ylab = "temperature")
# lines(sb2$value, lwd = 2, col = bcol[2])
# lines(sb3$value, lwd = 2, col = bcol[3])
# lines(sb4$value, lwd = 2, col = bcol[4])
# lines(sb5$value, lwd = 2, col = bcol[5])
# legend("topleft", c("b1", "b2", "b3", "b4", "b5"), col = bcol, lty = 1, lwd =2)
# 
# plot(fb1$value, type = "l", lwd = 2, col = bcol[1], ylim = c(11, 19), main = "fall stream temperatures", ylab = "temperature")
# lines(fb2$value, lwd = 2, col = bcol[2])
# lines(fb3$value, lwd = 2, col = bcol[3])
# lines(fb4$value, lwd = 2, col = bcol[4])
# lines(fb5$value, lwd = 2, col = bcol[5])
# legend("topleft", c("b1", "b2", "b3", "b4", "b5"), col = bcol, lty = 1, lwd =2)

# In-stream Specific Conductance
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

plot(fb1c$value, type = "l", lwd = 2, col = bcol[1], main = "fall stream specific conductance", ylab = "specific conductance", ylim = c(75, 100))
lines(fb2c$value, lwd = 2, col = bcol[2])
lines(fb3c$value, lwd = 2, col = bcol[3])
lines(fb4c$value, lwd = 2, col = bcol[4])
lines(fb5c$value, lwd = 2, col = bcol[5])
legend("topleft", c("b1", "b2", "b3", "b4", "b5"), col = bcol, lty = 1, lwd =2)

# In-stream Lux
for (i in 1:6){
  assign(paste0("sl", i), sqlQuery(meacham, 
                                   paste0(
                                  "SELECT * 
                                   FROM observations 
                                   WHERE metricIDX IN 
                                      (SELECT metricIDX FROM metrics WHERE metricName = 'Lux') 
                                   AND deploymentIDX IN 
                                      (SELECT deploymentIDX FROM deployments WHERE deploymentName LIKE 'SLux", i,"%')")))
}

plot(sl1$value, type="l")
lines(sl2$value, col = bcol[1])
lines(sl3$value, col = bcol[2])
lines(sl4$value, col = bcol[3])
lines(sl5$value, col = bcol[4])
 lines(sl6$value, col = bcol[5])

for (i in 1:6){
  assign(paste0("fl", i), sqlQuery(meacham, 
                                   paste0(
                                    "SELECT * 
                                     FROM observations 
                                     WHERE metricIDX IN 
                                        (SELECT metricIDX FROM metrics WHERE metricName = 'Lux') 
                                     AND deploymentIDX IN 
                                        (SELECT deploymentIDX FROM deployments WHERE deploymentName LIKE 'FLux", i,"%')")))
}

plot(fl1$value, type="l")
lines(fl2$value, col = bcol[1])
lines(fl3$value, col = bcol[2])
lines(fl4$value, col = bcol[3])
lines(fl5$value, col = bcol[4])
lines(fl6$value, col = bcol[5])

# Lux & Pyranometer Calibration Summer
library(plyr)
ssunnywslux <- sqlQuery(meacham, "SELECT * 
                          FROM observations 
                          WHERE metricIDX IN 
                              (SELECT metricIDX FROM metrics WHERE metricName = 'Lux') 
                          AND deploymentIDX IN 
                              (SELECT deploymentIDX FROM deployments WHERE deploymentName LIKE 'SSunnyWSLux%')")
ssunnywspy <- sqlQuery(meacham, "SELECT * 
                          FROM observations 
                        WHERE metricIDX IN 
                        (SELECT metricIDX FROM metrics WHERE metricName = 'SolarRadiation') 
                        AND deploymentIDX IN 
                        (SELECT deploymentIDX FROM deployments WHERE deploymentName LIKE 'SSunnyWSPyranometerUp%')")

sunnyLuxSolRad <- join(data.frame(observationTime = ssunnywslux$observationTime, sunnyLux = ssunnywslux$value), 
                       data.frame(observationTime = ssunnywspy$observationTime, sunnySolRad = ssunnywspy$value), 
                       by = "observationTime", 
                       type= "inner")
sunnyLuxCal <- lm(sunnySolRad ~ sunnyLux, data = sunnyLuxSolRad)
summary(sunnyLuxCal)


plot(sunnySolRad ~ sunnyLux, data = sunnyLuxSolRad)
abline(sunnyLuxCal, col = "violet", lwd = 2)

sshadywslux <- sqlQuery(meacham, "SELECT * 
                          FROM observations 
                        WHERE metricIDX IN 
                        (SELECT metricIDX FROM metrics WHERE metricName = 'Lux') 
                        AND deploymentIDX IN 
                        (SELECT deploymentIDX FROM deployments WHERE deploymentName LIKE 'SShadyWSLux%')")
sshadywspy <- sqlQuery(meacham, "SELECT * 
                       FROM observations 
                       WHERE metricIDX IN 
                       (SELECT metricIDX FROM metrics WHERE metricName = 'SolarRadiation') 
                       AND deploymentIDX IN 
                       (SELECT deploymentIDX FROM deployments WHERE deploymentName LIKE 'SShadyWSPyranometerUp%')")

shadyLuxSolRad <- join(data.frame(observationTime = sshadywslux$observationTime, shadyLux = sshadywslux$value), 
                       data.frame(observationTime = sshadywspy$observationTime, shadySolRad = sshadywspy$value), 
                       by = "observationTime", 
                       type= "inner")
shadyLuxCal <- lm(shadySolRad ~ shadyLux, data = shadyLuxSolRad)
summary(shadyLuxCal)

plot(shadySolRad ~ shadyLux, data = shadyLuxSolRad)
abline(shadyLuxCal, col = "violet", lwd = 2)

points(sunnySolRad ~ sunnyLux, data = sunnyLuxSolRad, col = "gray")
abline(sunnyLuxCal, col = "violet", lwd = 2)

summerLuxSolRad <- join(sunnyLuxSolRad, shadyLuxSolRad, by = "observationTime", type= "inner")
head(summerLuxSolRad)

summerLuxSolRadLong <- data.frame(observationTime = rep(summerLuxSolRad$observationTime, times = 2),
                                  lux = c(summerLuxSolRad$sunnyLux, summerLuxSolRad$shadyLux),
                                  solRad = c(summerLuxSolRad$sunnySolRad, summerLuxSolRad$shadySolRad),
                                  site = factor(c(rep("sunny", times = length(summerLuxSolRad$observationTime)), 
                                                  rep("shady", times = length(summerLuxSolRad$observationTime)))))
summerCal <- lm(solRad ~ lux * site, data = summerLuxSolRadLong)
summary(summerCal)

plot(solRad ~ lux, data = subset(summerLuxSolRadLong, summerLuxSolRadLong$site == "sunny"))
points(solRad ~ lux, data = subset(summerLuxSolRadLong, summerLuxSolRadLong$site == "shady"), col ="gray")
abline(summerCal, col = "gray", lwd = 2)
abline(coef(summerCal)[1] + coef(summerCal)[3], coef(summerCal)[2] + coef(summerCal)[4], lwd=2)
 
# work-hotelling standard error?



# Lux & Pyranometer Calibration Fall
library(plyr)
fsunnywslux <- sqlQuery(meacham, "SELECT * 
                        FROM observations 
                        WHERE metricIDX IN 
                        (SELECT metricIDX FROM metrics WHERE metricName = 'Lux') 
                        AND deploymentIDX IN 
                        (SELECT deploymentIDX FROM deployments WHERE deploymentName LIKE 'FSunnyWSLux%')")
fsunnywspy <- sqlQuery(meacham, "SELECT * 
                       FROM observations 
                       WHERE metricIDX IN 
                       (SELECT metricIDX FROM metrics WHERE metricName = 'SolarRadiation') 
                       AND deploymentIDX IN 
                       (SELECT deploymentIDX FROM deployments WHERE deploymentName LIKE 'FSunnyWSPyranometerUp%')")

fsunnyLuxSolRad <- join(data.frame(observationTime = fsunnywslux$observationTime, sunnyLux = fsunnywslux$value), 
                       data.frame(observationTime = fsunnywspy$observationTime, sunnySolRad = fsunnywspy$value), 
                       by = "observationTime", 
                       type= "inner")

fshadywslux <- sqlQuery(meacham, "SELECT * 
                          FROM observations 
                        WHERE metricIDX IN 
                        (SELECT metricIDX FROM metrics WHERE metricName = 'Lux') 
                        AND deploymentIDX IN 
                        (SELECT deploymentIDX FROM deployments WHERE deploymentName LIKE 'FShadyWSLux%')")
fshadywspy <- sqlQuery(meacham, "SELECT * 
                       FROM observations 
                       WHERE metricIDX IN 
                       (SELECT metricIDX FROM metrics WHERE metricName = 'SolarRadiation') 
                       AND deploymentIDX IN 
                       (SELECT deploymentIDX FROM deployments WHERE deploymentName LIKE 'FShadyWSPyranometerUp%')")

fshadyLuxSolRad <- join(data.frame(observationTime = fshadywslux$observationTime, shadyLux = fshadywslux$value), 
                       data.frame(observationTime = fshadywspy$observationTime, shadySolRad = fshadywspy$value), 
                       by = "observationTime", 
                       type= "inner")
fallLuxSolRad <- join(fsunnyLuxSolRad, fshadyLuxSolRad, by = "observationTime", type= "inner")

fallLuxSolRadLong <- data.frame(observationTime = rep(fallLuxSolRad$observationTime, times = 2),
                                  lux = c(fallLuxSolRad$sunnyLux, fallLuxSolRad$shadyLux),
                                  solRad = c(fallLuxSolRad$sunnySolRad, fallLuxSolRad$shadySolRad),
                                  site = factor(c(rep("sunny", times = length(fallLuxSolRad$observationTime)), 
                                                  rep("shady", times = length(fallLuxSolRad$observationTime)))))
fallCal <- lm(solRad ~ lux * site, data = fallLuxSolRadLong)
summary(fallCal)

plot(solRad ~ lux, data = subset(fallLuxSolRadLong, fallLuxSolRadLong$site == "sunny"))
points(solRad ~ lux, data = subset(fallLuxSolRadLong, fallLuxSolRadLong$site == "shady"), col ="gray")
abline(fallCal, col = "gray", lwd = 2)
abline(coef(fallCal)[1] + coef(fallCal)[3], coef(fallCal)[2] + coef(fallCal)[4], lwd=2)


