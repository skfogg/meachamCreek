library(RODBC)
library(stringr)
library(lubridate)
library(zoo)
library(xts)
library(stringr)

bcol <- c("red", "orange", "gold", "chartreuse3", "cyan3") 

meacham <- odbcConnect("meacham_heat")


baroData <- sqlQuery(meacham, 
  "SELECT observations.observationTime, observations.value, metrics.units, deployments.deploymentName 
  FROM observations, metrics, deployments
  WHERE observations.metricIDX IN 
  (SELECT metricIDX FROM metrics WHERE metricName = 'BarometricPressure') 
  AND observations.deploymentIDX IN 
  (SELECT deploymentIDX FROM deployments WHERE deploymentName LIKE '%Barometer%')
  AND metrics.units IN
  (SELECT units FROM metrics WHERE metricName = 'BarometricPressure')
  AND deployments.deploymentName IN
  (SELECT deploymentName FROM deployments WHERE deploymentName LIKE '%Barometer%')
  ")

head(baroData)

SunnyWS_aug <- subset(baroData, str_detect(baroData$deploymentName, "SSunny"))
SunnyWS_oct <- subset(baroData, str_detect(baroData$deploymentName, "FSunny"))

ShadyWS_aug <- subset(baroData, str_detect(baroData$deploymentName, "SShady"))
ShadyWS_oct <- subset(baroData, str_detect(baroData$deploymentName, "FShady"))


write.csv(SunnyWS_aug, file = "d:/Users/sarah.fogg/Desktop/BaroDataForByron/sunnyWS_aug.csv")
write.csv(SunnyWS_oct, file = "d:/Users/sarah.fogg/Desktop/BaroDataForByron/sunnyWS_oct.csv")

write.csv(ShadyWS_aug, file = "d:/Users/sarah.fogg/Desktop/BaroDataForByron/shadyWS_aug.csv")
write.csv(ShadyWS_oct, file = "d:/Users/sarah.fogg/Desktop/BaroDataForByron/shadyWS_oct.csv")




