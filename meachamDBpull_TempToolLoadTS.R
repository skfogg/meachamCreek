library(RODBC)
library(stringr)
library(lubridate)
library(zoo)
library(xts)

# CONNECT TO DATABASE
meacham <- odbcConnect("meacham_heat")
meacham2 <- odbcConnect("meacham_heat_2")
temptoolmeacham <- odbcConnect("temptoolmeacham")

# FUNCTION TO QUERY OBSERVATIONS TABLE FROM MEACHAM CREEK FIELD DATABASE
queryObs <- function(channel, campaignName, infrastructureName, metricName){
  return(sqlQuery(channel, paste0("SELECT observationTime, value, metricName, infrastructureName FROM observations obs INNER JOIN deployments deps ON obs.deploymentIDX = deps.deploymentIDX INNER JOIN infrastructures infs ON deps.infrastructureIDX = infs.infrastructureIDX INNER JOIN campaigns camps ON deps.campaignIDX = camps.campaignIDX INNER JOIN metrics mets ON obs.metricIDX = mets.metricIDX WHERE campaignName = '", campaignName,"' AND metricName = '", metricName,"' AND infrastructureName = '", infrastructureName,"';")))
}

# FUNCTION TO LOAD ATMOSPHERIC TIME SERIES TO TEMPTOOL
tsLoad <- function(DBconnection, tableName, tsValues){
  for(i in 1:nrow(tsValues)){
    sqlQuery(DBconnection, paste0("INSERT INTO ", tableName," (`key`, `value`) VALUES ('", tsValues[i,1], "', '" , tsValues[i,2], "');"))
  }
}

sqlQuery(meacham, "SELECT * FROM metrics")

# COMPARE MEACHAM AND MEACHAM 2 SUNNY WS DATA
meacham1sunny <- queryObs(meacham, campaignName = "Summer", infrastructureName = "SunnyWeatherStation", metricName = "RelativeHumidity")
meacham2sunny <- queryObs(meacham2, campaignName = "Summer", infrastructureName = "SunnyWeatherStation", metricName = "RelativeHumidity")

m1s <- xts(zoo(meacham1sunny$value, order.by = ymd_hms(meacham1sunny$observationTime)))
m2s <- xts(zoo(meacham2sunny$value, order.by = ymd_hms(meacham2sunny$observationTime)))

plot(m2s["2015-08-02"], type = "o")
lines(m1s, col = "violet", type = "o")


sqlQuery(meacham2, "SELECT * FROM deployments WHERE infrastructureIDX = 3 AND campaignIDX = 1")
## Deployment Indexes, excluding the Lux Logger attached to pyranometer AND the temp/rh sensor (will write TempRH sql by hand)##
deploymentIndexes <- c(139, 140, 142:144)

################ PICK UP HERE 
sqlQuery(meacham2, "SELECT metricIDX FROM observations WHERE deploymentIDX = 139")




for(j in 1:length(deploymentIndexes)){
all <- sqlQuery(meacham2, paste0("SELECT * FROM observations obs INNER JOIN deployments deps ON obs.deploymentIDX = deps.deploymentIDX INNER JOIN infrastructures infs ON deps.infrastructureIDX = infs.infrastructureIDX INNER JOIN campaigns camps ON deps.campaignIDX = camps.campaignIDX INNER JOIN metrics mets ON obs.metricIDX = mets.metricIDX WHERE campaignName = 'Summer' AND metricName = 'RelativeHumidity' AND infrastructureName = 'SunnyWeatherStation';"))

deployIDX <- all$deploymentIDX[1]
metIDX <- all$metricIDX[1]

missingTimes <- c("2015-08-02 08:00:00", "2015-08-02 08:15:00", "2015-08-02 08:30:00", "2015-08-02 08:45:00")

## INSERT ##
for (i in 1:4){
  sqlQuery(meacham2, paste0("INSERT INTO observations (observationTime, deploymentIDX, value, metricIDX, batchIndex) VALUES ('", missingTimes[i], "', '", deployIDX, "', NULL, '", metIDX, "', NULL);"))
}
}

## TEST ##
sqlQuery(meacham2, "SELECT * FROM observations WHERE observationTime = '2015-08-02 08:15:00' AND deploymentIDX = '145'")



#######################
## CODE FOR AIR TEMP ##
#######################
all <- sqlQuery(meacham2, "SELECT * FROM observations obs INNER JOIN deployments deps ON obs.deploymentIDX = deps.deploymentIDX INNER JOIN infrastructures infs ON deps.infrastructureIDX = infs.infrastructureIDX INNER JOIN campaigns camps ON deps.campaignIDX = camps.campaignIDX INNER JOIN metrics mets ON obs.metricIDX = mets.metricIDX WHERE campaignName = 'Summer' AND metricName = 'Temperature' AND infrastructureName = 'SunnyWeatherStation';")
airTemp <- subset(all, deploymentIDX == 145)

deployIDX <- airTemp$deploymentIDX[1]
metIDX <- airTemp$metricIDX[1]

missingTimes <- c("2015-08-02 08:00:00", "2015-08-02 08:15:00", "2015-08-02 08:30:00", "2015-08-02 08:45:00")

## INSERT ##
for (i in 1:4){
  sqlQuery(meacham2, paste0("INSERT INTO observations (observationTime, deploymentIDX, value, metricIDX, batchIndex) VALUES ('", missingTimes[i], "', '", deployIDX, "', NULL, '", metIDX, "', NULL);"))
}

## TEST ##
sqlQuery(meacham2, "SELECT * FROM observations WHERE observationTime = '2015-08-02 08:15:00' AND deploymentIDX = '145'")

# ## DELETE ##
# for(i in 1:4){
# sqlQuery(meacham2, paste0("DELETE FROM observations WHERE observationTime ='", missingTimes[i],"' AND deploymentIDX = '", deployIDX,"';"))
# }








# QUERY SHADY WEATHER STATION TEMPERATURES
sqlQuery(meacham, "SELECT observationTime, value, metricName, infrastructureName FROM observations obs INNER JOIN deployments deps ON obs.deploymentIDX = deps.deploymentIDX INNER JOIN infrastructures infs ON deps.infrastructureIDX = infs.infrastructureIDX INNER JOIN campaigns camps ON deps.campaignIDX = camps.campaignIDX INNER JOIN metrics mets ON obs.metricIDX = mets.metricIDX WHERE campaignName = 'Summer' AND metricName = 'Temperature' AND infrastructureName = 'ShadyWeatherStation';")


plot(shadyTemp$value)



shadyPressure <- queryObs(meacham, "Summer", "ShadyWeatherStation", "BarometricPressure")

shadySolRad <- queryObs(meacham, "Summer", "ShadyWeatherStation", "SolarRadiation")

shadyWind <- queryObs(meacham, "Summer", "")


atmMetrics <- list("Temperature", "BarometricPressure", "SolarRadiation", "WindSpeed", "RelativeHumidity")

atmQueries <- lapply(atmMetrics, queryObs, channel = meacham, campaignName = "Summer", infrastructureName = "ShadyWeatherStation")

temps <- zoo(atmQueries[[1]]$value, order.by = ymd_hms(atmQueries[[1]]$observationTime))
baro <- zoo(atmQueries[[2]]$value, order.by = ymd_hms(atmQueries[[2]]$observationTime))

plot(baro)
plot(temps, col = "red")

nrow(atmQueries[[1]])
nrow(shadyTemp)

# DATAFRAME FOR TEMPTOOL MODEL OF MEACHAM CREEK
shadyTempTS <- data.frame(key = 1:nrow(shadyTemp), value = shadyTemp$value)


# LOAD SHADY ATMOSPHERIC TEMPERATURE INTO TEMPTOOL
tsLoad(temptoolmeacham, "ts_atmospheretemp", shadyTempTS)


#############
# TESTING....
tsLoad2 <- function(DBconnection, tableName, tsValues){
    sqlQuery(DBconnection, paste0("INSERT INTO ", tableName," (`key`, `value`) VALUES ('", , "', '" , tsValues, "');"))
}


sqlQuery(temptoolmeacham, paste0("INSERT INTO ts_relhumid (`key`, `value`) VALUES (", list(gooddims[,,1]),");"))

#lapply(shadyTempTS, tsLoad, DBconnection = temptoolmeacham, tableName = "ts_atmospheretemp")

# mapply(tsLoad, shadyTempTS, MoreArgs = list(DBconnection = temptoolmeacham, tableName = "ts_atmospheretemp"))


# gooddims <- array(c(shadyTempTS$key, shadyTempTS$value), dim = c(1,2,nrow(shadyTempTS)))
zesty <- t(matrix(c(shadyTempTS$key, shadyTempTS$value), c(960,2)))
gooddims <- array(zesty, dim = c(1,2,nrow(shadyTempTS)))
