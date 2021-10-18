

#### Conductivity Calibrations ####
headernames <- c("reading", "dateTime", "lowRange", "temp", "spLowRange")

sn61 <- read.csv("d:/Users/sarah.fogg/Desktop/All Logger Calibrations/ConductivityCalibrations/2016_09_13/10104861.csv",
         stringsAsFactors = F, col.names = headernames)

sn62 <- read.csv("d:/Users/sarah.fogg/Desktop/All Logger Calibrations/ConductivityCalibrations/2016_09_13/10104862.csv",
                 stringsAsFactors = F, col.names = headernames)

sn63 <- read.csv("d:/Users/sarah.fogg/Desktop/All Logger Calibrations/ConductivityCalibrations/2016_09_13/10104863.csv",
                 stringsAsFactors = F, col.names = headernames)

sn64 <- read.csv("d:/Users/sarah.fogg/Desktop/All Logger Calibrations/ConductivityCalibrations/2016_09_13/10104864.csv",
                 stringsAsFactors = F, col.names = headernames)

sn65 <- read.csv("d:/Users/sarah.fogg/Desktop/All Logger Calibrations/ConductivityCalibrations/2016_09_13/10104865.csv",
                 stringsAsFactors = F, col.names = headernames)

sn66 <- read.csv("d:/Users/sarah.fogg/Desktop/All Logger Calibrations/ConductivityCalibrations/2016_09_13/10104866.csv",
                 stringsAsFactors = F, col.names = headernames)

sn67 <- read.csv("d:/Users/sarah.fogg/Desktop/All Logger Calibrations/ConductivityCalibrations/2016_09_13/10104867.csv",
                 stringsAsFactors = F, col.names = headernames)

sn69 <- read.csv("d:/Users/sarah.fogg/Desktop/All Logger Calibrations/ConductivityCalibrations/2016_09_13/10104869.csv",
                 stringsAsFactors = F, col.names = headernames)

sn75 <- read.csv("d:/Users/sarah.fogg/Desktop/All Logger Calibrations/ConductivityCalibrations/2016_09_13/10104875.csv",
                 stringsAsFactors = F, col.names = headernames)

sn77 <- read.csv("d:/Users/sarah.fogg/Desktop/All Logger Calibrations/ConductivityCalibrations/2016_09_13/10104877.csv",
                 stringsAsFactors = F, col.names = headernames)
sn61 <- xts(zoo(sn61$spLowRange, order.by = mdy_hms(sn61$dateTime)))
sn62 <- xts(zoo(sn62$spLowRange, order.by = mdy_hms(sn62$dateTime)))
sn63 <- xts(zoo(sn63$spLowRange, order.by = mdy_hms(sn63$dateTime)))
sn64 <- xts(zoo(sn64$spLowRange, order.by = mdy_hms(sn64$dateTime)))
sn65 <- xts(zoo(sn65$spLowRange, order.by = mdy_hms(sn65$dateTime)))
sn66 <- xts(zoo(sn66$spLowRange, order.by = mdy_hms(sn66$dateTime)))
sn67 <- xts(zoo(sn67$spLowRange, order.by = mdy_hms(sn67$dateTime)))
sn69 <- xts(zoo(sn69$spLowRange, order.by = mdy_hms(sn69$dateTime)))
sn75 <- xts(zoo(sn75$spLowRange, order.by = mdy_hms(sn75$dateTime)))
sn77 <- xts(zoo(sn77$spLowRange, order.by = mdy_hms(sn77$dateTime)))

timeperiod <- "2016-09-13 12:10:00/2016-09-13 14:00:00"

plot(sn61[timeperiod], ylim = c(10, 95))
lines(sn62[timeperiod], col = 2)
lines(sn63[timeperiod], col = 3)
lines(sn64[timeperiod], col = 4)
lines(sn65[timeperiod], col = 5)
lines(sn66[timeperiod], col = 6)
lines(sn67[timeperiod], col = 7)
lines(sn69[timeperiod], col = 8)
lines(sn75[timeperiod], col = 9)
lines(sn77[timeperiod], col = 10)

spCond <- c(10, 30.7, 27.2, 34.8, 
         28.7, 39.9, 36.5, 51.9, 50.8, 61.1, 62.0, 70.9, 71.2, 74.4,
         74.2, 75.1, 75.0, 75.2, 84.4, 84.4, 88.6, 89.7)
time <- c("2016-09-13 12:11:00", "2016-09-13 12:17:00", "2016-09-13 12:23:00", "2016-09-13 12:24:00",
          "2016-09-13 12:27:00", "2016-09-13 12:28:00", "2016-09-13 12:34:00", "2016-09-13 12:38:00", "2016-09-13 12:42:00", "2016-09-13 12:45:00", "2016-09-13 12:52:00", "2016-09-13 12:54:00", "2016-09-13 13:02:00", "2016-09-13 13:03:00",
          "2016-09-13 13:16:00", "2016-09-13 13:17:00", "2016-09-13 13:27:00", "2016-09-13 13:28:00", "2016-09-13 13:29:00", "2016-09-13 13:34:00", "2016-09-13 13:35:00", "2016-09-13 14:00:00")
ysi <- xts(zoo(spCond, order.by = ymd_hms(time)))

plot(sn61[time], ylim = c(10, 95))
lines(sn62[time], col = 2)
lines(sn63[time], col = 3)
lines(sn64[time], col = 4)
lines(sn65[time], col = 5)
lines(sn66[time], col = 6)
lines(sn67[time], col = 7)
lines(sn69[time], col = 8)
lines(sn75[time], col = 9)
lines(sn77[time], col = 10)
points(ysi, pch = 19)



plot(coredata(ysi) ~ coredata(sn61[time]))
abline(lm(coredata(ysi) ~ coredata(sn61[time])))

plot(coredata(ysi) ~ coredata(sn62[time]))
abline(lm(coredata(ysi) ~ coredata(sn62[time])))

