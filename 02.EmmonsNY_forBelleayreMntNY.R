#Setting working directory -----------------------------------------------------------------------------------------

setwd("~/Columbia/Climate and Society/Quant/Final Project/Data - V2")

install.packages('pracma')
library(pracma)

#upploaing data ----------------------------------------------------------------------------------------------------
newYorkRaw <- read.csv('EmmonsNY_forBelleayreMntNY.csv')

#Creating a date format --------------------------------------------------------------------------------------------
date <- as.Date(newYorkRaw$DATE, format='%m/%d/%Y')
newYork <- cbind(newYorkRaw,date)

#Finding winter
months <- as.numeric(format(newYork$date,'%m'))
year <- as.numeric(format(newYork$date,'%Y'))
newYork <- cbind(newYork,months,year)

decIndex <- which(newYork$months == 12)
janIndex <- which(newYork$months == 1)
febIndex <- which(newYork$months == 2)
marIndex <- which(newYork$months == 3)

winterIndex <- c(decIndex,janIndex,febIndex,marIndex)

winterDataRaw <- newYork[winterIndex,]
snow <- winterDataRaw$SNOW
snow[NA] <- 0 #Getting rid of NA in snow
snow

winterData <- as.data.frame(cbind(as.POSIXct(winterDataRaw$date, format = "%m/%d/%Y %I:%M:%S %p" , tz = "GMT"),winterDataRaw$year,winterDataRaw$months,winterDataRaw$TMAX,winterDataRaw$TMIN,winterDataRaw$SNOW))
colnames(winterData) <- c('date','year','month','maxTemp','minTemp','snow')

winterData <- winterData[do.call(order, winterData), ] 
obs <- nrow(winterData)
obs #4365
row.names(winterData) <- 1:obs
winterData <- winterData[-(4366:13122),]
nrow(winterData)

#Winter mean ------------------------------------------------------------------------------------------------------

#Winter Lenght 
january <- 31
december <- 31
february <- 28
march <- 31

normalWinterDays <- january+december+february+march
normalWinterDays #121
leapWinterDays <- normalWinterDays+1
leapWinterDays #122

n <- 2019-1983
n #36

leapYear <- c(2016, 2012, 2008, 2004, 2000, 1996, 1992, 1988, 1984)

'RUNNING MEAN = this part is still not working'

#Example of code before running the for loop
jan <- which(winterData$year == 1984 & winterData$month == 1) 
feb <- which(winterData$year == 1984 & winterData$month == 2) 
mar <- which(winterData$year == 1984 & winterData$month == 3) 
dec <- which(winterData$year == 1984-1 & winterData$month ==12) 

yearlyIndex <- c(dec,jan,feb,mar)
yearlyIndex

mean1984 <- mean(winterData$minTemp[yearlyIndex])
mean1984

#Creating a matrix for the values
yearMean <- matrix(NA,nrow=n,ncol=4)
row.names(yearMean) <- 1984:2019
colnames(yearMean) <- c('minTemp','maxTemp','averageT','snow')

#Setting up the loop 
for (increment in 1:n){
  yearData <- 1983 + increment
  jan <- which(winterData$year == yearData & winterData$month == 1) 
  feb <- which(winterData$year == yearData & winterData$month == 2) 
  mar <- which(winterData$year == yearData & winterData$month == 3) 
  dec <- which(winterData$year == yearData-1 & winterData$month ==12) 
  
  yearlyIndex <- c(dec,jan,feb,mar)
  yearlyIndex
  
  yearMean[increment,1] <- mean(winterData$minTemp[yearlyIndex])
  yearMean[increment,2] <- mean(winterData$maxTemp[yearlyIndex])
  yearMean[increment,3] <- mean(c((winterData$maxTemp[yearlyIndex]),(winterData$minTemp[yearlyIndex])))
  yearMean[increment,4] <- mean(winterData$snow[yearlyIndex], na.rm=TRUE)
  
}

yearMean <- as.data.frame(yearMean)
years <- 1984:2019
yearMean <- cbind(years,yearMean)
length(yearMean$maxTemp)


plot(years,yearMean$maxTemp, type = 'l')

plot(years,yearMean$minTemp, type = 'l')

plot(years,yearMean$averageT, type = 'l')

plot(years,yearMean$snow, type = 'l')
abline(yearRegression,col = 'coral3', lwd=2)

yearRegression <- lm(yearMean$years~yearMean$snow)

plot(yearMean$years,yearMean$snow)
yearRegression$coefficients

#Calculating the correlation and the regression ---------------------------------------------------------------------------------------------------

pairs(yearMean, col='lightblue3', pch = 19)

corWinter <- cor(yearMean) #correlation values for our variables
corWinter

regression <- lm(yearMean$snow~yearMean$maxTemp)
summary(regression)
coeffs <- regression$coefficients

plot(yearMean$maxTemp,yearMean$snow)
abline(regression,col = 'coral3', lwd=2)

#Trying to add ENSO --------------------------------------------------------------------------------------------------------------------------------

#Attempt 1
ENSOraw <- read.table('nino34.txt', header = F) #NOAA from <- https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Nino34/
colnames(ENSOraw) <- tolower(c('year',month.abb))

ENSO <- cbind(ENSOraw$oct,ENSOraw$nov,ENSOraw$dec)
row.names(ENSO) <- 1870:2019

ENSOmean <-  rowMeans(ENSO)
ENSOmean <- round(ENSOmean,digits=2)

ENSO <- cbind(ENSO, ENSOmean)
colnames(ENSO) <- c('oct','nov','dec','mean')

ENSO <- (ENSO[-(1:114),])
ENSO <- as.data.frame(ENSO)

yearMeanEnso <- cbind(yearMean,ENSO$mean)
colnames(yearMeanEnso) <- c('minTemp','maxTemp','averageT','snow','ENSO OND mean')

yearMeanEnso['2019',5] <- 0
yearMeanEnso <- round(yearMeanEnso,digits=2)

pairs(yearMeanEnso)
ENSOcor <- cor(yearMeanEnso)

ENSOregression <- lm(yearMeanEnso$snow~yearMeanEnso$maxTemp+yearMeanEnso$`ENSO OND mean`)
summary(ENSOregression)
coeffsENSO <- ENSOregression$coefficients

#Attempt 2
ENSO2raw <- read.csv('ENSO1JF.csv', header=FALSE) #https://www.esrl.noaa.gov/psd/enso/mei/

ENSO2 <- (ENSO2raw[-(1:5),])
ENSO2 <- as.data.frame(ENSO2)
colnames(ENSO2) <- c('year','ENSOindex')

ENSOcor <- cor(ENSO2$ENSOindex,yearMean$snow)
ENSOcor

ENSOtemp <- cor(ENSO2$ENSOindex,yearMean$maxTemp)
ENSOtemp

ENSOregression2 <- lm(yearMeanEnso$snow~yearMeanEnso$maxTemp+yearMeanEnso$`ENSO2$ENSOindex`)
summary(ENSOregression2)
coeffsENSO2 <- ENSOregression2$coefficients 
coeffsENSO2

#Calculating skiers per state ----------------------------------------------------------------------------------------------------------

skiersRaw <- read.csv('visitsnortheast.csv', header = T)
nyShare <- 0.2979

nySkiers <- skiersRaw$northeast*nyShare

skiersCor <- cor(nySkiers,yearMean$snow)
skiersCor

skiersRegression <- lm(nySkiers~yearMean$snow+yearMean$maxTemp)
summary(skiersRegression)

tempSkier <- cor(nySkiers,yearMean$maxTemp)
tempSkier

skiersTempReg <- lm(nySkiers~yearMean$maxTemp)
summary(skiersTempReg)


#calculating the mean Tmax

meanTmax <- mean(yearMean$maxTemp)
meanTmax

skiersTempENSOreg <- lm(nySkiers~yearMean$maxTemp+ENSO2$ENSOindex)
summary(skiersTempENSOreg)

#Correlating the NAO ----------------------------------------------------------------------------------------------------------------------

NAO <- read.table('nao_index.txt', header = T)

#Creating a matrix for the values
NAOmean <- matrix(NA,nrow=n,ncol=1)
row.names(NAOmean) <- 1984:2019
colnames(NAOmean) <- c('index')

#Setting up the loop 
for (increment in 1:n){
  yearData <- 1983 + increment
  janNAO <- which(NAO$YEAR == yearData & NAO$MONTH == 1) 
  febNAO <- which(NAO$YEAR == yearData & NAO$MONTH == 2) 
  marNAO <- which(NAO$YEAR == yearData & NAO$MONTH == 3) 
  decNAO <- which(NAO$YEAR == yearData-1 & NAO$MONTH ==12) 
  
  yearlyIndex <- c(decNAO,janNAO,febNAO,marNAO)
  yearlyIndex
  
  NAOmean[increment,1] <- mean(NAO$INDEX[yearlyIndex])
  
}

NAOmean <- as.data.frame(NAOmean)
years <- 1984:2019

NAOtemp <- cor(NAOmean,yearMean$maxTemp)
NAOtemp

NAOsnow <- cor(NAOmean,yearMean$snow)
NAOsnow

#Detrend -------------------------------------------------------------------------------------------------------------------------------

detrendMaxTemp <-  detrend(yearMean$maxTemp)
detrendMaxTemp

detrendSkiers <- detrend(nySkiers)
detrendSkiers

cor(yearMean$maxTemp, detrendSkiers)





