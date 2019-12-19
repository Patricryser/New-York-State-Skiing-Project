#Setting working directory -----------------------------------------------------------------------------------------

setwd("~/Columbia/Climate and Society/Quant/Final Project/Data")
setwd("C:/Users/Patri/polybox/Columbia Academics/Quant/Project/DataV1")
#upploaing data PLEASANTmnt FOR ELK MOUNTAIN PA----------------------------------------------------------------------------------------------------
ElkRaw <- read.csv('PleasantMntPA_for_ElkMnt.csv')

#Creating a date format --------------------------------------------------------------------------------------------
date <- as.Date(ElkRaw$DATE, format='%m/%d/%Y')
Elk <- cbind(ElkRaw,date)


#Finding winter
months <- as.numeric(format(Elk$date,'%m'))
year <- as.numeric(format(Elk$date,'%Y'))
Elk <- cbind(Elk,months,year)

decIndex <- which(Elk$months == 12)
janIndex <- which(Elk$months == 1)
febIndex <- which(Elk$months == 2)
marIndex <- which(Elk$months == 3)

winterPAIndex <- c(decIndex,janIndex,febIndex,marIndex)

winterElkRaw <- Elk[winterPAIndex,]
snow <- winterElkRaw$SNOW
snow[NA] <- 0 #Getting rid of NA in snow
snow

winterElkData <- as.data.frame(cbind(as.POSIXct(winterElkRaw$DATE, format = "%m/%d/%Y %I:%M:%S %p" , tz = "GMT"),winterElkRaw$year,winterElkRaw$months,winterElkRaw$TMAX,winterElkRaw$TMIN,winterElkRaw$SNOW))
colnames(winterElkData) <- c('date','year','month','maxTemp','minTemp','snow')


#winterElkData <- winterElkData[do.call(order, winterElkData), ] 
#obs <- nrow(winterElkData)
#obs #4365
#row.names(winterElkData) <- 1:obs
#winterElkData <- winterElkData[-(4366:13122),]
#nrow(winterElkData)

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

#'RUNNING MEAN = this part is still not working'

#Example of code before running the for loop
#jan <- which(winterData$year == 1984 & winterData$month == 1) 
#feb <- which(winterData$year == 1984 & winterData$month == 2) 
#mar <- which(winterData$year == 1984 & winterData$month == 3) 
#dec <- which(winterData$year == 1984-1 & winterData$month ==12) 

#yearlyIndex <- c(dec,jan,feb,mar)
#yearlyIndex

#mean1984 <- mean(winterData$minTemp[yearlyIndex])
#mean1984

#Creating a matrix for the values
yearElkMean <- matrix(NA,nrow=n,ncol=4)
row.names(yearElkMean) <- 1984:2019
colnames(yearElkMean) <- c('minTemp','maxTemp','averageT','snow')

#Setting up the loop 
for (increment in 1:n){
  yearElkData <- 1983 + increment
  jan <- which(winterElkData$year == yearElkData & winterElkData$month == 1) 
  feb <- which(winterElkData$year == yearElkData & winterElkData$month == 2) 
  mar <- which(winterElkData$year == yearElkData & winterElkData$month == 3) 
  dec <- which(winterElkData$year == yearElkData-1 & winterElkData$month ==12) 
  
  yearlyElkIndex <- c(dec,jan,feb,mar)
  yearlyElkIndex
  
  yearElkMean[increment,1] <- mean(winterElkData$minTemp[yearlyElkIndex], na.rm=T)
  yearElkMean[increment,2] <- mean(winterElkData$maxTemp[yearlyElkIndex], na.rm=T)
  #  yearElkMean[increment,3] <- mean(c((winterElkData$maxTemp[yearlyElkIndex]),(winterElkData$minTemp[yearlyElkIndex],na.rm=F)))
  yearElkMean[increment,4] <- mean(winterElkData$snow[yearlyElkIndex], na.rm=TRUE)
  
}

years <- 1984:2019
yearElkMean <- as.data.frame(yearElkMean)
yearElkMean <- cbind(years,yearElkMean)
length(yearElkMean$maxTemp)

#The pnorm function to determine the p values.
nowarming <- yearElkMean$maxTemp
sd(nowarming)
warming1.5 <- (yearElkMean$maxTemp)+2.7
sd(warming1.5)
warming2 <- (yearElkMean$maxTemp)+3.6

sd(yearElkMean$maxTemp)
PA_patric <- pnorm(41, mean(nowarming), sd(nowarming)) #no warming ()
PA_patric1.5 <- pnorm(41, mean(warming1.5), sd(warming1.5)) #1.5 degree warming
PA_patric2 <- pnorm(41, mean(warming2), sd(warming2)) #2 degree warming
PA_patric
PA_patric1.5
PA_patric2
hist(nowarming)
hist(warming1.5)
hist(warming2)




plot(years,yearElkMean$maxTemp, type = 'l')

plot(years,yearElkMean$minTemp, type = 'l')

plot(years,yearElkMean$averageT, type = 'l')

plot(years,yearElkMean$snow, type = 'l')



yearElkRegression <- lm(yearElkMean$years~yearElkMean$snow)
abline(yearElkRegression,col = 'coral3', lwd=2)


plot(yearElkMean$years,yearElkMean$snow)
yearElkRegression$coefficients

#Calculating the correlation and the regression ---------------------------------------------------------------------------------------------------

pairs(yearElkMean, col='lightblue3', pch = 19)

corElkWinter <- cor(yearElkMean) #correlation values for our variables
corElkWinter

regressionElk <- lm(yearElkMean$snow~yearElkMean$maxTemp)
summary(regressionElk)
coeffsElk <- regressionElk$coefficients

plot(yearElkMean$maxTemp,yearElkMean$snow)
abline(regressionElk,col = 'coral3', lwd=2)

#Trying to add ENSO --------------------------------------------------------------------------------------------------------------------------------

#Attempt 1
#ENSOraw <- read.table('nino34.txt', header = F) #NOAA from <- https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Nino34/
#colnames(ENSOraw) <- tolower(c('year',month.abb))

#ENSO <- cbind(ENSOraw$oct,ENSOraw$nov,ENSOraw$dec)
#row.names(ENSO) <- 1870:2019

#ENSOmean <-  rowMeans(ENSO)
#ENSOmean <- round(ENSOmean,digits=2)

#ENSO <- cbind(ENSO, ENSOmean)
#colnames(ENSO) <- c('oct','nov','dec','mean')

#ENSO <- (ENSO[-(1:114),])
#ENSO <- as.data.frame(ENSO)

#pairs(yearMeanEnso)
#ENSOcor <- cor(yearMeanEnso)

#ENSOregression <- lm(yearMeanEnso$snow~yearMeanEnso$maxTemp+yearMeanEnso$`ENSO OND mean`)
#summary(ENSOregression)
#coeffsENSO <- ENSOregression$coefficients

#Attempt 2
#ENSO2raw <- read.csv('ENSO1JF.csv', header=FALSE) #https://www.esrl.noaa.gov/psd/enso/mei/

#ENSO2 <- (ENSO2raw[-(1:5),])
#ENSO2 <- as.data.frame(ENSO2)
#colnames(ENSO2) <- c('year','ENSOindex')

#yearMeanEnso['2019',5] <- 0
#yearMeanEnso <- round(yearMeanEnso,digits=2)
#yearMeanEnso <- cbind(yearElkMean,ENSO2$ENSOindex)
#colnames(yearMeanEnso) <- c('year','minTemp','maxTemp','averageT','snow','ENSOindex')
#pairs(yearMeanEnso)
#ENSOcor <- cor(yearMeanEnso)

#ENSOregression2 <- lm(yearMeanEnso$snow~yearMeanEnso$maxTemp+yearMeanEnso$ENSOindex)
#summary(ENSOregression2)
#coeffsENSO2 <- ENSOregression2$coefficients 
#coeffsENSO2

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

NAOtemp <- cor(NAOmean,yearElkMean$maxTemp)
NAOtemp

NAOsnow <- cor(NAOmean,yearElkMean$snow)
NAOsnow


#Calculating skiers per state ----------------------------------------------------------------------------------------------------------

skiersRaw <- read.csv('visitsnortheast.csv', header = T)
pennShare <- 0.158

pennSkiers <- ((skiersRaw$northeast*pennShare)/27)*1000000

ElkskiersCor <- cor(pennSkiers,yearElkMean$snow)
ElkskiersCor

ElkskiersRegression <- lm(pennSkiers~yearElkMean$snow+yearElkMean$maxTemp)
summary(ElkskiersRegression)

ElktempSkier <- cor(pennSkiers,yearElkMean$maxTemp)
ElktempSkier

ElkskiersTmax <- lm(pennSkiers~yearElkMean$maxTemp)
summary(ElkskiersTmax)

ElkmaxTmean <- mean(yearElkMean$maxTemp)
ElkmaxTmean

#Detrend -------------------------------------------------------------------------------------------------------------------------------

library(pracma)
#detrendMaxTemp <-  detrend(yearElkMean$maxTemp)
#detrendMaxTemp

detrendElkSkiers <- detrend(pennSkiers)
detrendElkSkiers

#detrendBoth <- lm(detrendSkiers~detrendMaxTemp)
#summary(detrendBoth)


cor(yearElkMean$maxTemp, detrendElkSkiers, method = 'pearson')

detrendRegressionElk <- lm(detrendElkSkiers~yearElkMean$maxTemp)
summary(detrendRegressionElk)

coeffsDetrendElk <- detrendRegressionElk$coefficients
coeffsDetrendElk

#Confidence interval ------------------------------------------------------------------------------------------------------------------


ElksY <- sd(pennSkiers)  

ElksY

ElksX <- sd(yearElkMean$maxTemp)

ElkxBar <- mean(yearElkMean$maxTemp)

ElkxO <- mean(yearElkMean$maxTemp)

ElkxO


Elkx1 <- mean(yearElkMean$maxTemp)+1.08

Elkx1


Elkx2 <- mean(yearElkMean$maxTemp)+1.98

Elkx2


Elkr2 <- (ElktempSkier)^2

Elkr2


ElkseRegressionX0 <- ElksY*(sqrt(1-Elkr2))*(sqrt((n-1)/(n-2)))*(sqrt(1+(1/n)+((ElkxO-ElkxBar)^2/((n-1)*((ElksX)^2)))))

ElkseRegressionX0

ElkconfidenceInterval95X0 <- ElkseRegressionX0*2



ElkseRegressionX1 <- ElksY*(sqrt(1-Elkr2))*(sqrt((n-1)/(n-2)))*(sqrt(1+(1/n)+((Elkx1-ElkxBar)^2/((n-1)*((ElksX)^2)))))

ElkseRegressionX1


ElkseRegressionX2 <- ElksY*(sqrt(1-Elkr2))*(sqrt((n-1)/(n-2)))*(sqrt(1+(1/n)+((Elkx2-ElkxBar)^2/((n-1)*((ElksX)^2)))))

ElkseRegressionX2


