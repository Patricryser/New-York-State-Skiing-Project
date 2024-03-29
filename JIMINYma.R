#Setting working directory -----------------------------------------------------------------------------------------

setwd("~/Columbia/Climate and Society/Quant/Final Project/Data")
setwd("C:/Users/Patri/polybox/Columbia Academics/Quant/Project/DataV1")

#upploaing data ALBANY FOR JIMINY PEAK MOUNTAIN MA----------------------------------------------------------------------------------------------------
JiminyRaw <- read.csv('Albany_NY_for_JiminyPeakMA.csv')

#Creating a date format --------------------------------------------------------------------------------------------
date <- as.Date(JiminyRaw$DATE, format='%m/%d/%Y')
Jiminy <- cbind(JiminyRaw,date)


#Finding winter
months <- as.numeric(format(Jiminy$date,'%m'))
year <- as.numeric(format(Jiminy$date,'%Y'))
Jiminy <- cbind(Jiminy,months,year)

decIndex <- which(Jiminy$months == 12)
janIndex <- which(Jiminy$months == 1)
febIndex <- which(Jiminy$months == 2)
marIndex <- which(Jiminy$months == 3)


winterMAIndex <- c(decIndex,janIndex,febIndex,marIndex)

winterJiminyRaw <- Jiminy[winterMAIndex,]
snow <- winterJiminyRaw$SNOW
snow[NA] <- 0 #Getting rid of NA in snow
snow

winterJiminyData <- as.data.frame(cbind(as.POSIXct(winterJiminyRaw$DATE, format = "%m/%d/%Y %I:%M:%S %p" , tz = "GMT"),winterJiminyRaw$year,winterJiminyRaw$months,winterJiminyRaw$TMAX,winterJiminyRaw$TMIN,winterJiminyRaw$SNOW))
colnames(winterJiminyData) <- c('date','year','month','maxTemp','minTemp','snow')

#winterJiminyData <- winterJiminyData[do.call(order, winterJiminyData), ] 
#obs <- nrow(winterJiminyData)
#obs #4365
#row.names(winterJiminyData) <- 1:obs
#winterJiminyData <- winterJiminyData[-(4366:13122),]
#nrow(winterJiminyData)

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
yearJiminyMean <- matrix(NA,nrow=n,ncol=4)
row.names(yearJiminyMean) <- 1984:2019
colnames(yearJiminyMean) <- c('minTemp','maxTemp','averageT','snow')

#Setting up the loop 
for (increment in 1:n){
  yearJiminyData <- 1983 + increment
  jan <- which(winterJiminyData$year == yearJiminyData & winterJiminyData$month == 1) 
  feb <- which(winterJiminyData$year == yearJiminyData & winterJiminyData$month == 2) 
  mar <- which(winterJiminyData$year == yearJiminyData & winterJiminyData$month == 3) 
  dec <- which(winterJiminyData$year == yearJiminyData-1 & winterJiminyData$month ==12) 
  
  yearlyJiminyIndex <- c(dec,jan,feb,mar)
  yearlyJiminyIndex
  
  yearJiminyMean[increment,1] <- mean(winterJiminyData$minTemp[yearlyJiminyIndex])
  yearJiminyMean[increment,2] <- mean(winterJiminyData$maxTemp[yearlyJiminyIndex])
  yearJiminyMean[increment,3] <- mean(c((winterJiminyData$maxTemp[yearlyJiminyIndex]),(winterJiminyData$minTemp[yearlyJiminyIndex])))
  yearJiminyMean[increment,4] <- mean(winterJiminyData$snow[yearlyJiminyIndex], na.rm=TRUE)
  
}

yearJiminyMean <- as.data.frame(yearJiminyMean)
years <- 1984:2019
yearJiminyMean <- cbind(years,yearJiminyMean)
length(yearJiminyMean$maxTemp)


plot(years,yearJiminyMean$maxTemp, type = 'l')

plot(years,yearJiminyMean$minTemp, type = 'l')

plot(years,yearJiminyMean$averageT, type = 'l')

plot(years,yearJiminyMean$snow, type = 'l')

#Calculating the correlation and the regression ---------------------------------------------------------------------------------------------------

pairs(yearJiminyMean, col='lightblue3', pch = 19)

corJiminyWinter <- cor(yearJiminyMean) #correlation values for our variables
corJiminyWinter

regressionJiminy <- lm(yearJiminyMean$snow~yearJiminyMean$maxTemp)
summary(regressionJiminy)
coeffsJiminy <- regressionJiminy$coefficients

plot(yearJiminyMean$maxTemp,yearJiminyMean$snow)
abline(regressionJiminy,col = 'coral3', lwd=2)

#Trying to add ENSO --------------------------------------------------------------------------------------------------------------------------------

#Attempt 1
#ENSOraw <- read.table('ENSO1JF.csv', header = F) #NOAA from <- https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Nino34/
#colnames(ENSOraw) <- tolower(c('year',month.abb))

#ENSO <- cbind(ENSOraw$oct,ENSOraw$nov,ENSOraw$dec)
#row.names(ENSO) <- 1870:2019

#ENSOmean <-  rowMeans(ENSO)
#ENSOmean <- round(ENSOmean,digits=2)

#ENSO <- cbind(ENSO, ENSOmean)
#colnames(ENSO) <- c('oct','nov','dec','mean')

#ENSO <- (ENSO[-(1:114),])
#ENSO <- as.data.frame(ENSO)

#yearMeanEnso <- cbind(yearMean,ENSO$mean)
#colnames(yearMeanEnso) <- c('minTemp','maxTemp','averageT','snow','ENSO OND mean')

#yearMeanEnso['2019',5] <- 0
#yearMeanEnso <- round(yearMeanEnso,digits=2)

#pairs(yearMeanEnso)
#ENSOcor <- cor(yearMeanEnso)

#ENSOregression <- lm(yearMeanEnso$snow~yearMeanEnso$maxTemp+yearMeanEnso$`ENSO OND mean`)
#summary(ENSOregression)
#coeffsENSO <- ENSOregression$coefficients

#Attempt 2 ------------------------------------
#ENSO2raw <- read.csv('ENSO1JF.csv', header=FALSE) #https://www.esrl.noaa.gov/psd/enso/mei/

#ENSO2 <- (ENSO2raw[-(1:5),])
#ENSO2 <- as.data.frame(ENSO2)
#colnames(ENSO2) <- c('year','ENSOindex')

#yearJiminyMean <- cbind(yearJiminyMean,ENSO2$ENSOindex)
#yearJiminyMean <- round(yearJiminyMean,digits=2)
#colnames(yearJiminyMean) <- c('year','minTemp','maxTemp','averageT','snow','ENSOindex')
#pairs(yearJiminyMean)
#ENSOcor <- cor(yearJiminyMean)
#ENSOcor

#ENSOregression2 <- lm(yearJiminyMean$snow~yearJiminyMean$maxTemp+yearJiminyMean$ENSOindex)
#summary(ENSOregression2)
#coeffsENSO2 <- ENSOregression2$coefficients 

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

NAOtemp <- cor(NAOmean,yearJiminyMean$maxTemp)
NAOtemp

NAOsnow <- cor(NAOmean,yearJiminyMean$snow)
NAOsnow


#Calculating skiers per state ----------------------------------------------------------------------------------------------------------

skiersRaw <- read.csv('visitsnortheast.csv', header = T)
MaShare <- 0.077

maSkiers <- ((skiersRaw$northeast*MaShare)/13)*1000000

maskiersCor <- cor(maSkiers,yearJiminyMean$snow)
maskiersCor

maskiersRegression <- lm(maSkiers~yearJiminyMean$snow+yearJiminyMean$maxTemp)
summary(maskiersRegression)

matempSkier <- cor(maSkiers,yearJiminyMean$maxTemp)
matempSkier

maskiersTmax <- lm(maSkiers~yearJiminyMean$maxTemp)
summary(maskiersTmax)

mamaxTmean <- mean(yearJiminyMean$maxTemp)
mamaxTmean

#Detrend -------------------------------------------------------------------------------------------------------------------------------

library(pracma)
#detrendMaxTemp <-  detrend(yearJiminyMean$maxTemp)
#detrendMaxTemp

detrendmaSkiers <- detrend(maSkiers)
detrendmaSkiers

#detrendBoth <- lm(detrendSkiers~detrendmaMaxTemp)
#summary(detrendBoth)


cor(yearJiminyMean$maxTemp, detrendmaSkiers, method = 'pearson')

detrendRegressionma <- lm(detrendmaSkiers~yearJiminyMean$maxTemp)
summary(detrendRegressionma)

coeffsDetrendma <- detrendRegressionma$coefficients
coeffsDetrendma

#Confidence interval ------------------------------------------------------------------------------------------------------------------


JiminysY <- sd(maSkiers) 

JiminysY

JiminysX <- sd(yearJiminyMean$maxTemp)

JiminyxBar <- mean(yearJiminyMean$maxTemp)

JiminyxO <- mean(yearJiminyMean$maxTemp)

JiminyxO


Jiminyx1 <- mean(yearJiminyMean$maxTemp)+1.08

Jiminyx1


Jiminyx2 <- mean(yearJiminyMean$maxTemp)+1.98

Jiminyx2


Jiminyr2 <- (matempSkier)^2

Jiminyr2


JiminyseRegressionX0 <- JiminysY*(sqrt(1-Jiminyr2))*(sqrt((n-1)/(n-2)))*(sqrt(1+(1/n)+((JiminyxO-JiminyxBar)^2/((n-1)*((JiminysX)^2)))))

JiminyseRegressionX0

JiminyconfidenceInterval95X0 <- JiminyseRegressionX0*2



JiminyseRegressionX1 <- JiminysY*(sqrt(1-Jiminyr2))*(sqrt((n-1)/(n-2)))*(sqrt(1+(1/n)+((Jiminyx1-JiminyxBar)^2/((n-1)*((JiminysX)^2)))))

JiminyseRegressionX1

JiminyseRegressionX2 <- JiminysY*(sqrt(1-Jiminyr2))*(sqrt((n-1)/(n-2)))*(sqrt(1+(1/n)+((Jiminyx2-JiminyxBar)^2/((n-1)*((JiminysX)^2)))))

JiminyseRegressionX2



