# discrete-wavelet-transform-with-R Haar soft Thresholding Transform

install.packages("rdwd")
library("wavethresh")
library(rdwd)
link <- selectDWD("Potsdam", res="daily", var="kl", per="recent")
file <- dataDWD(link, read=FALSE, dir="DWDdata", quiet=TRUE, force=NA)
clim <- readDWD(file, varnames=TRUE)

link <- selectDWD("Potsdam", res="montly", var="kl", per="recent")
file <- dataDWD(link, read=FALSE, dir="DWDdata", quiet=TRUE, force=NA)
clim <- readDWD(file, varnames=TRUE)

datasicaklik = clim[1:512,14]
dataruzgarh = clim[1:512,5]
datanem = clim[1:512,15]
databasinc = clim[1:512,13]

#Haar wavelet
datasicaklikwd1 = wd(datasicaklik, filter.number = 1, family = "DaubExPhase")
dataruzgarhwd1 = wd(dataruzgarh, filter.number = 1, family= "DaubExPhase")
datanemwd1 = wd(datanem, filter.number = 1, family = "DaubExPhase")
databasincwd1 = wd(databasinc,filter.number = 1, family = "DaubExPhase")

Finecoef1=accessD(datasicaklikwd1,level = 8)
sigma=mad(Finecoef1)

uthreshold1= sigma*sqrt(2*log(512))


sicaklikwdT1 = threshold(datasicaklikwd1, policy = "manual",value = uthreshold1)
sicaklikwdT1
sicaklikwr1 =wr(sicaklikwdT1)
plot(sicaklikwr1,type = "l", xlab = "Günler", las=1, ylab="Sıcaklık (wr)")

plot(sicaklikwdT1)


Finecoefruzgar=accessD(dataruzgarhwd1,level = 8)
sigma=mad(Finecoefruzgar)

uthresholdr= sigma*sqrt(2*log(512))

ruzgarhwdT1 = threshold(dataruzgarhwd1, policy = "manual",value = uthresholdr, type= "soft")
ruzgarhwr1 =wr(ruzgarhwdT1)
plot(ruzgarhwr1,type = "l", xlab = "Günler", las=1, ylab = "Rüzgar hızı (wr)")
#
Finecoefnem=accessD(datanemwd1,level = 8)
sigmanem=mad(Finecoefnem)

uthresholdnem= sigmanem*sqrt(2*log(512))

nemwdT1 = threshold(datanemwd1, policy = "manual",value = uthresholdnem, type= "soft")
nemwr1 = wr(nemwdT1)
plot(nemwr1,type = "l", xlab = "Günler", las=1,ylab ="Nem (wr)")
#
Finecoefbasinc=accessD(databasincwd1,level = 8)
sigmab=mad(Finecoefbasinc)

uthresholdb= sigmab*sqrt(2*log(512))

basincwdT1 = threshold(databasincwd1, policy = "manual",value = uthresholdb, type= "soft")
basincwr1 =wr(basincwdT1)
plot(basincwr1,type = "l", xlab = "Günler", las=1, ylab = "Basınç (wr)")
###############################################################################################
sicaklikwdcvT1= threshold(datasicaklikwd1, policy = "cv", dev= madmad)
sicaklikcvwr1= wr(sicaklikwdcvT1)
plot(sicaklikcvwr1, type= "l", xlab = "Günler", ylab = "Sıcaklık CV-Eşiklendirilmiş")   

ruzgarhwdcvT1= threshold(dataruzgarhwd1, policy = "cv", dev= madmad, type= "hard")
ruzgarhcvwr1= wr(ruzgarhwdcvT1)
plot(ruzgarhcvwr1, type= "l", xlab = "Günler", ylab = "Rüzgar Hızı CV-Eşiklendirilmiş") 

nemwdcvT1= threshold(datanemwd1, policy = "cv", dev= madmad,type= "hard")
nemcvwr1= wr(nemwdcvT1)
plot(nemcvwr1, type= "l",  xlab = "Günler", ylab = "Nem CV-Eşiklendirilmiş") 

basincwdcvT1= threshold(databasincwd1, policy = "cv", dev= madmad, type= "hard")
basinccvwr1= wr(basincwdcvT1)
plot(basinccvwr1, type= "l",  xlab = "Günler", ylab = "Basınç CV-Eşiklendirilmiş") 


###############################################hataların boxplotı r-kare ve AIC###############################################
datawrcv1=data.frame(sicaklikcvwr1,dataruzgarh,datanem,databasinc)

datawr1 = data.frame(sicaklikwr1,ruzgarhwr1, nemwr1,basincwr1)


set.seed(123)
train=sample(512,448)
lm.fit=lm(datasicaklik~.,data=dataorjinal, subset = train)
summary(lm.fit)

TestError1= mean((datasicaklik-predict(lm.fit,dataorjinal))[-train ]^2)

TestError1= rep(0,10)
TestError2= rep(0,10)
TestError3= rep(0,10)

for (i in 1:10){
  train=sample(512,448)
  # Fit a linear regression
  lm.fit=lm(datasicaklik ~ dataruzgarh + datanem + databasinc ,data=dataorjinal, subset =train)
  TestError1[i]= mean((dataorjinal$datasicaklik-predict(lm.fit,dataorjinal))[-train ]^2)
  # Fit a second model as wr universal thresholed data
  lm.fit2=lm(sicaklikwr1~ruzgarhwr1 + nemwr1 + basincwr1,data=datawr1,subset=train) 
  # Compute the test error
  TestError2[i]= mean((datawr1$sicaklikwr1 -predict(lm.fit2,datawr1) )[-train ]^2)
  # Fit a third model cv-thresholed wr data 
  lm.fit3=lm(sicaklikcvwr1~ruzgarhcvwr1 + nemcvwr1 + basinccvwr1, data=datawrcv1, subset=train)
  # Compute the test error
  TestError3[i]= mean((datawrcv1$sicaklikcvwr1-predict(lm.fit3,datawrcv1) )[-train ]^2)
}
TestError= cbind.data.frame(TestError1,TestError2,TestError3)
boxplot(TestError)

summary(lm.fit2)
AIC(lm.fit2)

summary(lm.fit3)
AIC(lm.fit3)

#######Wavelet thresholding##############################################################################################
regressorwr1=lm(formula = sicaklikwr1 ~ ruzgarhwr1 + nemwr1 + basincwr1, data = datawr1)
summary(regressorwr1)
AIC(regressorwr1)

RMSE <- function(error) { sqrt(mean(error^2)) }
RMSE(regressorwr1$residuals)

set.seed(123)
partitionwr <- createDataPartition(y = datawr1$sicaklikwr1, p = 0.8, list = F)
trainingdatawr1 = datawr1[partitionwr, ]
testdatawr1 <- datawr1[-partitionwr, ]

ptest <- predict(regressorwr1, testdatawr1)
error <- (ptest - testdatawr1$sicaklikwr1)
RMSE_NewDatawr <- sqrt(mean(error2^2))

Method <- c("Train/Test Split")
RMSENewData2 <- c(RMSE_NewDatawr)

table2 <- data.frame(Method, RMSENewData2)

modelcv2 <- train(
  sicaklikwr1~ dataruzgarh + datanem + databasinc, data=datawr1,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10
  )
)
modelcv2
summary(modelcv2)
RMSE_Modelcv2 <- modelcv2$results$RMSE

pcv2 <- predict(modelcv2, testdatawr1)
errorcv2 <- (pcv2- testdatawr1$sicaklikwr1)
RMSE_NewDatacv1 <- sqrt(mean(errorcv2^2))
RMSE_NewDatacv1


############# cvthresholding cv k-fold #######################################################################################
regressorcv1 = lm(formula = sicaklikcvwr1 ~ datanem + databasinc + dataruzgarh, data= datawrcv1)
summary(regressorcv1)

set.seed(123)
partitionwrcv <- createDataPartition(y = datawrcv1$sicaklikcvwr1, p = 0.8, list = F)
trainingdatawrcv1 = datawrcv1[partitionwrcv, ]
testdatawrcv1 <- datawrcv1[-partitionwrcv, ]

ptest <- predict(regressorcv1, testdatawrcv1)
error3wrcv <- (ptest - testdatawrcv1$sicaklikcvwr1)
RMSE_NewDatawrcv1 <- sqrt(mean(error3wrcv^2))

Method <- c("Train/Test Split")
RMSENewData3<- c(RMSE_NewDatawrcv)

table3 <- data.frame(Method, RMSENewDatawrcv)

modelcv3 <- train(
  sicaklikcvwr1~ dataruzgarh + datanem + databasinc, data=datawrcv1,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10
  )
)
print(modelcv3)
summary(modelcv3)
RMSE_Modelcv3 <- modelcv3$results$RMSE

pcv1 <- predict(modelcv3, testdatawrcv1)
errorcv3 <- (pcv1- testdatawrcv1$sicaklikcvwr1)
RMSE_NewDatacv3 <- sqrt(mean(errorcv3^2))
RMSE_NewDatacv3
