library(wavethresh)
library(caret)
datasicaklikwd4 = wd(datasicaklik, filter.number = 4, family = "DaubExPhase")
dataruzgarhwd4 = wd(dataruzgarh, filter.number = 4, family= "DaubExPhase")
datanemwd4 = wd(datanem, filter.number = 4, family = "DaubExPhase")
databasincwd4 = wd(databasinc,filter.number = 4, family = "DaubExPhase")

#
Finecoef4=accessD(datasicaklikwd4,level = 8)
sigma=mad(Finecoef4)

uthreshold4= sigma*sqrt(2*log(512))

sicaklikwdT4 = threshold(datasicaklikwd4, policy = "manual",value = uthreshold4, type = "soft")
sicaklikwr4 =wr(sicaklikwdT4)
plot(sicaklikwr4,type = "l", xlab = "Days", las=1, ylab = "Temperature Soft-Universal Th.")
#
Finecoefruzgar=accessD(dataruzgarhwd4,level = 8)
sigma=mad(Finecoefruzgar)

uthresholdr= sigma*sqrt(2*log(512))

ruzgarhwdT4 = threshold(dataruzgarhwd4, policy = "manual",value = uthresholdr)
ruzgarhwr4 =wr(ruzgarhwdT4)
plot(ruzgarhwr4,type = "l", xlab = "Days", las=1, ylab = "Wind Speed Hard- Universal Th.")
#
Finecoefnem=accessD(datanemwd4,level = 8)
sigmanem=mad(Finecoefnem)

uthresholdnem= sigmanem*sqrt(2*log(512))

nemwdT4 = threshold(datanemwd4, policy = "manual",value = uthresholdnem)
nemwr4 = wr(nemwdT4)
plot(nemwr4,type = "l", xlab = "Days", las=1, ylab = "Humidity Hard-Universal Th.")
#
Finecoefbasinc=accessD(databasincwd4,level = 8)
sigmab=mad(Finecoefbasinc)

uthresholdb= sigmab*sqrt(2*log(512))

basincwdT4 = threshold(databasincwd4, policy = "manual",value = uthresholdb)
basincwr4 =wr(basincwdT4)
plot(basincwr4,type = "l", xlab = "Days", las=1, ylab = "Pressure Hard-Universal Th.")
###############################################################################################
sicaklikwdcvT4= threshold(datasicaklikwd4, policy = "cv", dev= madmad,type="soft")
sicaklikcvwr4= wr(sicaklikwdcvT4)
plot(sicaklikcvwr4, type= "l", xlab = "Days", ylab = "Temperature Soft-CV Thresholding.")   

ruzgarhwdcvT4= threshold(dataruzgarhwd4, policy = "cv", dev= madmad,type="hard")
ruzgarhcvwr4= wr(ruzgarhwdcvT4)
plot(ruzgarhcvwr4, type= "l", xlab = "Days", ylab = "Wind Speed Hard-CV Thresholding") 

nemwdcvT4= threshold(datanemwd4, policy = "cv", dev= madmad,type="hard")
nemcvwr4= wr(nemwdcvT4)
plot(nemcvwr4, type= "l", xlab = "Days", ylab = "Humidity Hard-CV Thresholding") 

basincwdcvT4= threshold(databasincwd4, policy = "cv", dev= madmad,type="hard")
basinccvwr4= wr(basincwdcvT4)
plot(basinccvwr4, type= "l", xlab = "Days", ylab = "Pressure Hard-CV Thresholding") 


##############################################################################################
datawrcv4=data.frame(sicaklikcvwr4,ruzgarhcvwr4,nemcvwr4,basinccvwr4)

datawr4 = data.frame(sicaklikwr4,ruzgarhwr4, nemwr4,basincwr4)


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
  lm.fit2=lm(sicaklikwr4~ruzgarhwr4 + nemwr4 + basincwr4,data=datawr,subset=train) 
  # Compute the test error
  TestError2[i]= mean((datawr4$sicaklikwr4 -predict(lm.fit2,datawr4) )[-train ]^2)
  # Fit a third model cv-thresholed wr data 
  lm.fit3=lm(sicaklikcvwr4~ruzgarhcvwr4 + nemcvwr4 + basinccvwr4, data=datawrcv4, subset=train)
  # Compute the test error
  TestError3[i]= mean((datawrcv4$sicaklikcvwr4-predict(lm.fit3,datawrcv4) )[-train ]^2)
}
TestError= cbind.data.frame(TestError1,TestError2,TestError3)
boxplot(TestError)

summary(lm.fit2)
AIC(lm.fit2)

summary(lm.fit3)
AIC(lm.fit3)

#######dalgacik thresholding##############################################################################################
regressorwr4=lm(formula = sicaklikwr4 ~ ruzgarhwr4 + nemwr4 + basincwr4, data = datawr4)
summary(regressorwr4)
##### enson tr makale icin düzenlenen kisim#####
library("readr")
write_csv(datawr4, path = "datawr4.csv")
write_tsv(datawr4, path = "datawr4.txt")


model1=anova(regressorwr4)
anova1= aov(formula = sicaklikwr4 ~ ruzgarhwr4 + nemwr4 + basincwr4, data = datawr4)

oneway.test(sicaklikwr4 ~ ruzgarhwr4 + nemwr4 + basincwr4, data = datawr4)
anova1
summary(anova1)
AIC(regressorwr4)

RMSE <- function(error) { sqrt(mean(error^2)) }
RMSE(regressorwr4$residuals)
###############################################

set.seed(123)
partitionwr <- createDataPartition(y = datawr4$sicaklikwr4, p = 0.8, list = F)
trainingdatawr4 = datawr4[partitionwr, ]
testdatawr4 <- datawr4[-partitionwr, ]

ptest <- predict(regressorwr4, testdatawr4)
error <- (ptest - testdatawr$sicaklikwr)
RMSE_NewDatawr <- sqrt(mean(error2^2))

Method <- c("Train/Test Split")
RMSENewData2 <- c(RMSE_NewDatawr)

table2 <- data.frame(Method, RMSENewData2)

modelcv2 <- train(
  sicaklikwr4~ ruzgarhwr4 + nemwr4 + basincwr4, data=datawr4,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10
  )
)
modelcv2
summary(modelcv2)

RMSE_Modelcv2 <- modelcv2$results$RMSE

pcv2 <- predict(modelcv2, testdatawr4)
errorcv2 <- (pcv2- testdatawr4$sicaklikwr4)
RMSE_NewDatacv4 <- sqrt(mean(errorcv2^2))
RMSE_NewDatacv4


############# cvthresholding cv k-fold #######################################################################################
regressorcv4 = lm(formula = sicaklikcvwr4 ~ nemcvwr4 + basinccvwr4 + ruzgarhcvwr4, data= datawrcv4)
summary(regressorcv4)

set.seed(42)
partitionwrcv <- createDataPartition(y = datawrcv4$sicaklikcvwr4, p = 0.8, list = F)
trainingdatawrcv4 = datawrcv4[partitionwrcv, ]
testdatawrcv4 <- datawrcv4[-partitionwrcv, ]

ptest4 <- predict(regressorcv4, testdatawrcv4)
error3wrcv <- (ptest4 - testdatawrcv4$sicaklikcvwr4)
RMSE_NewDatawrcv4 <- sqrt(mean(error3wrcv^2))

Method <- c("Train/Test Split")
RMSENewData3<- c(RMSE_NewDatawrcv)

table3 <- data.frame(Method, RMSENewDatawrcv)

modelcv3 <- train(
  sicaklikcvwr4~ ruzgarhcvwr4 + nemcvwr4 + basinccvwr4, data=datawrcv4,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10
  )
)
print(modelcv3)
summary(modelcv3)


RMSE_Modelcv3 <- modelcv3$results$RMSE

pcv4 <- predict(modelcv3, testdatawrcv4)
errorcv3 <- (pcv4- testdatawrcv4$sicaklikcvwr4)
RMSE_NewDatacv3 <- sqrt(mean(errorcv3^2))
RMSE_NewDatacv3

p3 = predict(regressorcv)
error3 <- (p3 - datawrcv$sicaklikcvwr)
rmse3 = sqrt(mean(error3^2))
rmse3




