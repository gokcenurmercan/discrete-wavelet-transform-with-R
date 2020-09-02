library("wavethresh")

datasicaklikwd2 = wd(datasicaklik, filter.number = 2, family = "DaubExPhase")
dataruzgarhwd2 = wd(dataruzgarh, filter.number = 2, family= "DaubExPhase")
datanemwd2 = wd(datanem, filter.number = 2, family = "DaubExPhase")
databasincwd2 = wd(databasinc,filter.number = 2, family = "DaubExPhase")

############################################################################################

Finecoef2=accessD(datasicaklikwd2,level = 8)
sigma=mad(Finecoef2)

uthreshold2= sigma*sqrt(2*log(512))


sicaklikwdT2 = threshold(datasicaklikwd2, policy = "manual",value = uthreshold2)
#sicaklikwdT2
sicaklikwr2 =wr(sicaklikwdT2)
#plot(sicaklikwr2,type = "l", xlab = "Günler", las=1, ylab="Sicaklik (wr)")

#plot(sicaklikwdT2)

Finecoefruzgar=accessD(dataruzgarhwd2,level = 8)
sigma=mad(Finecoefruzgar)

uthresholdr= sigma*sqrt(2*log(512))

ruzgarhwdT2 = threshold(dataruzgarhwd2, policy = "manual",value = uthresholdr, type= "hard")
ruzgarhwr2 =wr(ruzgarhwdT2)
#plot(ruzgarhwr2,type = "l", xlab = "Günler", las=1, ylab = "Rüzgar hizi (wr)")

Finecoefnem=accessD(datanemwd2,level = 8)
sigmanem=mad(Finecoefnem)

uthresholdnem= sigmanem*sqrt(2*log(512))

nemwdT2 = threshold(datanemwd2, policy = "manual",value = uthresholdnem, type= "hard")
nemwr2 = wr(nemwdT2)
#plot(nemwr2,type = "l", xlab = "Günler", las=1,ylab ="Nem (wr)")

Finecoefbasinc=accessD(databasincwd2,level = 8)
sigmab=mad(Finecoefbasinc)

uthresholdb= sigmab*sqrt(2*log(512))

basincwdT2 = threshold(databasincwd2, policy = "manual",value = uthresholdb, type= "hard")
basincwr2 =wr(basincwdT2)
#plot(basincwr2,type = "l", xlab = "Günler", las=1, ylab = "Basinç (wr)")

################################################################################################

datawr2 = data.frame(sicaklikwr2,ruzgarhwr2, nemwr2,basincwr2)

regressorwr2=lm(formula = sicaklikwr2 ~ ruzgarhwr2 + nemwr2 + basincwr2, data = datawr2)
summary(regressorwr2)
AIC(regressorwr2)

RMSE <- function(error) { sqrt(mean(error^2)) }
RMSE(regressorwr2$residuals)








