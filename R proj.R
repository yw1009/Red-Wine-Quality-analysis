wquality <- read.csv(file.choose(), header= T, sep=",")

summary(wquality)
str(wquality)

######density
cor_quality_density <- cor(wquality$density,wquality$quality)
cortest1 <- cor.test(wquality$density,wquality$quality)
cortest1
plot(wquality$density,wquality$quality)

model1<-lm(wquality$quality~wquality$density)

plot(wquality$density,model1$fitted.values,type="o",xlab="density",ylab="quality",ylim=c(3,8))
par(new=TRUE)
plot(wquality$density,wquality$quality,xlab="density",ylab="quality",ylim=c(3,8))

#######pH
cor_quality_pH <- cor(wquality$pH,wquality$quality)
cortest2 <- cor.test(wquality$pH,wquality$quality)
cortest2
plot(wquality$pH,wquality$quality)
model2<-lm(wquality$quality~wquality$pH)
plot(wquality$pH,model2$fitted.values,type="o",xlab="pH",ylab="quality",ylim=c(3,8))
par(new=TRUE)
plot(wquality$pH,wquality$quality,xlab="pH",ylab="quality",ylim=c(3,8))

#######fixed.acidity
cor_quality_fixed.acidity <- cor(wquality$fixed.acidity,wquality$quality)
cortest3 <- cor.test(wquality$fixed.acidity,wquality$quality)
cortest3
plot(wquality$fixed.acidity,wquality$quality)
model3<-lm(wquality$quality~wquality$fixed.acidity)
plot(wquality$fixed.acidity,model3$fitted.values,type="o",xlab="fixed.acidity",ylab="quality",ylim=c(3,8))
par(new=TRUE)
plot(wquality$fixed.acidity,wquality$quality,xlab="fixed.acidity",ylab="quality",ylim=c(3,8))

#######volatile.acidity
cor_quality_volatile.acidity <- cor(wquality$volatile.acidity,wquality$quality)
cortest4 <- cor.test(wquality$volatile.acidity,wquality$quality)
cortest4
plot(wquality$volatile.acidity,wquality$quality)
model4<-lm(wquality$quality~wquality$volatile.acidity)
plot(wquality$volatile.acidity,model4$fitted.values,type="o",xlab="volatile.acidity",ylab="quality",ylim=c(3,8))
par(new=TRUE)
plot(wquality$fixed.acidity,wquality$quality,xlab="volatile.acidity",ylab="quality",ylim=c(3,8))

#######citric.acid
cor_quality_citric.acid <- cor(wquality$citric.acid,wquality$quality)
cortest5 <- cor.test(wquality$citric.acid,wquality$quality)
cortest5
plot(wquality$citric.acid,wquality$quality)
model5<-lm(wquality$quality~wquality$citric.acid)
plot(wquality$citric.acid,model5$fitted.values,type="o",xlab="citric.acid",ylab="quality",ylim=c(3,8))
par(new=TRUE)
plot(wquality$citric.acid,wquality$quality,xlab="citric.acid",ylab="quality",ylim=c(3,8))

#######residual.sugar
cor_quality_residual.sugar <- cor(wquality$residual.sugar,wquality$quality)
cortest6 <- cor.test(wquality$residual.sugar,wquality$quality)
cortest6
plot(wquality$residual.sugar,wquality$quality)
model6<-lm(wquality$quality~wquality$residual.sugar)
plot(wquality$residual.sugar,model6$fitted.values,type="o",xlab="residual.sugar",ylab="quality",ylim=c(3,8))
par(new=TRUE)
plot(wquality$residual.sugar,wquality$quality,xlab="residual.sugar",ylab="quality",ylim=c(3,8))

#######chlorides
cor_quality_chlorides <- cor(wquality$chlorides,wquality$quality)
cortest7 <- cor.test(wquality$chlorides,wquality$quality)
cortest7
plot(wquality$chlorides,wquality$quality)
model7<-lm(wquality$quality~wquality$chlorides)
plot(wquality$chlorides,model7$fitted.values,type="o",xlab="chlorides",ylab="quality",ylim=c(3,8))
par(new=TRUE)
plot(wquality$chlorides,wquality$quality,xlab="chlorides",ylab="quality",ylim=c(3,8))

#######free.sulfur.dioxide
cor_quality_free.sulfur.dioxide <- cor(wquality$free.sulfur.dioxide,wquality$quality)
cortest8 <- cor.test(wquality$free.sulfur.dioxide,wquality$quality)
cortest8
plot(wquality$free.sulfur.dioxide,wquality$quality)
model8<-lm(wquality$quality~wquality$free.sulfur.dioxide)
plot(wquality$free.sulfur.dioxide,model8$fitted.values,type="o",xlab="free.sulfur.dioxide",ylab="quality",ylim=c(3,8))
par(new=TRUE)
plot(wquality$free.sulfur.dioxide,wquality$quality,xlab="free.sulfur.dioxide",ylab="quality",ylim=c(3,8))

#######sulphates
cor_quality_sulphates <- cor(wquality$sulphates,wquality$quality)
cortest9 <- cor.test(wquality$sulphates,wquality$quality)
cortest9
plot(wquality$sulphates,wquality$quality)
model9<-lm(wquality$quality~wquality$sulphates)
plot(wquality$sulphates,model9$fitted.values,type="o",xlab="sulphates",ylab="quality",ylim=c(3,8))
par(new=TRUE)
plot(wquality$sulphates,wquality$quality,xlab="sulphates",ylab="quality",ylim=c(3,8))

#######alcohol
cor_quality_alcohol <- cor(wquality$alcohol,wquality$quality)
cortest10 <- cor.test(wquality$alcohol,wquality$quality)
cortest10
plot(wquality$alcohol,wquality$quality)
model10<-lm(wquality$quality~wquality$alcohol)
plot(wquality$alcohol,model10$fitted.values,type="o",xlab="alcohol",ylab="quality",ylim=c(3,8))
par(new=TRUE)
plot(wquality$alcohol,wquality$quality,xlab="alcohol",ylab="quality",ylim=c(3,8))


#######total.sulfur.dioxide
cor_quality_total.sulfur.dioxide <- cor(wquality$total.sulfur.dioxide,wquality$quality)
cortest11 <- cor.test(wquality$total.sulfur.dioxide,wquality$quality)
cortest11
plot(wquality$total.sulfur.dioxide,wquality$quality)
model11<-lm(wquality$quality~wquality$total.sulfur.dioxide)
plot(wquality$total.sulfur.dioxide,model11$fitted.values,type="o",xlab="total.sulfur.dioxide",ylab="quality",ylim=c(3,8))
par(new=TRUE)
plot(wquality$total.sulfur.dioxide,wquality$quality,xlab="total.sulfur.dioxide",ylab="quality",ylim=c(3,8))


