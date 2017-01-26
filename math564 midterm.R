#  scatter plot matrix of all the variables Y and X1 ??? X4
pairs(~Commercial_Property$Y+Commercial_Property$X1+Commercial_Property$X2+Commercial_Property$X3+Commercial_Property$X4, main='Scatterplot Matrix')
# Model 1: Y = ??0 + ??1X1 + ??2X2 + ??3X3 + ??4X4 + E
fit<-lm(Commercial_Property$Y~Commercial_Property$X1+Commercial_Property$X2+Commercial_Property$X3+Commercial_Property$X4,data=Commercial_Property)
summary(fit)

#Plot the residuals against Y
plot(Commercial_Property$Y,fit$resid,xlab = 'Y',ylab = 'Residuals',main = 'Residuals vs Y')
#against 4 predictor variables
par(mfrow=c(1,4))
plot(Commercial_Property$X1,fit$resid,xlab='x1',ylab='Residuals',main = 'Residuals vs X1')
plot(Commercial_Property$X2,fit$resid,xlab='x2',ylab='Residuals',main = 'Residuals vs X2')
plot(Commercial_Property$X3,fit$resid,xlab='x3',ylab='Residuals',main = 'Residuals vs X3')
plot(Commercial_Property$X4,fit$resid,xlab='x4',ylab='Residuals',main = 'Residuals vs X4')

#against each two-factor interaction
par(mfrow=c(1,6))
par(mfrow=c(1,6))
plot(Commercial_Property$X1*Commercial_Property$X2,fit$resid,xlab='X1*X2',ylab='Residuals',main = 'Residuals vs X1*X2')
plot(Commercial_Property$X1*Commercial_Property$X3,fit$resid,xlab='X1*X3',ylab='Residuals',main = 'Residuals vs X1*X3')
plot(Commercial_Property$X1*Commercial_Property$X4,fit$resid,xlab='X1*X4',ylab='Residuals',main = 'Residuals vs X1*X4')
plot(Commercial_Property$X2*Commercial_Property$X3,fit$resid,xlab='X2*X3',ylab='Residuals',main = 'Residuals vs X2*X3')
plot(Commercial_Property$X2*Commercial_Property$X4,fit$resid,xlab='X2*X4',ylab='Residuals',main = 'Residuals vs X2*X4')
plot(Commercial_Property$X3*Commercial_Property$X4,fit$resid,xlab='X3*X4',ylab='Residuals',main = 'Residuals vs X3*X4')

#ANOVA table of the regression
anova(fit)
#95% confidence intervals and prediction intervals
test.X1<-c(4.0,6.0,12.0)
test.X2<-c(10.0,11.5,12.5)
test.X3<-c(0.10,0,0.32)
test.X4<-c(80000,120000,340000)
test.commer= data.frame(X1=test.X1,X2=test.X2,X3=test.X3,X4=test.X4)
pred.commer<-predict(fit,newdata = test.commer)
pred.commerc<-predict(fit,newdata=test.commer, interval='confidence')
pred.coomerp<-predict(fit,newdata=test.commer, interval='prediction')

#Fit the regression model Y = ??0 + ??1X1 + ??2X2 + ??3X4 + E
fit1=lm(Commercial_Property$Y~Commercial_Property$X1+Commercial_Property$X2+Commercial_Property$X4,data=Commercial_Property)
summary(fit1)

#Use partial F test to compare Model 1 and Model 2.
anova(fit)
anova(fit1)

#Plot Y against X1
plot(Commercial_Property$X1,Commercial_Property$Y,xlab = 'X1',ylab = 'Y',main = 'X1 vs Y')

#Fit the regression model Y = ??0 + ??1X1 + ??2X2 + ??3X4 + ??4X1^2+E
fit2=lm(Commercial_Property$Y~Commercial_Property$X1+Commercial_Property$X2+Commercial_Property$X4+I(Commercial_Property$X1^2),data=Commercial_Property)
summary(fit2)
# Plot Y against the fitted Y
plot(Commercial_Property$Y,fit2$resid)
#partial F test to compare Model 2 and Model 3.

anova(fit1)
anova(fit2)