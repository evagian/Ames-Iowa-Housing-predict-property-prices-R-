
#################################
#10 -	Train regression model without outliers
#################################
Mtotal <- read.csv("data/data-without-outliers.csv", dec=",", stringsAsFactors=FALSE)
Mtotal <- subset(Mtotal, select=-c(X,X.1))

## 90% of the sample size
smp_size <- floor(0.90 * nrow(Mtotal))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(Mtotal)), size = smp_size)

train <- Mtotal[train_ind, ]
test <- Mtotal[-train_ind, ]

# Stepwise regression
m<- lm(log(SalePrice)~.+I(Gr.Liv.Area^2),data=train)
step(m, direction='both')
step(m, direction='back')
m<-lm(log(SalePrice)~.+I(Gr.Liv.Area^2),data=train)
mnull<-lm(log(SalePrice)~.+I(Gr.Liv.Area^2),data=train)
step(mnull, scope=list(lower=mnull,upper=m), direction='both' )
summary(m)
anova(m)
plot(m)

#################################
#11 - Plot predicted vs actual & residuals
#################################
names(train[,-1])
y_pred_train = predict(m,train[,-1])
y_pred_test = predict(m,test[,-1])



y_train = log(train$SalePrice)
y_test = log(test$SalePrice)

# plot predictions on training data
mse_lasso = sum((y_pred_train-y_train)^2)/length(y_train)
plot(y_train,y_pred_train,xlab="True log( price )",ylab="Predicted log( price )",
     main="Figure 1. Prediction on Train Data using Stepwise regression")
mtext("add +I(Gr.Liv.Area^2)", 3, line=0.5,cex=1.2)
text(-1,4,substitute(r^2 == r2,list(r2=cor(y_train,y_pred_train))),adj=0)
text(-1,3.7,substitute(MSE == r2,list(r2=mse_lasso)),adj=0)
abline(0,1)
eruption.res = resid(m)

# plot predictions on test data
mse_lasso = sum((y_pred_test-y_test)^2)/length(y_test)
plot(y_test,y_pred_test,xlab="True log( price )",ylab="Predicted log( price )",
     main="Figure 1. Prediction on test Data using Stepwise regression")
mtext("add +I(Gr.Liv.Area^2)", 3, line=0.5,cex=1.2)
text(-1,4,substitute(r^2 == r2,list(r2=cor(y_test,y_pred_test))),adj=0)
text(-1,3.7,substitute(MSE == r2,list(r2=mse_lasso)),adj=0)
abline(0,1)
eruption.res = resid(m)

#A density plot
plot(density(resid(m))) 
qqnorm(resid(m)) 

# A quantile normal plot - good for checking normality
qqline(resid(m))
dev.off()
residualPlot(m, type='rstudent')
residualPlots(m, plot=F)

# plot studentized residuals vs. fitted values 
spreadLevelPlot(m)


#################################
#12 -	Plot model results
#################################

# added variable plots 
av.plots(m)

# Normality of Residuals
# qq plot for studentized resid
qqPlot(m, main="QQ Plot")

# distribution of studentized residuals
library(MASS)
sresid <- studres(m) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

# visualize variables distribution
library(vioplot)

Mtotal[, 1:20] <- sapply(Mtotal[, 1:20], as.numeric)

Mtotalnums <- Mtotal[, 1:9]
eda.plots <- function(Mtotalnums, ask=F){
  graphics.off()
  numeric.only <- sapply(Mtotalnums,class)=='numeric'
  y <- Mtotalnums[,numeric.only]
  n<-ncol(y) 
  for (i in 1:n){
    if (!ask) win.graph()
    par(mfrow=c(2,2), ask=ask)
    
    y1 <- y[,i]
    vioplot(y1)
    hist(y1, probability=TRUE, main=names(y)[i])
    lines(density(y1), col=2)
    qqnorm(y1, main=names(y)[i])
    qqline(y1)
    boxplot(y1, main=names(y)[i], horizontal=TRUE)}}
eda.plots(Mtotalnums)

# plot correlation between attributes
library(corrplot)
corrplot((cor(Mtotal)), method="square")
plotCorr <- function(Mtotal, i){
  data <- data.frame(x = Mtotal[[i]], SalePrice = Mtotal$SalePrice)
  p <- ggplot(data, aes(x = x, y = SalePrice)) + geom_point(shape = 1, na.rm = TRUE) + geom_smooth(method = lm ) + xlab(paste0(colnames(Mtotal)[i], '\n', 'R-Squared: ', round(cor(Mtotal[[i]], data$SalePrice, use = 'complete.obs'), 2))) + theme_light()
  return(suppressWarnings(p))
}
doPlots(data_corr, fun = plotCorr, ii = 1:6)


#################################
#13 -	Evaluate regression model assumptions
#################################

# Evaluate homoscedasticity
# non-constant error variance test
lmtest::bptest(m) 
ncvTest(m)

# Evaluate Collinearity
vif(m) # variance inflation factors 
sqrt(vif(m)) > 2 # problem?

#fix collinearity
library(gvlma)
gvmodel2 <- gvlma(m) 
summary(gvmodel2)

# Evaluate Nonlinearity
# component + residual plot 
crPlots(m)

# Test for Autocorrelated Errors
library(lmtest);dwtest(m) # Global test of model assumptions

library(car); durbinWatsonTest(m)

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(m) 
summary(gvmodel)

# plot and evaluate residuals
eruption.res = resid(Mtotal)
plot(Mtotal$SalePrice, eruption.res, 
     ylab="Residuals", xlab="Sale Price", 
     main="Residual Plot") 
abline(0, 0)

#A density plot
plot(density(resid(m))) 

# A quantile normal plot - good for checking normality
qqnorm(resid(m)) 
qqline(resid(m))
dev.off()
residualPlot(m, type='rstudent')

#studentised residuals
Stud.residuals<-rstudent(m)
yhat <- fitted(m)
par(mfrow=c(1,2))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)
plot(yhat, Stud.residuals^2)
abline(h=4, col=2, lty=2)

#Non-constant Variance Score Test 
#Variance formula: ~ fitted.values
library(car)
ncvTest(m)

#Levene's Test for Homogeneity of Variance (center = median)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)

leveneTest(rstudent(m)~yhat.quantiles)

# boxplot
boxplot(rstudent(m)~yhat.quantiles)

# check independence
plot(rstudent(m), type='l')
library(randtests); runs.test(m$res)



