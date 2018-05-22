
#################################
#10 -	Train regression model without outliers
#################################
Mtotal <- read.csv("data/data-without-outliers.csv", dec=",", stringsAsFactors=FALSE)
Mtotal <- subset(Mtotal, select=-c(X,X.1))

## 75% of the sample size
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



