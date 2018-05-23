
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

y_pred_train = predict(m,train[,-1])
y_pred_test = predict(m,test[,-1])

y_train = log(train$SalePrice)
y_test = log(test$SalePrice)

#################################
#14 -	Improve model
#################################
library(boot)
library(MASS)

m<- lm(log(SalePrice)~.+I(Gr.Liv.Area^5),data=train)

# leave-one-out and 10-fold cross-validation prediction error 
glm <- glm(log(SalePrice)~.+I(Gr.Liv.Area^5),data=train)
(cv.err <- cv.glm(train, glm)$delta)
(cv.err.6 <- cv.glm(train, glm, K = 10)$delta)

# As this is a linear model we could calculate the leave-one-out 
# cross-validation estimate without any extra model-fitting.
muhat <- fitted(glm)
diag <- glm.diag(glm)
(cv.err <- mean((glm$y - muhat)^2/(1 - diag$h)^2))

# check which polynominal degree leads to min cv.error
#leave one out
require(boot)

glm.fit=glm(log(SalePrice)~.+I(Gr.Liv.Area^5), data=train )

#LOOCV: Leave-one-out cross validation 
cv.glm(train,glm.fit)$delta 
fit <- glm.fit
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
  
}

# A vector for collecting the errors.
cv.error=vector(mode="numeric",length=5)

# The polynomial degree
degree=1:10

# A fit for each degree
for(d in degree){
  glm.fit=glm(log(SalePrice)~.+I(Gr.Liv.Area^5), data=train)
  cv.error[d]=loocv(glm.fit)
}

# The plot of the errors
plot(degree,cv.error,type="b")

# 10-fold CV
# A vector for collecting the errors.
cv.error10=rep(0,5)

# The polynomial degree
degree=1:10

# A fit for each degree
for(d in degree){
  glm.fit=glm(log(SalePrice)~.+I(poly(Gr.Liv.Area,d)), data=train)
  cv.error10[d]=cv.glm(train,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")

#################################
#15 -	Test model
#################################

#10 fold cross validatiton use dataset as both training and test set
library(faraway)
library(DAAG)
model3.daag<- CVlm(train, m=10,form.lm=formula(log(SalePrice)~.+I(Gr.Liv.Area^5), data=train))
summary(model3.daag)

#10 fold cross validatiton use dataset 22 training set and test dataset as test set
#predict the outcome of the testing data
predicted <- predict(glm.fit, newdata=test[ ,-1])

#apply model
#fit the model
model <- lm(log(SalePrice)~.+I(Gr.Liv.Area^5),data=train)
print(model)

# the proportion variation explained in the outcome of the testing data
actual <- log(test$SalePrice)
predicted <- unname(predicted)
rsq <- 1-sum((actual-predicted)^2)/sum((actual-mean(actual))^2)

#plot actual vs predicted
par(mfrow=c(2,1))
plot(actual,type="l", main="Actual")
plot(predicted, type="l", main = "Predictions")
par(mfrow=c(1,1))
plot(predicted,type="l", col=2, main = "Actual (blue) vs Predictions (red)")
lines(actual,col="blue")
max(abs(predicted-actual))

