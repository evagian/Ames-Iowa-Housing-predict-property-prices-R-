data2 <- read.csv("data/highly-correlated.csv", dec=",", stringsAsFactors=FALSE)

#################################
#6 -	Lasso
#################################

#lasso - reduce number of attributes
library(magrittr)
library(dplyr)
library(psych)
data2 <- droplevels(data2)
data2 <- subset(data2, select=-c(X, PriceTotalSQRM, PriceLivSQRM))
names(data2)
#all attributes should be numeric
data2[, 1:57] <- sapply(data2[, 1:57], as.numeric)

sapply(data2,class)
tolasso <- data2

#Lars lasso
mfull <- lm(SalePrice~.,data=tolasso)
library(lars)
X<-model.matrix(mfull)[,-1]
lasso1 <- lars( X, tolasso$SalePrice ) 
plot(lasso1, xvar='n')
plot(lasso1, xvar='n', breaks=F)
plot(lasso1, xvar='n', breaks=F, xlim=c(0.5,1), ylim=c(-20,15) )
plot(lasso1, xvar='df')
plot(lasso1, xvar='arc')
plot(lasso1, xvar='step')
res.cv <- cv.lars( X, tolasso$SalePrice ) # default model='fraction'
lambda<-res.cv$index
cv    <-res.cv$cv
mincv.s <- lambda[cv==min(cv)]
coef( lasso1, s=mincv.s, mode='fraction' )

rescp<-summary(lasso1)
coef(lasso1, s=which.min(rescp$Cp), mode="step")
plot(lasso1, xvar='n', plottype='Cp')
plot(lasso1, xvar='n', plottype='Cp', ylim=c(12,20), xlim=c(0.7,1))

# finding the s corresponding to the optimal Cp
blasso <- coef(lasso1, s=which.min(rescp$Cp), mode="step")
bols   <- coef(mfull)[-1] 

# use std coef
zblasso <- coef(lasso1, s=which.min(rescp$Cp), mode="step") * apply(X,2,sd)
zbols   <- coef(mfull)[-1]  * apply(X,2,sd)
s <- sum( abs( zblasso ) )/sum( abs( zbols ) )
s

#Glmnet Lasso
library(glmnet)
lasso2 = glmnet(X, tolasso$SalePrice  ) 
plot(lasso2, label=T)

plot(lasso2, xvar='lambda', label=T)

# 10-fold CV
lasso3 <- cv.glmnet(X, tolasso$SalePrice )
lasso3$lambda.min
lasso3$lambda.1se
plot(lasso3)

#Use lamda.1se
blasso3<- coef(lasso3) # blasso3<- coef(lasso3,  s = "lambda.1se")
blasso3
zblasso <- blasso3[-1] * apply(X,2,sd)
zbols   <- coef(mfull)[-1]  * apply(X,2,sd)
s <- sum( abs( zblasso ) )/sum( abs( zbols ) )
s

# lasso selected attributes
# 57 x 1 sparse Matrix of class "dgCMatrix"
# 1
# (Intercept)            -2.938969e+05
# Lot.Area                2.963945e-02
# Overall.Qual            1.688471e+04
# Year.Built              3.950254e+01
# Year.Remod.Add          9.980089e+01
# BsmtFin.SF.1            2.191611e+00
# Gr.Liv.Area             1.798261e+01
# Garage.Cars             1.195652e+04
# TotalArea               1.722299e+01
# HasFireplace            5.635439e+03
# MS.Zoning_RM           -3.594993e+03
# Neighborhood_StoneBr    1.373402e+03
# Neighborhood_NridgHt    4.936820e+03
# Neighborhood_NoRidge    9.156319e+03
# Exter.Qual_TA          -5.106764e+03
# Bsmt.Qual_Ex            3.018510e+04
# Bsmt.Exposure_Gd        6.434554e+03
# BsmtFin.Type.1_GLQ      2.901241e+03

data2 <- subset(data2, select=c(SalePrice, Lot.Area, Overall.Qual,       
Year.Built,    Year.Remod.Add,   BsmtFin.SF.1,         
Gr.Liv.Area  ,        Garage.Cars  ,     TotalArea  ,        
HasFireplace    ,      MS.Zoning_RM    ,       Neighborhood_StoneBr,     
Neighborhood_NridgHt,    Neighborhood_NoRidge ,  
Exter.Qual_TA   ,       Bsmt.Qual_Ex   ,     Bsmt.Exposure_Gd,      
BsmtFin.Type.1_GLQ  , Kitchen.Qual_Ex,  Kitchen.Qual_TA))

write.csv(data2, "data/lasso-selected-attributes.csv")
