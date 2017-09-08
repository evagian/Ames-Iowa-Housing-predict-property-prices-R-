housing <- read.csv("data/dataset.csv", dec=",", stringsAsFactors=FALSE)

#################################
#1 - Data cleaning & null replacement
#################################

colSums(sapply(housing, is.na))
#Most of them are missing. 1389, 1493, 1209, 1452 out of 1500
housing2 <- subset(housing, select = -c( Alley, Misc.Feature, Fence, Fireplace.Qu ) )
colSums(sapply(housing2, is.na))

# There are 79 observations with GarageType = NA, 1 observation with GarageArea and GarageCars as # NA, 159 observations with GarageYrBlt, GarageFinish, GarageQual and GarageCond as NAs.
table(housing2$Garage.Area == 0 & housing2$Garage.Cars==0 & is.na(housing2$Garage.Type)) 
col.Garage <- c("Garage.Type", "Garage.Yr.Blt", "Garage.Finish", "Garage.Qual","Garage.Cond")
housing2[housing2$Garage.Area == 0 & housing2$Garage.Cars==0 & is.na(housing2$Garage.Type), col.Garage]<-apply(housing2[housing2$Garage.Area == 0 & housing2$Garage.Cars==0 & is.na(housing2$Garage.Type), col.Garage], 2, function(x) x <- rep("None", 79))
summary(housing2$Garage.Type)

# This leaves us with following observations with missing GarageYrBlt, GarageFinish, GarageQual and GarageCond
table(is.na(housing2$Garage.Yr.Blt) & is.na(housing2$Garage.Finish) & is.na(housing2$Garage.Qual) & is.na(housing2$Garage.Cond))
df <- housing2

# Predict GarageArea
library(rpart)
col.pred <- c("Garage.Type", "Garage.Yr.Blt", "Garage.Finish", "Garage.Qual", "Garage.Cond", "Year.Built",  "Garage.Cars", "Garage.Area")
area.rpart <- rpart(Garage.Area ~ .,
                    data = df[!is.na(df$Garage.Area),col.pred], method = "anova",  na.action=na.omit)
df$Garage.Area[is.na(df$Garage.Area)] <- round(predict(area.rpart, df[is.na(df$Garage.Area),col.pred]))

# Predict GarageCars
cars.rpart <- rpart(Garage.Cars ~ .,
                    data = df[!is.na(df$Garage.Cars),col.pred], method = "anova", na.action=na.omit)
df$Garage.Cars[is.na(df$Garage.Cars)] <- round(predict(cars.rpart, df[is.na(df$Garage.Cars),col.pred]))

# Predict GarageYrBlt
blt.rpart <- rpart(as.factor(Garage.Yr.Blt) ~ .,
                   data = df[!is.na(df$Garage.Yr.Blt),col.pred], method = "class", na.action=na.omit)
df$Garage.Yr.Blt[is.na(df$Garage.Yr.Blt)] <- as.numeric(as.character(predict(blt.rpart, df[is.na(df$Garage.Yr.Blt),col.pred], type = "class")))
colSums(sapply(df, is.na))

# Let's now look at the observations missing GarageFinish, GarageQual and GarageCond
df[is.na(df$Garage.Finish) & is.na(df$Garage.Qual) & is.na(df$Garage.Cond), c(col.Garage, c("Garage.Cars", "Garage.Area"))]

#Basement
col.bsmt <- c("Total.Bsmt.SF", "Bsmt.Exposure", "Bsmt.Cond", "Bsmt.Qual","Bsmt.Fin.Type.1", "Bsmt.Fin.Type.2",  "Bsmt.FinSF.1","Bsmt.Fin.SF.2", "Bsmt.Unf.SF")

# There is one row with NA for all basement data and missing TotalBsmtSF which is assumed 0.
df$Total.Bsmt.SF[is.na(df$Bsmt.Exposure) & is.na(df$Total.Bsmt.SF)] <- 0
table(is.na(df$BsmtFin.Type.1))
col.bsmt <- c("Bsmt.Exposure", "Bsmt.Cond", "Bsmt.Qual","BsmtFin.Type.1", "BsmtFin.Type.2")

# There are 36 rows with NA for basement data
df[df$Total.Bsmt.SF == 0 & is.na(df$Bsmt.Exposure), col.bsmt] <-   apply(df[df$Total.Bsmt.SF == 0 & is.na(df$Bsmt.Exposure), col.bsmt], 2, function(x) x <- rep("None", 36))


# Let us look at the rest of the missing basement data.
df[is.na(df$Bsmt.Exposure)|is.na(df$Bsmt.Cond)|is.na(df$Bsmt.Qual)|is.na(df$BsmtFin.Type.2),    c(col.bsmt, c("Total.Bsmt.SF","BsmtFin.SF.1","BsmtFin.SF.2", "Bsmt.Unf.SF"))]

# Assume that all the basement info and the year the house was built can be a good predictor of any basement info.
col.pred <- c("Bsmt.Exposure", "Bsmt.Cond", "Bsmt.Qual","BsmtFin.Type.1", "BsmtFin.Type.2","Total.Bsmt.SF","Year.Built")
BsmtFin.Type.2.rpart <- rpart(as.factor(BsmtFin.Type.2) ~ .,
                              data = df[!is.na(df$BsmtFin.Type.2),col.pred], method = "class", na.action=na.omit)                     
df$BsmtFin.Type.2[is.na(df$BsmtFin.Type.2)] <- as.character(predict(BsmtFin.Type.2.rpart,                                               
                                                                    df[is.na(df$BsmtFin.Type.2),col.pred],   type="class"))                                                                 
Bsmt.Qual.rpart <- rpart(as.factor(Bsmt.Qual) ~ .,
                         data = df[!is.na(df$Bsmt.Qual),col.pred], method = "class",  na.action=na.omit)                     
df$Bsmt.Qual[is.na(df$Bsmt.Qual)] <- as.character(predict(Bsmt.Qual.rpart,                                                         df[is.na(df$Bsmt.Qual),col.pred], type="class"))
Bsmt.Cond.rpart <- rpart(as.factor(Bsmt.Cond) ~ .,
                         data = df[!is.na(df$Bsmt.Cond),col.pred], method = "class", na.action=na.omit)                
df$Bsmt.Cond[is.na(df$Bsmt.Cond)] <- as.character(predict(Bsmt.Cond.rpart, df[is.na(df$Bsmt.Cond),col.pred], type="class"))
Bsmt.Exposure.rpart <- rpart(as.factor(Bsmt.Exposure) ~ .,
                             data = df[!is.na(df$Bsmt.Exposure),col.pred], method = "class", na.action=na.omit)
df$Bsmt.Exposure[is.na(df$Bsmt.Exposure)] <- as.character(predict(Bsmt.Exposure.rpart,                                                                                                               df[is.na(df$Bsmt.Exposure),col.pred], type="class"))                                                              

# Let us check there are any more NAs
df[is.na(df$BsmtFin.SF.1)|is.na(df$BsmtFin.SF.2)|is.na(df$Bsmt.Unf.SF), c(col.pred, c("BsmtFin.SF.1", "BsmtFin.SF.2","Bsmt.Unf.SF", "Bsmt.Full.Bath","Bsmt.Half.Bath"))]

# There is no basement here as TotalBsmtSF = 0
df$BsmtFin.SF.1[is.na(df$BsmtFin.SF.1)|is.na(df$BsmtFin.SF.2)|is.na(df$Bsmt.Unf.SF)] <- 0
df$BsmtFin.SF.2[is.na(df$BsmtFin.SF.1)|is.na(df$BsmtFin.SF.2)|is.na(df$Bsmt.Unf.SF)] <- 0
df$Bsmt.Unf.SF[is.na(df$BsmtFin.SF.1)|is.na(df$BsmtFin.SF.2)|is.na(df$Bsmt.Unf.SF)] <- 0
table(is.na(df$Bsmt.Full.Bath) & is.na(df$Bsmt.Half.Bath))
df$Bsmt.Full.Bath[df$Total.Bsmt.SF == 0 & is.na(df$Bsmt.Full.Bath)] <- rep(0,1)
df$Bsmt.Half.Bath[df$Total.Bsmt.SF == 0 & is.na(df$Bsmt.Half.Bath)] <- rep(0,1)

#MasVnrType
summary(as.factor(df$Mas.Vnr.Type))

# There are 10 NA's and 862 None MasVnrType. Let's check their corresponding area and see they are 0 or NAs
table(df$Mas.Vnr.Area[df$Mas.Vnr.Type == "None"])

# Since one square feet of MasVnrAreaseem to be very unlikely, let us change these 3 observations with MasVnrArea = 1 to MasVnrArea = 0
df$Mas.Vnr.Area <- ifelse(df$Mas.Vnr.Area == 1,0,df$Mas.Vnr.Area)

# Assign other 4 observations with areas > 0 but having Type as None to NA, which will be fixed later
df$Mas.Vnr.Type[df$Mas.Vnr.Area > 0 & df$Mas.Vnr.Type == "None" & !is.na(df$Mas.Vnr.Type)] <- rep(NA, 4)
table(is.na(df$Mas.Vnr.Type) & is.na(df$Mas.Vnr.Area))

# There are 10 NAs for both MasVnrType and MasVnrArea. Lets assign 0 to all MasVnrArea and None to all MasVnrType.
df$Mas.Vnr.Area[is.na(df$Mas.Vnr.Area)] <-rep(0, 10)
df$Mas.Vnr.Type[is.na(df$Mas.Vnr.Type) & df$Mas.Vnr.Area == 0] <- rep("None", 10)

# Lot.Frontage
table(is.na(df$Lot.Frontage))

# Likely predictors
col.pred <- c("MS.SubClass", "MS.Zoning", "Lot.Frontage", "Lot.Area", "Street", "Lot.Shape", "Land.Contour", "Lot.Config", "Land.Slope", "Bldg.Type", "House.Style", "Yr.Sold", "Sale.Type", "Sale.Condition")

# Predict LotFrontage
frntage.rpart <- rpart(Lot.Frontage ~ .,
                       data = df[!is.na(df$Lot.Frontage),col.pred], method = "anova", na.action=na.omit)

# Let us plot the existing and imputed values and check if imputed values follow the same patten
df.frontage <- as.data.frame(rbind(cbind(rep("Existing", nrow(df[!is.na(df$Lot.Frontage),])),df[!is.na(df$Lot.Frontage), "Lot.Frontage"]),
                                   cbind(rep("Imputed", nrow(df[is.na(df$Lot.Frontage),])),
                                         ceiling(predict(frntage.rpart, df[is.na(df$Lot.Frontage),col.pred])))))
library(ggplot2)
ggplot(df.frontage, aes (x = as.numeric(as.character(V2)), colour = V1)) + geom_density()+  xlab("Lot Frontage")+  theme(legend.title=element_blank())

# Imputed value seem to be fine.
df$Lot.Frontage[is.na(df$Lot.Frontage)] <- ceiling(predict(frntage.rpart, df[is.na(df$Lot.Frontage),col.pred]))

#Pool 
summary(as.factor(df$Pool.QC))
table(df$Pool.Area > 0, df$Pool.QC, useNA = "ifany")

# Reassign 'None' for 'no pool' instead of NAs
df[df$Pool.Area == 0,]$Pool.QC <- rep('None', 1493)

#new column - has pool YES=1, NO=0
#################################
#2 -	Data transformation & new columns
#################################

dataset$TotalArea <- dataset$Gr.Liv.Area + dataset$Total.Bsmt.SF
dataset$PriceTotalSQRM <- dataset$SalePrice/dataset$TotalArea
dataset$PriceLivSQRM <- dataset$SalePrice/dataset$Gr.Liv.Area
library(dplyr)
names(housing)
dataset3 <- housing
colSums(sapply(dataset3, is.na))
#Fireplaces (Discrete): Number of fireplaces transform to 0/1
dataset3$HasFireplace<-ifelse(dataset3$Fireplaces>0,1,0)
#remodeled to 0/1
dataset3$Remodeled<-ifelse(dataset3$Year.Remod.Add>dataset3$Year.Built,1,0)
#decade built
dataset3$DecadeBuilt <- (dataset3$Year.Built%/% 10) * 10
dataset4 <- dataset3
#plot all categorical variables
#the same code was used for all categorical variables
p <- ggplot(data = dataset4, aes(x = MS.Zoning))
p <- p + geom_bar(stat="count")
p
#according to ggplot we delete
dataset5 <- subset(dataset4, select = -c( Fireplaces, Garage.Area, Street,Utilities,Land.Slope,Condition.2,Roof.Matl,Heating,Central.Air,Paved.Drive) )

#################################
#3 - Visualize variables
#################################
library(data.table)
library(testthat)
library(gridExtra)
library(corrplot)
library(GGally)
library(ggplot2)
library(e1071)
library(dplyr)
train_cat <- dataset22[,.SD, .SDcols = cat_var]
train_cont <- dataset22

plotHist <- function(Mtotal, i) {
  data <- data.frame(x=Mtotal[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(Mtotal)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}
p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(Mtotal)[i]) + theme_light() + 
  doPlots <- function(Mtotal, fun, ii, ncol=3) {
    pp <- list()
    for (i in ii) {
      p <- fun(Mtotal=Mtotal, i=i)
      pp <- c(pp, list(p))
    }
    do.call("grid.arrange", c(pp, ncol=ncol))
  }
plotDen <- function(Mtotal, i){
  data <- data.frame(x=Mtotal[[i]], SalePrice = Mtotal$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(Mtotal)[i]), '\n', 'Skewness: ',round(skewness(Mtotal[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)  
}
# plot numeric variables
library(ggplot2)
library(doPlot)
doPlots(train_cont, fun = plotDen, ii = 1:6, ncol = 2)
# plot categorical variables
doPlots(Mtotal, fun = plotHist, ii = 1:6, ncol = 2)

#################################
#4 -	Split numeric and nominal variables into two different data frames
#################################
#nominal variables
chars.only <- sapply(dataset5,class)=='character'
chars <- dataset5[ , chars.only]

#numeric variables
nums.only <- sapply(dataset5,class)=='numeric'

nums <- dataset5[ , nums.only]
nums[, 1:46] <- sapply(nums[, 1:46], as.numeric)

#################################
#5 - Categorical variables to dummy
#################################

#convert categorical variables to dummies

#the same code was used for all categorical variables
for(level in unique(char$MS.Zoning)){
  char[paste("MS.Zoning", level, sep = "_")] <- ifelse(char$MS.Zoning == level, 1, 0)
}

# keep only dummy variables
dummy_chars <- subset(char, select = -c(Pool.QC, MS.Zoning, Lot.Shape, Land.Contour, Lot.Config, Neighborhood, Condition.1, Bldg.Type, House.Style, Roof.Style, Exterior.1st, Exterior.2nd, Mas.Vnr.Type, Exter.Qual, Exter.Cond, Foundation, Bsmt.Qual, Bsmt.Cond, Bsmt.Exposure, BsmtFin.Type.1, BsmtFin.Type.2, Heating.QC, Electrical, Kitchen.Qual, Functional, Garage.Type, Garage.Yr.Blt, Garage.Finish , Garage.Qual, Garage.Cond, Sale.Type, Sale.Condition) )
dummy_chars[, 1:211] <- sapply(dummy_chars[, 1:211], as.numeric)

#merge numeric and dummy variables
merged <- merge(nums, dum,  by = intersect( names(nums), names(dum)),  by.nums = "X", by.dum = "X")

#################################
#6 -	Reduce number of columns
#################################

#keep only attributes highly correlated to price
correlations <- cor(dum)

#correlation atts - price
pcor <- correlations[,1]
library(corrplot)
corrplot(cor(housenum2),method='e')

# keep only attributes with correlation to price greater/less than 0.25/-0.25 
row_indic <- apply(correlations, 1, function(x) sum(x > 0.25 | x < -0.25) > 1)
highcor2 <- highcor[ , row_indic]

#################################
#7 -	Lasso
#################################

#lasso - reduce number of attributes
library(magrittr)
library(dplyr)
library(psych)
data2 <- droplevels(data2)

#all attributes should be numeric
data2[, 1:58] <- sapply(data2[, 1:58], as.numeric)

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

#################################
#8 -	Train regression models
#################################

# Stepwise regression
mfull<- lm(log(SalePrice)~.+I(Gr.Liv.Area^2)+I(Total.Bsmt.SF^2),data=Mtotal)
step(mfull, direction='both')
step(mfull, direction='back')
mfull<-lm(log(SalePrice)~.+I(Gr.Liv.Area^2)+I(Total.Bsmt.SF^2),data=Mtotal)
mnull<-lm(log(SalePrice)~.+I(Gr.Liv.Area^2)+I(Total.Bsmt.SF^2),data=Mtotal)
step(mnull, scope=list(lower=mnull,upper=mfull), direction='both' )
summary(mfull)
anova(mfull)
plot(mfull)

#################################
#9 -	Evaluate outliers
#################################

cooksd <- cooks.distance(m)

#we clearly have outliers
# plot cook's distance
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  

# add cutoff line
abline(h = 4*mean(cooksd, na.rm=T), col="red")  
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")

# influential row numbers

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  head(Mtotal[influential, ])
car::outlierTest(m)

# Assessing Outliers
outlierTest(m) # Bonferonni p-value for most extreme obs
qqPlot(m, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(m) # leverage plots

# Influential Observations
# Cook's D plot
# identify D values 
cutoff <- 4/((nrow(Mtotal)-length(m$coefficients)-2)) 
plot(m, which=4, cook.levels=cutoff)

# Influence Plot 
influencePlot(m,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# remove outliers
Mtotal2 <- Mtotal[-c(718, 1262 ), ]

#################################
#10 - Plot predicted vs actual & residuals
#################################

training <- Mtotal
testing <- test
y_train = log(Mtotal$SalePrice)
y_test = log(test$SalePrice)
mse_lasso = sum((y_pred-y_test)^2)/length(y_test)
plot(y_test,y_pred,xlab="True log( price )",ylab="Predicted log( price )",
     main="Figure 1. Prediction using Stepwise regression")
mtext("add +I(Gr.Liv.Area^2)+I(Total.Bsmt.SF^2)", 3, line=0.5,cex=1.2)
text(-1,4,substitute(r^2 == r2,list(r2=cor(y_test,y_pred))),adj=0)
text(-1,3.7,substitute(MSE == r2,list(r2=mse_lasso)),adj=0)
abline(0,1)
eruption.res = resid(m9)
plot(Mtotal$SalePrice, eruption.res, ylab="Residuals", xlab="Waiting Time", 
     main="Residual Plot") 
abline(0, 0)

#A density plot
plot(density(resid(m9))) qqnorm(resid(m9)) 

# A quantile normal plot - good for checking normality
qqline(resid(m9))
dev.off()
residualPlot(m9, type='rstudent')
residualPlots(m9, plot=F)

# plot studentized residuals vs. fitted values 
spreadLevelPlot(m)

#################################
#11 -	More plots
#################################

# added variable plots 
av.plots(mfull)

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
install.packages('vioplot')
library(vioplot)
sapply(Mtotal3, class)
Mtotal3[, 1:15] <- sapply(Mtotal3[, 1:15], as.numeric)
eda.plots <- function(Mtotal3, ask=F){
  graphics.off()
  numeric.only <- sapply(Mtotal3,class)=='numeric'
  y <- Mtotal[,numeric.only]
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
eda.plots(Mtotal3)

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
#12 -	Evaluate regression model assumptions
#################################

# Evaluate homoscedasticity
# non-constant error variance test
lmtest::bptest(m) 
ncvTest(m)

# Evaluate Collinearity
vif(m) # variance inflation factors 
sqrt(vif(m)) > 2 # problem?

#fix collinearity
gvmodel2 <- gvlma(m) 
summary(gvmodel2)

# Evaluate Nonlinearity
# component + residual plot 
crPlots(m)

# Test for Autocorrelated Errors
library(lmtest);dwtest(m) # Global test of model assumptions

library(car); durbinWatsonTest(m)

# Global test of model assumptions
install.packages('gvlma')
library(gvlma)
gvmodel <- gvlma(m) 
summary(gvmodel)

# plot and evaluate residuals
eruption.res = resid(mfull2)
plot(Mtotal6$SalePrice, eruption.res, 
     ylab="Residuals", xlab="Sale Price", 
     main="Residual Plot") 
abline(0, 0)

#A density plot
plot(density(resid(mfull2))) 

# A quantile normal plot - good for checking normality
qqnorm(resid(mfull2)) 
qqline(resid(mfull2))
dev.off()
residualPlot(mfull2, type='rstudent')

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
ncvTest(m1)

#Levene's Test for Homogeneity of Variance (center = median)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)

leveneTest(rstudent(m)~yhat.quantiles)

# boxplot
boxplot(rstudent(m)~yhat.quantiles)

# check independence
plot(rstudent(m), type='l')
library(randtests); runs.test(m$res)

#################################
#13 -	Improve model
#################################

install.packages("boot")
library(boot)
library(MASS)
plot(mfull3, data=Mtotal3, main = "Housing" ,
     xlab = "Attributes", ylab = "Sale Price")
mfull3<- lm(log(SalePrice)~.+I(Gr.Liv.Area^5)+I(Total.Bsmt.SF^2),data=Mtotal3)
summary(cv.glm(Mtotal, mfull2,  10))

# leave-one-out and 10-fold cross-validation prediction error 
glm <- glm(log(SalePrice)~.+I(Gr.Liv.Area^5)+I(Total.Bsmt.SF^2),data=Mtotal)
(cv.err <- cv.glm(Mtotal, glm)$delta)
(cv.err.6 <- cv.glm(Mtotal, glm, K = 10)$delta)

# As this is a linear model we could calculate the leave-one-out 
# cross-validation estimate without any extra model-fitting.
muhat <- fitted(glm)
diag <- glm.diag(glm)
(cv.err <- mean((glm$y - muhat)^2/(1 - diag$h)^2))

# check which polynominal degree leads to min cv.error
#leave one out
require(boot)
myData <- Mtotal
glm.fit=glm(log(SalePrice)~.+I(Gr.Liv.Area^5)+I(Total.Bsmt.SF^2), data=myData )

#LOOCV
cv.glm(myData,glm.fit)$delta 
fit <- mfull2
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
  glm.fit=glm(log(SalePrice)~.+I(Gr.Liv.Area^5)+I(Total.Bsmt.SF^2), data=myData)
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
  glm.fit=glm(log(SalePrice)~.+I(poly(Gr.Liv.Area,d))+I(Total.Bsmt.SF^2), data=myData)
  cv.error10[d]=cv.glm(myData,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")

#################################
#14 -	Test model
#################################

#10 fold cross validatiton use dataset 22 as both training and test set
install.packages('faraway')
library(faraway)
install.packages('DAAG')
library(DAAG)
model3.daag<- CVlm(Mtotal6, m=10,form.lm=formula(log(SalePrice)~.+I(Gr.Liv.Area^5)+I(Total.Bsmt.SF^2), data=Mtotal6))
summary(model3.daag)

#10 fold cross validatiton use dataset 22 as training set and test dataset as test set
#predict the outcome of the testing data
predicted <- predict(model, newdata=testing[ ,-1])

# splitdf function will return a list of training and testing sets
splitdf <- function(Mtotal3, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(Mtotal3)
  trainset <- Mtotal3
  testset <- test2
  list(trainset=trainset,testset=testset)}

#apply the function
splits <- splitdf(Mtotal3 , seed=808)

#it returns a list - two data frames called trainset and testset
str(splits)
lapply(splits,nrow)

#view the first few columns in each data frame
lapply(splits,head)

# save the training and testing sets as data frames
training <- splits$trainset
testing <- splits$testset

#apply model
#fit the randomforest model
model <- lm(log(SalePrice)~.+I(Gr.Liv.Area^5)+I(Total.Bsmt.SF^2),data=Mtotal3)
print(model)

# the proportion variation explained in the outcome of the testing data
actual <- log(testing$SalePrice)
predicted <- unname(predicted)
rsq <- 1-sum((actual-predicted2)^2)/sum((actual-mean(actual))^2)

#plot actual vs predicted
par(mfrow=c(2,1))
plot(actual,type="l", main="Actual")
plot(predicted2, type="l", main = "Predictions")
par(mfrow=c(1,1))
plot(predicted2,type="l", col=2, main = "Actual (blue) vs Predictions (red)")
lines(actual,col="blue")
max(abs(predicted2-actual))


    
