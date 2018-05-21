#################################
#1 - Data cleaning & null replacement
#################################
housing <- read.csv("data/dataset.csv", dec=",", stringsAsFactors=FALSE)

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
dataset <- df
dataset<- dataset[complete.cases(dataset), ]
colSums(sapply(dataset, is.na))

dataset$TotalArea <- dataset$Gr.Liv.Area + dataset$Total.Bsmt.SF
dataset$PriceTotalSQRM <- dataset$SalePrice/dataset$TotalArea
dataset$PriceLivSQRM <- dataset$SalePrice/dataset$Gr.Liv.Area
library(dplyr)
dataset3 <- dataset

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


library(data.table)
library(gridExtra)
library(corrplot)
library(ggplot2)
library(e1071)
library(dplyr)


#################################
#3 -	Split numeric and nominal variables into two different data frames
#################################
sapply(dataset5, class)
colSums(sapply(dataset5, is.na))
dataset5$Garage.Yr.Blt <- as.numeric(dataset5$Garage.Yr.Blt)

#nominal variables
chars.only <- sapply(dataset5,class)=='character'
chars <- dataset5[ , chars.only]
names(chars)
str(chars)

#numeric variables
nums.only <- sapply(dataset5,class)!='character'

nums <- dataset5[ , nums.only]
nums[, 1:42] <- sapply(nums[, 1:42], as.numeric)
colSums(sapply(nums, is.na))
nums <- subset(nums, select=-c(Garage.Yr.Blt))

#################################
#4 - Categorical variables to dummy
#################################

#convert categorical variables to dummies
names(chars)
#the same code was used for all categorical variables
for(level in unique(chars$MS.Zoning)){
  chars[paste("MS.Zoning", level, sep = "_")] <- ifelse(chars$MS.Zoning == level, 1, 0)
}

for(level in unique(chars$Lot.Shape)){
  chars[paste("Lot.Shape", level, sep = "_")] <- ifelse(chars$Lot.Shape == level, 1, 0)
}

for(level in unique(chars$Land.Contour)){
  chars[paste("Land.Contour", level, sep = "_")] <- ifelse(chars$Land.Contour == level, 1, 0)
}

for(level in unique(chars$Lot.Config)){
  chars[paste("Lot.Config", level, sep = "_")] <- ifelse(chars$Lot.Config == level, 1, 0)
}

for(level in unique(chars$Neighborhood)){
  chars[paste("Neighborhood", level, sep = "_")] <- ifelse(chars$Neighborhood == level, 1, 0)
}



for(level in unique(chars$Condition.1)){
  chars[paste("Condition.1", level, sep = "_")] <- ifelse(chars$Condition.1 == level, 1, 0)
}

for(level in unique(chars$Bldg.Type)){
  chars[paste("Bldg.Type", level, sep = "_")] <- ifelse(chars$Bldg.Type == level, 1, 0)
}

for(level in unique(chars$Roof.Style)){
  chars[paste("Roof.Style", level, sep = "_")] <- ifelse(chars$Roof.Style == level, 1, 0)
}
for(level in unique(chars$House.Style)){
  chars[paste("House.Style", level, sep = "_")] <- ifelse(chars$House.Style == level, 1, 0)
}
for(level in unique(chars$Exterior.1st)){
  chars[paste("Exterior.1st", level, sep = "_")] <- ifelse(chars$Exterior.1st == level, 1, 0)
}


for(level in unique(chars$Exterior.2nd)){
  chars[paste("Exterior.2nd", level, sep = "_")] <- ifelse(chars$Exterior.2nd == level, 1, 0)
}

for(level in unique(chars$Mas.Vnr.Type)){
  chars[paste("Mas.Vnr.Type", level, sep = "_")] <- ifelse(chars$Mas.Vnr.Type == level, 1, 0)
}

for(level in unique(chars$Exter.Qual)){
  chars[paste("Exter.Qual", level, sep = "_")] <- ifelse(chars$Exter.Qual == level, 1, 0)
}

for(level in unique(chars$Exter.Cond)){
  chars[paste("Exter.Cond", level, sep = "_")] <- ifelse(chars$Exter.Cond == level, 1, 0)
}
for(level in unique(chars$Foundation)){
  chars[paste("Foundation", level, sep = "_")] <- ifelse(chars$Foundation == level, 1, 0)
}


for(level in unique(chars$Bsmt.Qual)){
  chars[paste("Bsmt.Qual", level, sep = "_")] <- ifelse(chars$Bsmt.Qual == level, 1, 0)
}
for(level in unique(chars$Bsmt.Cond)){
  chars[paste("Bsmt.Cond", level, sep = "_")] <- ifelse(chars$Bsmt.Cond == level, 1, 0)
}

for(level in unique(chars$Bsmt.Exposure)){
  chars[paste("Bsmt.Exposure", level, sep = "_")] <- ifelse(chars$Bsmt.Exposure == level, 1, 0)
}

for(level in unique(chars$BsmtFin.Type.1)){
  chars[paste("BsmtFin.Type.1", level, sep = "_")] <- ifelse(chars$BsmtFin.Type.1 == level, 1, 0)
}
for(level in unique(chars$BsmtFin.Type.2)){
  chars[paste("BsmtFin.Type.2", level, sep = "_")] <- ifelse(chars$BsmtFin.Type.2 == level, 1, 0)
}


for(level in unique(chars$Heating.QC)){
  chars[paste("Heating.QC", level, sep = "_")] <- ifelse(chars$Heating.QC == level, 1, 0)
}
for(level in unique(chars$Electrical)){
  chars[paste("Electrical", level, sep = "_")] <- ifelse(chars$Electrical == level, 1, 0)
}

for(level in unique(chars$Kitchen.Qual)){
  chars[paste("Kitchen.Qual", level, sep = "_")] <- ifelse(chars$Kitchen.Qual == level, 1, 0)
}

for(level in unique(chars$Functional)){
  chars[paste("Functional", level, sep = "_")] <- ifelse(chars$Functional == level, 1, 0)
}
for(level in unique(chars$Garage.Type)){
  chars[paste("Garage.Type", level, sep = "_")] <- ifelse(chars$Garage.Type == level, 1, 0)
}


for(level in unique(chars$Garage.Finish)){
  chars[paste("Garage.Finish", level, sep = "_")] <- ifelse(chars$Garage.Finish == level, 1, 0)
}

for(level in unique(chars$Garage.Qual)){
  chars[paste("Garage.Qual", level, sep = "_")] <- ifelse(chars$Garage.Qual == level, 1, 0)
}

for(level in unique(chars$Garage.Cond)){
  chars[paste("Garage.Cond", level, sep = "_")] <- ifelse(chars$Garage.Cond == level, 1, 0)
}
for(level in unique(chars$Pool.QC)){
  chars[paste("Pool.QC", level, sep = "_")] <- ifelse(chars$Pool.QC == level, 1, 0)
}



for(level in unique(chars$Sale.Type)){
  chars[paste("Sale.Type", level, sep = "_")] <- ifelse(chars$Sale.Type == level, 1, 0)
}
for(level in unique(chars$Sale.Condition)){
  chars[paste("Sale.Condition", level, sep = "_")] <- ifelse(chars$Sale.Condition == level, 1, 0)
}

# keep only dummy variables
dummy_chars <- subset(chars, select = -c(Pool.QC, MS.Zoning, Lot.Shape, Land.Contour, Lot.Config, Neighborhood, Condition.1, Bldg.Type, House.Style, Roof.Style, Exterior.1st, Exterior.2nd, Mas.Vnr.Type, Exter.Qual, Exter.Cond, Foundation, Bsmt.Qual, Bsmt.Cond, Bsmt.Exposure, BsmtFin.Type.1, BsmtFin.Type.2, Heating.QC, Electrical, Kitchen.Qual, Functional, Garage.Type,Garage.Finish , Garage.Qual, Garage.Cond, Sale.Type, Sale.Condition) )
dummy_chars[, 1:214] <- sapply(dummy_chars[, 1:214], as.numeric)

#merge numeric and dummy variables
merged <- merge(nums, dummy_chars, by="row.names")

merged[, 1:257] <- sapply(merged[, 1:257], as.numeric)

merged = subset(merged, select=-c(Row.names))

merged<- merged[complete.cases(merged), ]
colSums(sapply(merged, is.na))
write.csv(merged, "data/merged.csv")

#################################
#5 -	Reduce number of columns
#################################

#keep only attributes highly correlated to price
names(merged)
#correlation atts - sale price
str(merged)
library(corrplot)

# keep only attributes with correlation to price greater/less than 0.25/-0.25 

#keep only attributes highly correlated to price
merged <- subset(merged, select=-c(Order, PID))

colSums(sapply(merged , is.na))
names(merged)
#keep only attributes highly correlated to price
correlations <- cor(merged)

#correlation atts - price
pcor <- correlations[,34]
summary(pcor)
library(corrplot)

# keep only attributes with correlation to price greater/less than 0.25/-0.25 
pcor<- data.frame(as.list(pcor))
pcor[colSums(pcor > 0.25 | pcor < -0.25) >= 1, ]

highcor <- subset(merged, select=c(SalePrice,
Lot.Frontage,  Lot.Area, Overall.Qual,
Year.Built, Year.Remod.Add,
Total.Bsmt.SF, X1st.Flr.SF, X2nd.Flr.SF,
Mas.Vnr.Area,BsmtFin.SF.1,
Gr.Liv.Area, Bsmt.Full.Bath,
Full.Bath, Half.Bath,
TotRms.AbvGrd, Garage.Cars ,Wood.Deck.SF ,Open.Porch.SF,
TotalArea, PriceTotalSQRM,
PriceLivSQRM ,HasFireplace , DecadeBuilt ,MS.Zoning_RL, MS.Zoning_RM,
Lot.Shape_IR1, Lot.Shape_Reg,
Neighborhood_StoneBr,
Neighborhood_NridgHt,
Neighborhood_NoRidge,
Roof.Style_Gable ,Roof.Style_Hip,
Exterior.1st_VinylSd,
Exterior.2nd_VinylSd,
Mas.Vnr.Type_None,Mas.Vnr.Type_Stone,
Exter.Qual_TA ,Exter.Qual_Gd,
Exter.Qual_Ex,
Foundation_CBlock, Foundation_PConc,
Bsmt.Qual_Ex ,Bsmt.Qual_TA,
Bsmt.Exposure_Gd ,Bsmt.Exposure_No  , BsmtFin.Type.1_GLQ,
Heating.QC_TA ,Heating.QC_Ex,
Kitchen.Qual_Gd ,Kitchen.Qual_Ex, Kitchen.Qual_TA,
Garage.Type_Attchd ,Garage.Type_Detchd,
Garage.Finish_Unf, Garage.Finish_Fin,
Garage.Cond_TA,
Sale.Type_New,
Sale.Condition_Partial
)) 
corrplot(cor(highcor),method='e')
write.csv(highcor, "data/highly-correlated.csv")
