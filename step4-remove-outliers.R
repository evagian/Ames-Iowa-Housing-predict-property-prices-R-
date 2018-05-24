#################################
#8 -	Train regression model
#################################
Mtotal <- read.csv("data/lasso-selected-attributes.csv", dec=",", stringsAsFactors=FALSE)
Mtotal <- subset(Mtotal, select=-c(X))

# Stepwise regression
mfull<- lm(log(SalePrice)~.+I(Gr.Liv.Area^2),data=Mtotal)
mnull<-lm(log(SalePrice)~+I(Gr.Liv.Area^2),data=Mtotal)
m<-step(mnull, scope=list(lower=mnull,upper=mfull), direction='both' )
summary(m)
anova(m)
plot(m)


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

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  
head(Mtotal[influential, ])

library(car)
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
Mtotal2 <- Mtotal[-c(1338, 341, 1189, 1453, 687, 1126 ), ]

write.csv(Mtotal2, "data/data-without-outliers.csv")

