#################################
# 7 - Data exploration and visualization
#################################
data <- read.csv("data/lasso-selected-attributes.csv", dec=",", stringsAsFactors=FALSE)
data = subset(data, select=-c(X))
str(data)
library(corrplot)

# visualize attribute inter correlation 
corrplot(cor(data),method='e')


plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}


doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}


plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}




cat <- subset(data, select = c(Overall.Qual,  Garage.Cars,
                               HasFireplace, MS.Zoning_RM, Neighborhood_StoneBr, Neighborhood_NridgHt,
                               Neighborhood_NoRidge, Exter.Qual_TA, Bsmt.Qual_Ex, Bsmt.Exposure_Gd,
                               BsmtFin.Type.1_GLQ, Kitchen.Qual_Ex, Kitchen.Qual_TA))
num <- subset(data, select = -c(Overall.Qual,  Garage.Cars,
                               HasFireplace, MS.Zoning_RM, Neighborhood_StoneBr, Neighborhood_NridgHt,
                               Neighborhood_NoRidge, Exter.Qual_TA, Bsmt.Qual_Ex, Bsmt.Exposure_Gd,
                               BsmtFin.Type.1_GLQ, Kitchen.Qual_Ex, Kitchen.Qual_TA))

doPlots(cat, fun = plotHist, ii = 1:5, ncol = 2)
doPlots(cat, fun = plotHist, ii = 6:10, ncol = 2)
doPlots(cat, fun = plotHist, ii = 11:13, ncol = 2)

doPlots(num, fun = plotDen, ii = 1:5, ncol = 2)
doPlots(num, fun = plotDen, ii = 6:7, ncol = 2)


