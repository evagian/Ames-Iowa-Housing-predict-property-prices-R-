# Ames-Iowa-Housing-predict-property-prices-R-

A data set of 1500 residential property sales in Ames, Iowa between 2006 and 2012 was used for the purposes of this project.
The data set contains 82 explanatory variables describing every aspect of the home. The dataset is
heterogeneous containing both ordinal, nominal, continuous and discrete attributes. Lasso was used in order
to reduce the number of redundant variables and to define which ones will be used as input in the final
multiple regression model. Objective of this project is to compare the predictive performance of multiple
regression models and to identify the best model for predicting the prices of the properties.
2 Introduction

The data set can be found 
here http://www.amstat.org/publications/jse/v19n3/Decock/AmesHousing.txt
or here https://www.openintro.org/stat/data/?data=ames


Objective of this project is to identify the most important variables and to define the best regression model for
predicting the housing prices in Ames, Iowa. The data set used for the project purposes, describes 1500
residential property sales in Ames, Iowa between 2006 and 2012. It contains 82 explanatory variables
describing every aspect of the home. The dataset is heterogeneous and contains 23 ordinal, 23 nominal, 22
continuous and 14 discrete attributes. Continuous variables determine the various area dimensions such as
the size of the living area, the basement and the porch while discrete variables quantify the number of
rooms, baths, kitchens, parking spots etc. Nominal variables typically describe the various types or classes
of dwellings, materials and locations such as the name of the neighborhood, the garage type, the sale type
etc. Ordinal variables typically rate the quality and condition of different house parts and utilities.
The fact that the dataset was over parameterized and heterogeneous lead to the following hardships and
increased the difficulty of the analysis. The first problem was the necessity of reducing the number of
attributes used as input in the multiple regression model from 82 to 15. Glmnet Lasso was used for dealing
with the first problem. The fact that nominal variables should be converted to numeric before they can be
used as input in the regression model lead to the second problem. All nominal variables’ levels were
transformed to individual dummy variables and were then treated as binary attributes. Finally,
multicollinearity, the phenomenon in which predictor variables in a multiple regression model are highly
correlated complicated the attribute selection procedure.

In this project, multiple linear regression models are applied to the ANOVA enhancement and the model
which performs best will be further analyzed, interpreted and evaluated. The experimental results presented
in this paper (based on the Boston Housing data set) indicate the performance of the better fitted model
relative to the other models.

Only 15 attributes were used as input in the best fitted model. According to this project the
variables which foremost define the housing price are the overall quality of the house, the type of zone
where it is located, the year when it was built and remodeled, the number of parking spots and the garage
type, the size of living and basement area, whether it has fireplaces or not, the quality of the basement and
the kitchen, and finally the total number of rooms above ground.

Dataset overview 

![alt text](https://github.com/evagian/Ames-Iowa-Housing-predict-property-prices-R-/blob/master/data/iowa%20dataset%20overview.png)

Attribute correlation to price

![alt text](https://github.com/evagian/Ames-Iowa-Housing-predict-property-prices-R-/blob/master/data/iowa%20correlations%20to%20price.png)

The model 

![alt text](https://github.com/evagian/Ames-Iowa-Housing-predict-property-prices-R-/blob/master/data/iowa%20model.png)

Cross validation

![alt text](https://github.com/evagian/Ames-Iowa-Housing-predict-property-prices-R-/blob/master/data/iowa%20kfold.png)

Prediction on train set 

![alt text](https://github.com/evagian/Ames-Iowa-Housing-predict-property-prices-R-/blob/master/data/iowa%20train%20set%20prediction.png)

Prediction on test set 

![alt text](https://github.com/evagian/Ames-Iowa-Housing-predict-property-prices-R-/blob/master/data/iowa%20test%20set%20prediction.png)

Running the code

You should run the R code from all steps with the given order (from step 1 to step n)
