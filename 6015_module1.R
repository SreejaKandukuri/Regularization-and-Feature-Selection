library(rmarkdown)
library(readxl)
library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
library(magrittr)
library(RColorBrewer)
library(tables)
library(ggpubr)
library(pander)
library(corrgram)
library(corrplot)
library(zoo)
library(car)
library(leaps)




rawdataset = read.csv("C:\\Users\\sreej\\OneDrive\\Documents\\Cloud Computing\\ameshousing.csv", header=TRUE, stringsAsFactors=FALSE)
#descriptive statistics 
descp=summary(rawdataset)
descp
# to know the number of rows and columns in the dataset
row=nrow(rawdataset)
row
columns=ncol(rawdataset)
columns
# Finding the Nulls in the data set
nullvals =colSums(is.na(rawdataset))
nullvals
#Structure of the data
structure=str(rawdataset)
structure

#boxplot 
boxplot(rawdataset$Year.Built, main= "Boxplot of the Years in which the Houses were built", horizontal = T , col = "lightpink")

# boxplot of Year Sold
boxplot(rawdataset$Yr.Sold, main= "Boxplot of the Years the Houses were sold", col = "lightblue")

#histogram of total basement square feet 
hist(rawdataset$Total.Bsmt.SF, col= brewer.pal(8,"BrBG"), main="Histogram of Total Basement Squarefeet of the houses", xlab= "Sq.ft", breaks = 50)
#histogram of Garage Area
hist(rawdataset$Garage.Area, col= brewer.pal(8,"Set3"), main="Histogram of The Garage Area of the Houses", xlab= "Area of the Garage" )

#Data Cleaning and Preparation
rawdataset$Alley = replace_na("NA")
rawdataset$Fence = replace_na("NA")
rawdataset$Misc.Feature = replace_na("NA")
rawdataset$Pool.QC = replace_na("NA")
rawdataset$Garage.Type = replace_na("NA")
rawdataset$Garage.Finish = replace_na("NA")
rawdataset$Garage.Cond = replace_na("NA")
rawdataset$Garage.Qual = replace_na("NA")
rawdataset$Fireplace.Qu = replace_na("NA")
#Replacing them with null values 
rawdataset$Lot.Frontage[is.na(rawdataset$Lot.Frontage)] = mean(rawdataset$Lot.Frontage, na.rm=TRUE)
rawdataset$Mas.Vnr.Area[is.na(rawdataset$Mas.Vnr.Area)] = mean(rawdataset$Mas.Vnr.Area, na.rm=TRUE)
rawdataset$BsmtFin.SF.1[is.na(rawdataset$BsmtFin.SF.1)] = mean(rawdataset$BsmtFin.SF.1, na.rm=TRUE)
rawdataset$BsmtFin.SF.2[is.na(rawdataset$BsmtFin.SF.2)] = mean(rawdataset$BsmtFin.SF.2, na.rm=TRUE)
rawdataset$Total.Bsmt.SF[is.na(rawdataset$Total.Bsmt.SF)] = mean(rawdataset$Total.Bsmt.SF, na.rm=TRUE)
rawdataset$Bsmt.Unf.SF[is.na(rawdataset$Bsmt.Unf.SF)] = mean(rawdataset$Bsmt.Unf.SF, na.rm=TRUE)
rawdataset$Garage.Yr.Blt[is.na(rawdataset$Garage.Yr.Blt)] = mean(rawdataset$Garage.Yr.Blt, na.rm=TRUE)
rawdataset$Garage.Area[is.na(rawdataset$Garage.Area)] = mean(rawdataset$Garage.Area, na.rm=TRUE)
rawdataset$Garage.Cars[is.na(rawdataset$Garage.Cars)] = mean(rawdataset$Garage.Cars, na.rm=TRUE)
rawdataset$Bsmt.Full.Bath[is.na(rawdataset$Bsmt.Full.Bath)] = mean(rawdataset$Bsmt.Full.Bath, na.rm=TRUE)
rawdataset$Bsmt.Half.Bath[is.na(rawdataset$Bsmt.Half.Bath)] = mean(rawdataset$Bsmt.Half.Bath, na.rm=TRUE)

#Correlation Matrix and plot 
# Select numeric variables from the dataset
numerics = sapply(rawdataset, is.numeric)

# Subset the dataset with only numeric variables
numericdataset = rawdataset[, numerics]

#correlation matrix
cormatrix = cor(numericdataset)


# Create a correlation plot using corrplot
corrplot(cormatrix, method = "color")

#Scatterplot of saleprice with overall.Qual - Highest Correlation
scatterplot( rawdataset$SalePrice ~ rawdataset$Overall.Qual, xlab = "Overall Qual", ylab = "Sale Price", main = "Scatter Plot: Sale Price vs Overall Qual")


#Scatterplot of saleprice with Enclosed Porch - Lowest Correlation
scatterplot( rawdataset$SalePrice ~ rawdataset$Enclosed.Porch, xlab = "Enclosed Porch", ylab = "Sale Price", main = "Scatter Plot: Sale Price vs Enclosed Porch")

#Scatterplot of saleprice with TotRms.AbvGrd - closest to 0.5
scatterplot( rawdataset$SalePrice ~ rawdataset$TotRms.AbvGrd, xlab = "TotRms.Abv.Grd", ylab = "Sale Price", main = "Scatter Plot: Sale Price vs TotRms.AbvGrd")

# regression model
regmodel = lm(rawdataset$SalePrice ~ rawdataset$Gr.Liv.Area + rawdataset$Garage.Area + rawdataset$Total.Bsmt.SF, data = rawdataset)

# Summarize the regression model
summary(regmodel)

# plotting regression model
par(mfrow = c(2, 2))

plot(regmodel)


#Task:10 - multicollinearity
multicolli= vif(regmodel)
print(multicolli)

#task11 : outliers check 

# Plot standardized residuals vs fitted values
plot(regmodel, which = 1)

# Plot leverage vs standardized residuals
plot(regmodel, which = 5)

# attempting to make changes 
regmodel = lm(SalePrice ~ Gr.Liv.Area + Garage.Area + Total.Bsmt.SF, data = rawdataset)
residuals = resid(regmodel)
# Residuals vs. Predictor Variables Plot
plot(rawdataset$Gr.Liv.Area, residuals, main = "Residuals vs. Gr.Liv.Area", xlab = "Gr.Liv.Area", ylab = "Residuals")
plot(rawdataset$Garage.Area, residuals, main = "Residuals vs. Garage.Area", xlab = "Garage.Area", ylab = "Residuals")
plot(rawdataset$Total.Bsmt.SF, residuals, main = "Residuals vs. Total.Bsmt.SF", xlab = "Total.Bsmt.SF", ylab = "Residuals")




# task 13: Create a data frame containing all the predictor variables 
predictors = c("Gr.Liv.Area","Garage.Area","Total.Bsmt.SF")  
data_subset = rawdataset[, c("SalePrice", predictors)]  

# Perform all subsets regression
allsubsetsmodel = regsubsets(SalePrice ~ ., data = data_subset)

# Get the summary of all subsets regression
summarysubsets = summary(allsubsetsmodel)

# best model based on adjusted R-squared
best_model = which.max(summarysubsets$adjr2)


# Extract the predictor variables from the best model
best_predictors=  predictors[as.logical(summarysubsets$which[best_model, -1])]

# Extract the equation of the preferred model
preferredmodelequation = paste("SalePrice =", paste(best_predictors, collapse = " + "))

print(preferredmodelequation)

