library(dplyr)
library(glmnet)
# Predicting House Prices 
# Reading the train and test files and Substituting NA value in integer columns using mean substitution.
# Also removinng the outliers for train and test
# Also changing NA categories to None

# For Train ==================================

house.train.data <- read.csv(file.choose(), header=TRUE, sep=",", na.strings = "") # load train.csv

# to print structure of columns and columns having NA.
str(house.train.data)
names(which(colSums(house.train.data == "NA")>0))

#converting columns to integer type and imputing NAs with mean
house.train.data$LotFrontage <- as.integer(as.character(house.train.data$LotFrontage))
house.train.data$MasVnrArea <- as.integer(as.character(house.train.data$MasVnrArea))
house.train.data$GarageYrBlt <- as.integer(as.character(house.train.data$GarageYrBlt))

sum(is.na(house.train.data))
sum(is.na(house.train.data$LotFrontage))
sum(is.na(house.train.data$MasVnrArea))
sum(is.na(house.train.data$GarageYrBlt))

house.train.data$LotFrontage[is.na(house.train.data$LotFrontage)] <- round(mean(house.train.data$LotFrontage, na.rm = TRUE))
house.train.data$MasVnrArea[is.na(house.train.data$MasVnrArea)] <- round(mean(house.train.data$MasVnrArea, na.rm = TRUE))
house.train.data$GarageYrBlt[is.na(house.train.data$GarageYrBlt)] <- round(mean(house.train.data$GarageYrBlt, na.rm = TRUE))

# na.rm ignores NA while calculating mean
# # Changing NA in columns where NA are categories to None

house.train.data$Alley <- as.character(house.train.data$Alley)
house.train.data$Alley[which(house.train.data$Alley == "NA")] <- "None"
house.train.data$Alley <- as.factor(house.train.data$Alley)

house.train.data$MasVnrType <- as.character(house.train.data$MasVnrType)
house.train.data$MasVnrType[which(house.train.data$MasVnrType == "NA")] <- "None"
house.train.data$MasVnrType <- as.factor(house.train.data$MasVnrType)

house.train.data$BsmtQual <- as.character(house.train.data$BsmtQual)
house.train.data$BsmtQual[which(house.train.data$BsmtQual == "NA")] <- "None"
house.train.data$BsmtQual <- as.factor(house.train.data$BsmtQual)

house.train.data$BsmtCond <- as.character(house.train.data$BsmtCond)
house.train.data$BsmtCond[which(house.train.data$BsmtCond == "NA")] <- "None"
house.train.data$BsmtCond <- as.factor(house.train.data$BsmtCond)

house.train.data$BsmtExposure <- as.character(house.train.data$BsmtExposure)
house.train.data$BsmtExposure[which(house.train.data$BsmtExposure == "NA")] <- "None"
house.train.data$BsmtExposure <- as.factor(house.train.data$BsmtExposure)

house.train.data$BsmtFinType1 <- as.character(house.train.data$BsmtFinType1)
house.train.data$BsmtFinType1[which(house.train.data$BsmtFinType1 == "NA")] <- "None"
house.train.data$BsmtFinType1 <- as.factor(house.train.data$BsmtFinType1)

house.train.data$BsmtFinType2 <- as.character(house.train.data$BsmtFinType2)
house.train.data$BsmtFinType2[which(house.train.data$BsmtFinType2 == "NA")] <- "None"
house.train.data$BsmtFinType2 <- as.factor(house.train.data$BsmtFinType2)

house.train.data$Electrical <- as.character(house.train.data$Electrical)
house.train.data$Electrical[which(house.train.data$Electrical == "NA")] <- "None"
house.train.data$Electrical <- as.factor(house.train.data$Electrical)

house.train.data$FireplaceQu <- as.character(house.train.data$FireplaceQu)
house.train.data$FireplaceQu[which(house.train.data$FireplaceQu == "NA")] <- "None"
house.train.data$FireplaceQu <- as.factor(house.train.data$FireplaceQu)

house.train.data$GarageType <- as.character(house.train.data$GarageType)
house.train.data$GarageType[which(house.train.data$GarageType == "NA")] <- "None"
house.train.data$GarageType <- as.factor(house.train.data$GarageType)

house.train.data$GarageFinish <- as.character(house.train.data$GarageFinish)
house.train.data$GarageFinish[which(house.train.data$GarageFinish == "NA")] <- "None"
house.train.data$GarageFinish <- as.factor(house.train.data$GarageFinish)

house.train.data$GarageQual <- as.character(house.train.data$GarageQual)
house.train.data$GarageQual[which(house.train.data$GarageQual == "NA")] <- "None"
house.train.data$GarageQual <- as.factor(house.train.data$GarageQual)

house.train.data$GarageCond <- as.character(house.train.data$GarageCond)
house.train.data$GarageCond[which(house.train.data$GarageCond == "NA")] <- "None"
house.train.data$GarageCond <- as.factor(house.train.data$GarageCond)

house.train.data$PoolQC <- as.character(house.train.data$PoolQC)
house.train.data$PoolQC[which(house.train.data$PoolQC == "NA")] <- "None"
house.train.data$PoolQC <- as.factor(house.train.data$PoolQC)

house.train.data$Fence <- as.character(house.train.data$Fence)
house.train.data$Fence[which(house.train.data$Fence == "NA")] <- "None"
house.train.data$Fence <- as.factor(house.train.data$Fence)

house.train.data$MiscFeature <- as.character(house.train.data$MiscFeature)
house.train.data$MiscFeature[which(house.train.data$MiscFeature == "NA")] <- "None"
house.train.data$MiscFeature <- as.factor(house.train.data$MiscFeature)

str(house.train.data)
sum(is.na(house.train.data))
names(which(colSums(house.train.data == "NA")>0))

#replacing some outliers
house.train.data[house.train.data$Id == 524,]
house.train.data[house.train.data$Id == 525,]
house.train.data[house.train.data$Id == 524,-1] <- subset(house.train.data, Id == 525, select = -1)
house.train.data[house.train.data$Id == 524,]
house.train.data[house.train.data$Id == 525,]


house.train.data[house.train.data$Id == 1299,]
house.train.data[house.train.data$Id == 1300,]
house.train.data[house.train.data$Id == 1299,-1] <- subset(house.train.data, Id == 1300, select = -1)
house.train.data[house.train.data$Id == 1299,]
house.train.data[house.train.data$Id == 1300,]

write.csv(house.train.data, file = "trainImputedData.csv",  row.names = F)
# row.names, removes auto gen column while writing.

# ==================================================

# For Test

house.test.data <- read.csv(file.choose(), header=TRUE, sep=",", na.strings = "") # load test.csv

# to print structure of columns and columns having NA.
str(house.test.data)
names(which(colSums(house.test.data == "NA")>0))

#converting columns to integer type and imputing NAs with mean
house.test.data$LotFrontage <- as.integer(as.character(house.test.data$LotFrontage))
house.test.data$MasVnrArea <- as.integer(as.character(house.test.data$MasVnrArea))
house.test.data$GarageYrBlt <- as.integer(as.character(house.test.data$GarageYrBlt))

sum(is.na(house.test.data))
sum(is.na(house.test.data$LotFrontage))
sum(is.na(house.test.data$MasVnrArea))
sum(is.na(house.test.data$GarageYrBlt))

house.test.data$LotFrontage[is.na(house.test.data$LotFrontage)] <- round(mean(house.test.data$LotFrontage, na.rm = TRUE))
house.test.data$MasVnrArea[is.na(house.test.data$MasVnrArea)] <- round(mean(house.test.data$MasVnrArea, na.rm = TRUE))
house.test.data$GarageYrBlt[is.na(house.test.data$GarageYrBlt)] <- round(mean(house.test.data$GarageYrBlt, na.rm = TRUE))

# na.rm ignores NA while calculating mean

# # Changing NA in columns where NA are categories to None

house.test.data$Alley <- as.character(house.test.data$Alley)
house.test.data$Alley[which(house.test.data$Alley == "NA")] <- "None"
house.test.data$Alley <- as.factor(house.test.data$Alley)

house.test.data$MasVnrType <- as.character(house.test.data$MasVnrType)
house.test.data$MasVnrType[which(house.test.data$MasVnrType == "NA")] <- "None"
house.test.data$MasVnrType <- as.factor(house.test.data$MasVnrType)

house.test.data$BsmtQual <- as.character(house.test.data$BsmtQual)
house.test.data$BsmtQual[which(house.test.data$BsmtQual == "NA")] <- "None"
house.test.data$BsmtQual <- as.factor(house.test.data$BsmtQual)

house.test.data$BsmtCond <- as.character(house.test.data$BsmtCond)
house.test.data$BsmtCond[which(house.test.data$BsmtCond == "NA")] <- "None"
house.test.data$BsmtCond <- as.factor(house.test.data$BsmtCond)

house.test.data$BsmtExposure <- as.character(house.test.data$BsmtExposure)
house.test.data$BsmtExposure[which(house.test.data$BsmtExposure == "NA")] <- "None"
house.test.data$BsmtExposure <- as.factor(house.test.data$BsmtExposure)

house.test.data$BsmtFinType1 <- as.character(house.test.data$BsmtFinType1)
house.test.data$BsmtFinType1[which(house.test.data$BsmtFinType1 == "NA")] <- "None"
house.test.data$BsmtFinType1 <- as.factor(house.test.data$BsmtFinType1)

house.test.data$BsmtFinType2 <- as.character(house.test.data$BsmtFinType2)
house.test.data$BsmtFinType2[which(house.test.data$BsmtFinType2 == "NA")] <- "None"
house.test.data$BsmtFinType2 <- as.factor(house.test.data$BsmtFinType2)

house.test.data$Electrical <- as.character(house.test.data$Electrical)
house.test.data$Electrical[which(house.test.data$Electrical == "NA")] <- "None"
house.test.data$Electrical <- as.factor(house.test.data$Electrical)

house.test.data$FireplaceQu <- as.character(house.test.data$FireplaceQu)
house.test.data$FireplaceQu[which(house.test.data$FireplaceQu == "NA")] <- "None"
house.test.data$FireplaceQu <- as.factor(house.test.data$FireplaceQu)

house.test.data$GarageType <- as.character(house.test.data$GarageType)
house.test.data$GarageType[which(house.test.data$GarageType == "NA")] <- "None"
house.test.data$GarageType <- as.factor(house.test.data$GarageType)

house.test.data$GarageFinish <- as.character(house.test.data$GarageFinish)
house.test.data$GarageFinish[which(house.test.data$GarageFinish == "NA")] <- "None"
house.test.data$GarageFinish <- as.factor(house.test.data$GarageFinish)

house.test.data$GarageQual <- as.character(house.test.data$GarageQual)
house.test.data$GarageQual[which(house.test.data$GarageQual == "NA")] <- "None"
house.test.data$GarageQual <- as.factor(house.test.data$GarageQual)

house.test.data$GarageCond <- as.character(house.test.data$GarageCond)
house.test.data$GarageCond[which(house.test.data$GarageCond == "NA")] <- "None"
house.test.data$GarageCond <- as.factor(house.test.data$GarageCond)

house.test.data$PoolQC <- as.character(house.test.data$PoolQC)
house.test.data$PoolQC[which(house.test.data$PoolQC == "NA")] <- "None"
house.test.data$PoolQC <- as.factor(house.test.data$PoolQC)

house.test.data$Fence <- as.character(house.test.data$Fence)
house.test.data$Fence[which(house.test.data$Fence == "NA")] <- "None"
house.test.data$Fence <- as.factor(house.test.data$Fence)

house.test.data$MiscFeature <- as.character(house.test.data$MiscFeature)
house.test.data$MiscFeature[which(house.test.data$MiscFeature == "NA")] <- "None"
house.test.data$MiscFeature <- as.factor(house.test.data$MiscFeature)

str(house.test.data)
sum(is.na(house.test.data))
names(which(colSums(house.test.data == "NA")>0))

#There are some columns in test data where values are NA. 
# Imputing these columns with mode substitution

# get mode function to get the mode from columns for substitution.
getmode <- function(a) {
  uniqa <- unique(a)
  uniqa[which.max(tabulate(match(a, uniqa)))]
}

house.test.data$MSZoning[which(house.test.data$MSZoning == "NA")] <- getmode(house.test.data$MSZoning)
house.test.data$Utilities[which(house.test.data$Utilities == "NA")] <- getmode(house.test.data$Utilities)
house.test.data$Exterior1st[which(house.test.data$Exterior1st == "NA")] <- getmode(house.test.data$Exterior1st)
house.test.data$Exterior2nd[which(house.test.data$Exterior2nd == "NA")] <- getmode(house.test.data$Exterior2nd)
house.test.data$BsmtFinSF1[which(house.test.data$BsmtFinSF1 == "NA")] <- getmode(house.test.data$BsmtFinSF1)
house.test.data$BsmtFinSF2[which(house.test.data$BsmtFinSF2 == "NA")] <- getmode(house.test.data$BsmtFinSF2)
house.test.data$BsmtUnfSF[which(house.test.data$BsmtUnfSF == "NA")] <- getmode(house.test.data$BsmtUnfSF)
house.test.data$TotalBsmtSF[which(house.test.data$TotalBsmtSF == "NA")] <- getmode(house.test.data$TotalBsmtSF)
house.test.data$BsmtFullBath[which(house.test.data$BsmtFullBath == "NA")] <- getmode(house.test.data$BsmtFullBath)
house.test.data$BsmtHalfBath[which(house.test.data$BsmtHalfBath == "NA")] <- getmode(house.test.data$BsmtHalfBath)
house.test.data$KitchenQual[which(house.test.data$KitchenQual == "NA")] <- getmode(house.test.data$KitchenQual)
house.test.data$Functional[which(house.test.data$Functional == "NA")] <- getmode(house.test.data$Functional)
house.test.data$GarageCars[which(house.test.data$GarageCars == "NA")] <- getmode(house.test.data$GarageCars)
house.test.data$GarageArea[which(house.test.data$GarageArea == "NA")] <- getmode(house.test.data$GarageArea)
house.test.data$SaleType[which(house.test.data$SaleType == "NA")] <- getmode(house.test.data$SaleType)

names(which(colSums(house.test.data == "NA")>0))

# replacing outliers
summary(house.test.data$GarageYrBlt)
house.test.data$GarageYrBlt[which(house.test.data$GarageYrBlt == 2207)] <- 2007
summary(house.test.data$GarageYrBlt)

write.csv(house.test.data, file = "testImputedData.csv",  row.names = F)
# row.names, removes auto gen column while writing.

## ===========================================================

# Error (MAPE) Function

mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

## ===========================================================
# Creating a regression model :- 

house.train.data<- read.csv(file.choose(), header=TRUE, sep=",", na.strings = "") #load trainImputedData.csv

house.test.data<- read.csv(file.choose(), header=TRUE, sep=",", na.strings = "") #load testImputedData.csv

sum(is.na(house.train.data))
sum(is.na(house.test.data))
names(which(colSums(house.train.data == "NA")>0))
names(which(colSums(house.test.data == "NA")>0))


fit.step<-step(lm(SalePrice ~ .,data=house.train.data),direction="both")

summary(fit.step)

plot(fit.step)

# SalePrice, LotFrontage and MSSubClass histograms are left skewed. 
# Also, Sale Price and LotFrontage and other variables have heterosked. So Taking log is better.
#removing insigficant variables and building the model again.

fit.ii<-lm(log(SalePrice)~ MSZoning + log(LotFrontage) + log(LotArea) + Street + 
             + LotConfig + Neighborhood + Condition1 + 
             Condition2 + HouseStyle + log(OverallQual) + log(OverallCond) + log(YearBuilt) + 
             log(YearRemodAdd) + RoofMatl + ExterQual + Foundation + 
             BsmtQual + BsmtExposure + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF +
             HeatingQC + CentralAir + X2ndFlrSF + log(GrLivArea) + 
             BsmtFullBath + FullBath + HalfBath + KitchenAbvGr + Fireplaces + GarageCars + 
             GarageArea + GarageQual + WoodDeckSF + ScreenPorch + PoolArea + SaleCondition, data=house.train.data)

summary(fit.ii)

# #use the "fit" model to predict prices for the prediction data
predicted.prices.nonlinear<-exp(predict(fit.ii, newdata = house.test.data, type = "response")) 

# substituting NA values for SalePrice predition with mean.
predicted.prices.nonlinear[which(is.na(predicted.prices.nonlinear))] <- mean(predicted.prices.nonlinear,na.rm=T)

# calculating MAPE
predicted.prices.train<-exp(predict(fit.ii, newdata = house.train.data, type = "response"))
mape(house.train.data$SalePrice,predicted.prices.train)

# Writing to File
write.table(cbind(house.test.data$Id, predicted.prices.nonlinear), file = "Predicted Prices nonlinear.csv", 
            row.names = F, col.names = c("Id","SalePrice"), sep = ",")


#==================================
# creating dummy matrices and performing Lasso regression

library(glmnet)

house.data.lasso.training <- house.train.data
house.data.lasso.testing <- house.test.data
house.data.lasso.testing["SalePrice"] <- 0

house.data.glmnet <- rbind(house.data.lasso.training, house.data.lasso.testing)

#creating model matrix
house.data.lasso <- model.matrix( ~ MSZoning + log(LotFrontage) + log(LotArea) + Street + LotConfig
                                  + Neighborhood + Condition1 +Condition2 + HouseStyle + log(OverallQual)
                                  + log(OverallCond) + log(YearBuilt) + log(YearRemodAdd) + RoofMatl 
                                  + ExterQual + Foundation + BsmtQual + BsmtExposure + BsmtFinSF1
                                  + BsmtFinSF2 + BsmtUnfSF + HeatingQC + CentralAir + X2ndFlrSF
                                  + log(GrLivArea) + BsmtFullBath + FullBath + HalfBath + KitchenAbvGr
                                  + Fireplaces + GarageCars +GarageArea + GarageQual + WoodDeckSF
                                  + ScreenPorch + PoolArea + SaleCondition, data=house.data.glmnet)

head(house.data.lasso,1)
tail(house.data.lasso,1)
str(house.data.lasso)


# split X into testing, trainig/holdout and prediction
house.data.lasso.training<-house.data.lasso[1:1460,]
house.data.lasso.testing<-house.data.lasso[1461:2919,]
y<-log(house.train.data$SalePrice)

#LASSO (alpha=1)
lasso.fit<-glmnet(x = house.data.lasso.training, y = y, alpha = 1)
plot(lasso.fit, xvar = "lambda")

crossval <-  cv.glmnet(x = house.data.lasso.training, y = y, alpha = 1) #create cross-validation data
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
lasso.opt.fit <-glmnet(x = house.data.lasso.training, y = y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients

# predicting the performance on the testing set
predicted.prices.lasso <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =house.data.lasso.testing))

predicted.prices.lasso[which(is.na(predicted.prices.lasso))] <- mean(predicted.prices.lasso,na.rm=T)

# Writing to File
write.table(cbind(house.test.data$Id, predicted.prices.lasso), file = "Predicted Prices Lasso.csv", 
            row.names = F, col.names = c("Id","SalePrice"), sep = ",")

# calculating MAPE and RMSE
predicted.prices.train<-exp(predict(lasso.opt.fit, s = penalty.lasso, newx =house.data.lasso.training))
mape(house.train.data$SalePrice,predicted.prices.train) # MAPE
sqrt(mean((house.train.data$SalePrice - predicted.prices.train)^2)) # RMSE
#==================================
# performing Ridge regression


#Ridge Regression
ridge.fit<-glmnet(x = house.data.lasso.training, y = y, alpha = 0)
plot(ridge.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = house.data.lasso.training, y = y, alpha = 0)
plot(crossval)
penalty.ridge <- crossval$lambda.min 
log(penalty.ridge) 
ridge.opt.fit <-glmnet(x = house.data.lasso.training, y = y, alpha = 0, lambda = penalty.ridge) #estimate the model with that
coef(ridge.opt.fit)

predicted.prices.ridge <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =house.data.lasso.testing))

predicted.prices.ridge[which(is.na(predicted.prices.ridge))] <- mean(predicted.prices.ridge,na.rm=T)

# Writing to File

write.table(cbind(house.test.data$Id, predicted.prices.ridge), file = "Predicted Prices Ridge.csv", 
            row.names = F, col.names = c("Id","SalePrice"), sep = ",")


# calculating MAPE and RMSE
predicted.prices.train<-exp(predict(ridge.opt.fit, s = penalty.ridge, newx =house.data.lasso.training))
mape(house.train.data$SalePrice,predicted.prices.train) # MAPE
sqrt(mean((house.train.data$SalePrice - predicted.prices.train)^2)) # RMSE

#====================
#Lasso with multiple interactions

library(glmnet)

house.data.interactions <- model.matrix( ~ MSZoning + log(LotFrontage) + log(LotArea) * Street * LotConfig
                                         + Neighborhood * Condition1 * Condition2 * HouseStyle * log(OverallQual)
                                         + log(OverallCond) * log(YearBuilt) * log(YearRemodAdd) * RoofMatl 
                                         * ExterQual * Foundation * BsmtQual + BsmtExposure + BsmtFinSF1
                                         + BsmtFinSF2 + BsmtUnfSF + HeatingQC + CentralAir + X2ndFlrSF
                                         + log(GrLivArea) * BsmtFullBath * FullBath * HalfBath * KitchenAbvGr
                                         + Fireplaces * GarageCars *GarageArea + GarageQual + WoodDeckSF
                                         + ScreenPorch + PoolArea + SaleCondition, data=house.data.glmnet)

head(house.data.interactions,1)
tail(house.data.interactions,1)
str(house.data.lasso)


# split X into testing, trainig/holdout and prediction
house.data.interactions.training<-house.data.interactions[1:1460,]
house.data.interactions.testing<-house.data.interactions[1461:2919,]
y<-log(house.train.data$SalePrice)

#LASSO (alpha=1)
lasso.fit.interactions <-glmnet(x = house.data.interactions.training, y = y, alpha = 1)
plot(lasso.fit.interactions, xvar = "lambda")

crossval <-  cv.glmnet(x = house.data.interactions.training, y = y, alpha = 1) #create cross-validation data
plot(crossval)
penalty.lasso.interactions <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso.interactions) #see where it was on the graph
lasso.opt.fit.interactions <-glmnet(x = house.data.interactions.training, y = y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit.interactions) #resultant model coefficients

# predicting the performance on the testing set
lasso.testing.interactions <- exp(predict(lasso.opt.fit.interactions, s = penalty.lasso.interactions, newx =house.data.interactions.testing))

lasso.testing.interactions[which(is.na(lasso.testing.interactions))] <- mean(lasso.testing.interactions,na.rm=T)

write.table(cbind(house.test.data$Id, lasso.testing.interactions), file = "Predicted Prices interactions.csv", 
            row.names = F, col.names = c("Id","SalePrice"), sep = ",")

#Comparing non linear and lasso regression.
predicted.prices.train<-exp(predict(lasso.opt.fit.interactions, s = penalty.lasso.interactions, newx =house.data.interactions.training))
mape(house.train.data$SalePrice,predicted.prices.train)

#============================================================