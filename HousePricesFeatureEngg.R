
#=======Initial Library Loading and Reading and Writing Data======

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} 
# Check if you have universal installer package, install if not

pacman::p_load("dplyr","caret","corrplot","ROCR","lift","glmnet","MASS","e1071", "xgboost", "randomForest", "nnet") 
#Check, and if needed install the necessary packages


# Reading and Writing data
getwd()
setwd("C:/Users/Mann-A2/Downloads/House Prices")
#file <- read.csv("File.csv")

# Predicting House Prices 
# Reading the train and test files.
# Substituting NA value in integer columns using mean
# Substituting NA value in character columns using mode substitution.
# Also removinng the outliers for train and test
# Also changing NA categories to None

# For Train ==================================

house.train.data <- read.csv(file.choose(), header=TRUE, sep=",", na.strings = "") # load train.csv
house.test.data <- read.csv(file.choose(), header=TRUE, sep=",", na.strings = "") # load test.csv
house.test.data["SalePrice"] <- ""
house.data <- rbind(house.train.data, house.test.data)
write.csv(house.data, file = "House Data.csv",  row.names = F)

house.data <- read.csv(file.choose(), header=TRUE, sep=",", na.strings = "") # House Data.csv

#to print structure of columns and columns having NA.
str(house.data)
names(which(colSums(house.data == "NA")>0))

#converting columns to integer type and imputing NAs with mean
house.data$LotFrontage <- as.integer(as.character(house.data$LotFrontage))
house.data$MasVnrArea <- as.integer(as.character(house.data$MasVnrArea))
house.data$GarageYrBlt <- as.integer(as.character(house.data$GarageYrBlt))

sum(is.na(house.data))
sum(is.na(house.data$LotFrontage))
sum(is.na(house.data$MasVnrArea))
sum(is.na(house.data$GarageYrBlt))

house.data$LotFrontage[is.na(house.data$LotFrontage)] <- round(mean(house.data$LotFrontage, na.rm = TRUE))
house.data$MasVnrArea[is.na(house.data$MasVnrArea)] <- round(mean(house.data$MasVnrArea, na.rm = TRUE))
house.data$GarageYrBlt[is.na(house.data$GarageYrBlt)] <- round(mean(house.data$GarageYrBlt, na.rm = TRUE))

# na.rm ignores NA while calculating mean

# Imputing character NA columns with mode substitution

# get mode function to get the mode from columns for substitution.
getmode <- function(a) {
  uniqa <- unique(a)
  uniqa[which.max(tabulate(match(a, uniqa)))]
}

house.data$MSZoning[which(house.data$MSZoning == "NA")] <- getmode(house.data$MSZoning)
house.data$Utilities[which(house.data$Utilities == "NA")] <- getmode(house.data$Utilities)
house.data$Exterior1st[which(house.data$Exterior1st == "NA")] <- getmode(house.data$Exterior1st)
house.data$Exterior2nd[which(house.data$Exterior2nd == "NA")] <- getmode(house.data$Exterior2nd)
house.data$BsmtFinSF1[which(house.data$BsmtFinSF1 == "NA")] <- getmode(house.data$BsmtFinSF1)
house.data$BsmtFinSF2[which(house.data$BsmtFinSF2 == "NA")] <- getmode(house.data$BsmtFinSF2)
house.data$BsmtUnfSF[which(house.data$BsmtUnfSF == "NA")] <- getmode(house.data$BsmtUnfSF)
house.data$TotalBsmtSF[which(house.data$TotalBsmtSF == "NA")] <- getmode(house.data$TotalBsmtSF)
house.data$BsmtFullBath[which(house.data$BsmtFullBath == "NA")] <- getmode(house.data$BsmtFullBath)
house.data$BsmtHalfBath[which(house.data$BsmtHalfBath == "NA")] <- getmode(house.data$BsmtHalfBath)
house.data$KitchenQual[which(house.data$KitchenQual == "NA")] <- getmode(house.data$KitchenQual)
house.data$Functional[which(house.data$Functional == "NA")] <- getmode(house.data$Functional)
house.data$GarageCars[which(house.data$GarageCars == "NA")] <- getmode(house.data$GarageCars)
house.data$GarageArea[which(house.data$GarageArea == "NA")] <- getmode(house.data$GarageArea)
house.data$SaleType[which(house.data$SaleType == "NA")] <- getmode(house.data$SaleType)

names(which(colSums(house.data == "NA")>0))

# Changing NA in columns where NA are categories to None

house.data$Alley <- as.character(house.data$Alley)
house.data$Alley[which(house.data$Alley == "NA")] <- "None"
house.data$Alley <- as.factor(house.data$Alley)

house.data$MasVnrType <- as.character(house.data$MasVnrType)
house.data$MasVnrType[which(house.data$MasVnrType == "NA")] <- "None"
house.data$MasVnrType <- as.factor(house.data$MasVnrType)

house.data$BsmtQual <- as.character(house.data$BsmtQual)
house.data$BsmtQual[which(house.data$BsmtQual == "NA")] <- "None"
house.data$BsmtQual <- as.factor(house.data$BsmtQual)

house.data$BsmtCond <- as.character(house.data$BsmtCond)
house.data$BsmtCond[which(house.data$BsmtCond == "NA")] <- "None"
house.data$BsmtCond <- as.factor(house.data$BsmtCond)

house.data$BsmtExposure <- as.character(house.data$BsmtExposure)
house.data$BsmtExposure[which(house.data$BsmtExposure == "NA")] <- "None"
house.data$BsmtExposure <- as.factor(house.data$BsmtExposure)

house.data$BsmtFinType1 <- as.character(house.data$BsmtFinType1)
house.data$BsmtFinType1[which(house.data$BsmtFinType1 == "NA")] <- "None"
house.data$BsmtFinType1 <- as.factor(house.data$BsmtFinType1)

house.data$BsmtFinType2 <- as.character(house.data$BsmtFinType2)
house.data$BsmtFinType2[which(house.data$BsmtFinType2 == "NA")] <- "None"
house.data$BsmtFinType2 <- as.factor(house.data$BsmtFinType2)

house.data$Electrical <- as.character(house.data$Electrical)
house.data$Electrical[which(house.data$Electrical == "NA")] <- "None"
house.data$Electrical <- as.factor(house.data$Electrical)

house.data$FireplaceQu <- as.character(house.data$FireplaceQu)
house.data$FireplaceQu[which(house.data$FireplaceQu == "NA")] <- "None"
house.data$FireplaceQu <- as.factor(house.data$FireplaceQu)

house.data$GarageType <- as.character(house.data$GarageType)
house.data$GarageType[which(house.data$GarageType == "NA")] <- "None"
house.data$GarageType <- as.factor(house.data$GarageType)

house.data$GarageFinish <- as.character(house.data$GarageFinish)
house.data$GarageFinish[which(house.data$GarageFinish == "NA")] <- "None"
house.data$GarageFinish <- as.factor(house.data$GarageFinish)

house.data$GarageQual <- as.character(house.data$GarageQual)
house.data$GarageQual[which(house.data$GarageQual == "NA")] <- "None"
house.data$GarageQual <- as.factor(house.data$GarageQual)

house.data$GarageCond <- as.character(house.data$GarageCond)
house.data$GarageCond[which(house.data$GarageCond == "NA")] <- "None"
house.data$GarageCond <- as.factor(house.data$GarageCond)

house.data$PoolQC <- as.character(house.data$PoolQC)
house.data$PoolQC[which(house.data$PoolQC == "NA")] <- "None"
house.data$PoolQC <- as.factor(house.data$PoolQC)

house.data$Fence <- as.character(house.data$Fence)
house.data$Fence[which(house.data$Fence == "NA")] <- "None"
house.data$Fence <- as.factor(house.data$Fence)

house.data$MiscFeature <- as.character(house.data$MiscFeature)
house.data$MiscFeature[which(house.data$MiscFeature == "NA")] <- "None"
house.data$MiscFeature <- as.factor(house.data$MiscFeature)

str(house.data)
sum(is.na(house.data))
names(which(colSums(house.data == "NA")>0))

#replacing some outliers
house.data[house.data$Id == 524,]
house.data[house.data$Id == 525,]
house.data[house.data$Id == 524,-1] <- subset(house.data, Id == 525, select = -1)
house.data[house.data$Id == 524,]
house.data[house.data$Id == 525,]

house.data[house.data$Id == 1299,]
house.data[house.data$Id == 1300,]
house.data[house.data$Id == 1299,-1] <- subset(house.data, Id == 1300, select = -1)
house.data[house.data$Id == 1299,]
house.data[house.data$Id == 1300,]

summary(house.data$GarageYrBlt)
house.data$GarageYrBlt[which(house.data$GarageYrBlt == 2207)] <- 2007
summary(house.data$GarageYrBlt)

#================================
#Combining Rare Categories

table(house.data$Alley)# check for rare categories

# Create another a custom function to combine rare categories into "Other."+the name of the original variavle (e.g., Other.State)
# This function has two arguments: the name of the dataframe and the count of observation in a category to define "rare"

combinerarecategories<-function(data_frame,mincount){ 
  for (i in 1 : ncol(data_frame)){
    a<-data_frame[,i]
    replace <- names(which(table(a) < mincount))
    levels(a)[levels(a) %in% replace] <-paste("Other",colnames(data_frame)[i],sep=".")
    data_frame[,i]<-a }
  return(data_frame) }

combinerarecategories(house.data,5)

##=======================Feature Engineering.

str(house.data)

house.data$TotalBsmtSF <- as.integer(as.character(house.data$TotalBsmtSF))
house.data$X1stFlrSF <- as.integer(as.character(house.data$X1stFlrSF))
house.data$X2ndFlrSF <- as.integer(as.character(house.data$X2ndFlrSF))
house.data$EnclosedPorch <- as.integer(as.character(house.data$EnclosedPorch))
house.data$ScreenPorch <- as.integer(as.character(house.data$ScreenPorch))
house.data$X3SsnPorch <- as.integer(as.character(house.data$X3SsnPorch))
house.data$OpenPorchSF <- as.integer(as.character(house.data$OpenPorchSF))
house.data$FullBath <- as.integer(as.character(house.data$FullBath))
house.data$HalfBath <- as.integer(as.character(house.data$HalfBath))
house.data$BsmtHalfBath <- as.integer(as.character(house.data$BsmtHalfBath))
house.data$BsmtFullBath <- as.integer(as.character(house.data$BsmtFullBath))
house.data$GarageArea <- as.integer(as.character(house.data$GarageArea))

# TotalBsmtSF is BsmtFinSF2+BsmtUnfSF+BsmtFinSF1 #SF - Square footage
house.data$TotalSF <- house.data$TotalBsmtSF + house.data$X1stFlrSF + house.data$X2ndFlrSF
house.data$PorchSF <- house.data$EnclosedPorch + house.data$ScreenPorch + house.data$X3SsnPorch + house.data$OpenPorchSF + house.data$WoodDeckSF
house.data$countBathrooms <- house.data$FullBath + 0.5*house.data$HalfBath + 0.5*house.data$BsmtHalfBath + house.data$BsmtFullBath	


corrplot(cor(house.data[1:2]), method = "circle")

write.csv(house.data, file = "House Prices Imputed.csv",  row.names = F)
# row.names, removes auto gen column while writing.

## ===========================================================

# Error (MAPE) Function

mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

## ===========================================================
# Creating a linear regression model :- 

house.imputed.data<- read.csv(file.choose(), header=TRUE, sep=",", na.strings = "") #load House Prices Imputed.csv
str(house.imputed.data)
house.imputed.data$SalePrice <- as.integer(as.character(house.imputed.data$SalePrice))
tail(house.imputed.data)

training.x<-house.imputed.data[1:1460,]
testing.x<-house.imputed.data[1461:2919,]

fit.step<-step(lm(SalePrice ~ .,data=training.x),direction="both")

# SalePrice, LotFrontage and MSSubClass histograms are left skewed. 
# Also, Sale Price and LotFrontage and other variables have heterosked. So Taking log is better.
#removing insigficant variables and building the model again.

fit.ii<-step(lm(log(SalePrice)~ MSZoning + log(LotFrontage) + log(LotArea) + Street + LotConfig
                + Neighborhood + Condition1 +Condition2 + HouseStyle + log(OverallQual)
                + log(OverallCond) + log(YearBuilt) + log(YearRemodAdd) + RoofMatl 
                + ExterQual + Foundation + BsmtQual + BsmtExposure + BsmtFinSF1
                + BsmtFinSF2 + BsmtUnfSF + HeatingQC + CentralAir + TotalSF
                + log(GrLivArea) + countBathrooms + KitchenAbvGr
                + Fireplaces + GarageCars +GarageArea + GarageQual
                + PorchSF + PoolArea + SaleCondition,
                data=training.x),direction="both")

summary(fit.ii)

# use the "fit" model to predict prices for the prediction data
predicted.prices.linear<-exp(predict(fit.ii, newdata = testing.x, type = "response")) 

# substituting NA values for SalePrice predition with mean.
predicted.prices.linear[which(is.na(predicted.prices.linear))] <- mean(predicted.prices.linear,na.rm=T)

# Writing to File
write.table(cbind(house.test.data$Id, predicted.prices.linear), file = "Predicted Prices linear.csv", 
            row.names = F, col.names = c("Id","SalePrice"), sep = ",")


#==================================
# creating dummy matrices and performing Lasso and Ridge regression

house.imputed.data<- read.csv(file.choose(), header=TRUE, sep=",", na.strings = "") #load House Prices Imputed.csv
str(house.imputed.data)
house.imputed.data$SalePrice <- as.integer(as.character(house.imputed.data$SalePrice))
tail(house.imputed.data)

training.x<-house.imputed.data[1:1460,]
testing.x<-house.imputed.data[1461:2919,]


house.data.lasso.training <- training.x
house.data.lasso.testing <- testing.x
house.data.lasso.testing["SalePrice"] <- 0

house.data.glmnet <- rbind(house.data.lasso.training, house.data.lasso.testing)

#creating model matrix, Interactions with Lasso
house.data.lasso <- model.matrix( ~ MSZoning + log(LotFrontage) + log(LotArea) + Street + LotConfig
                                  + Neighborhood + Condition1 +Condition2 + HouseStyle + log(OverallQual)
                                  + log(OverallCond) + log(YearBuilt) + log(YearRemodAdd) + RoofMatl 
                                  + ExterQual + Foundation + BsmtQual + BsmtExposure + BsmtFinSF1
                                  + BsmtFinSF2 + BsmtUnfSF + HeatingQC + CentralAir + TotalSF
                                  + log(GrLivArea) + countBathrooms + KitchenAbvGr + Fireplaces
                                  + GarageCars +GarageArea + GarageQual + PorchSF + PoolArea 
                                  + SaleCondition  + TotalSF*OverallQual + Fireplaces*YearBuilt 
                                  + BsmtQual*OverallQual + KitchenQual*TotalSF 
                                  + as.integer(ExterQual) * as.integer(ExterCond), data=house.data.glmnet)

# split X into testing, training/holdout and prediction
house.data.lasso.training<-house.data.lasso[1:1460,]
house.data.lasso.testing<-house.data.lasso[1461:2919,]
y<-log(training.x$SalePrice)

#LASSO (alpha=1)
lasso.fit<-glmnet(x = house.data.lasso.training, y = y, alpha = 1)
plot(lasso.fit, xvar = "lambda")

crossval <-  cv.glmnet(x = house.data.lasso.training, y = y, alpha = 1) #create cross-validation data
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph #log(lamdba) = -7.088, Kaggle score = 0.11490
lasso.opt.fit <-glmnet(x = house.data.lasso.training, y = y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients

# predicting the performance on the testing set
predicted.prices.lasso <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =house.data.lasso.testing))
predicted.prices.lasso[which(is.na(predicted.prices.lasso))] <- mean(predicted.prices.lasso,na.rm=T)

# Writing to File
write.table(cbind(house.test.data$Id, predicted.prices.lasso), file = "Predicted Prices Lasso.csv", 
            row.names = F, col.names = c("Id","SalePrice"), sep = ",")


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


# Elastic Net Model :- 

set.seed(123)
cv_5 = trainControl(method = "cv", number = 5)

hit_elnet = train( SalePrice ~ MSZoning + log(LotFrontage) + log(LotArea) + Street + LotConfig
                   + Neighborhood + Condition1 +Condition2 + HouseStyle + log(OverallQual)
                   + log(OverallCond) + log(YearBuilt) + log(YearRemodAdd) + RoofMatl 
                   + ExterQual + Foundation + BsmtQual + BsmtExposure + BsmtFinSF1
                   + BsmtFinSF2 + BsmtUnfSF + HeatingQC + CentralAir + TotalSF
                   + log(GrLivArea) + countBathrooms + KitchenAbvGr
                   + Fireplaces + GarageCars +GarageArea + GarageQual
                   + PorchSF + PoolArea + SaleCondition, data=house.data.glmnet, 
                   method = "glmnet",  trControl = cv_5)


# split X into testing, training/holdout and prediction
house.data.elastic.training<-house.data.lasso[1:1460,]
house.data.elastic.testing<-house.data.lasso[1461:2919,]
y<-log(training.x$SalePrice)

#LASSO (alpha=1)
elastic.fit<-glmnet(x = house.data.elastic.training, y = y, alpha = 0.55) # alpha = 0.55, lambda = 6913.918
plot(lasso.fit, xvar = "lambda")

crossval <-  cv.glmnet(x = house.data.elastic.training, y = y, alpha = 0.55) #create cross-validation data
plot(crossval)
penalty.elastic <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.elastic) #see where it was on the graph 
elastic.opt.fit <-glmnet(x = house.data.elastic.training, y = y, alpha = 0.55, lambda = penalty.elastic) #estimate the model with the optimal penalty

# predicting the performance on the testing set
predicted.prices.elastic <- exp(predict(elastic.opt.fit, s = penalty.elastic, newx =house.data.elastic.testing))
predicted.prices.elastic[which(is.na(predicted.prices.elastic))] <- mean(predicted.prices.elastic,na.rm=T)

# Writing to File
write.table(cbind(house.test.data$Id, predicted.prices.lasso), file = "Predicted Prices Elastic.csv", 
            row.names = F, col.names = c("Id","SalePrice"), sep = ",")


#============================================================
# Random Forest 1

house.imputed.data<- read.csv(file.choose(), header=TRUE, sep=",", na.strings = "") #load House Prices Imputed.csv
str(house.imputed.data)
house.imputed.data$SalePrice <- as.integer(as.character(house.imputed.data$SalePrice))
tail(house.imputed.data)

training.x<-house.imputed.data[1:1460,]
testing.x<-house.imputed.data[1461:2919,]

str(training.x)
str(testing.x)

model_forest <- randomForest(SalePrice~ ., data=training.x, 
                             importance=TRUE,proximity=TRUE,
                             cutoff = c(0.5, 0.5),type="regression")

predicted.prices.forest<-predict(model_forest,newdata=testing.x,type="response")
predicted.prices.forest[which(is.na(predicted.prices.forest))] <- mean(predicted.prices.forest,na.rm=T)

write.table(cbind(testing.x$Id, predicted.prices.forest), file = "Predicted Prices Forest.csv", 
            row.names = F, col.names = c("Id","SalePrice"), sep = ",")



# Random Forest 2
model_forest <- randomForest(SalePrice~ MSZoning + (LotFrontage) + (LotArea) + Street + LotConfig
                             + Neighborhood + Condition1 +Condition2 + HouseStyle + 
                               + (OverallCond) + (YearBuilt) + (YearRemodAdd) + RoofMatl 
                             + ExterQual + Foundation + BsmtQual + BsmtExposure + BsmtFinSF1
                             + BsmtFinSF2 + BsmtUnfSF + HeatingQC + CentralAir + TotalSF
                             + (GrLivArea) + countBathrooms + KitchenAbvGr
                             + Fireplaces + GarageCars +GarageArea + GarageQual
                             + PorchSF + PoolArea + SaleCondition, data=training.x, 
                             importance=TRUE,proximity=TRUE,
                             cutoff = c(0.5, 0.5),type="regression")


predicted.prices.forest<- predict(model_forest,newdata=testing.x,type="response")
predicted.prices.forest[which(is.na(predicted.prices.forest))] <- mean(predicted.prices.forest,na.rm=T)

write.table(cbind(testing.x$Id, predicted.prices.forest), file = "Predicted Prices forest 2.csv", 
            row.names = F, col.names = c("Id","SalePrice"), sep = ",")

#============================================================
#Gradient Boosting

house.imputed.data<- read.csv(file.choose(), header=TRUE, sep=",", na.strings = "") #load House Prices Imputed.csv
str(house.imputed.data)
house.imputed.data$SalePrice <- as.integer(as.character(house.imputed.data$SalePrice))
tail(house.imputed.data)

training.x<-house.imputed.data[1:1460,]
testing.x<-house.imputed.data[1461:2919,]

house.data.gb.training <- training.x
house.data.gb.testing <- testing.x
house.data.gb.testing["SalePrice"] <- 0

house.data.gb <- rbind(house.data.gb.training, house.data.gb.testing)

#creating model matrix
house.data.gb <- model.matrix( ~ MSZoning + log(LotFrontage) + log(LotArea) + Street + LotConfig
                               + Neighborhood + Condition1 +Condition2 + HouseStyle + log(OverallQual)
                               + log(OverallCond) + log(YearBuilt) + log(YearRemodAdd) + RoofMatl 
                               + ExterQual + Foundation + BsmtQual + BsmtExposure + BsmtFinSF1
                               + BsmtFinSF2 + BsmtUnfSF + HeatingQC + CentralAir + TotalSF
                               + log(GrLivArea) + countBathrooms + KitchenAbvGr
                               + Fireplaces + GarageCars +GarageArea + GarageQual
                               + PorchSF + PoolArea + SaleCondition, data=house.data.gb)

# split X into testing, training/holdout and prediction
house.data.gb.training<-house.data.gb[1:1460,]
house.data.gb.testing<-house.data.gb[1461:2919,]

model_XGboost<-xgboost(data = house.data.gb.training[,-1],
                       label = as.numeric(as.character(training.x$SalePrice)), 
                       objective = "reg:linear",
                       booster = "gbtree",
                       eta=0.3, #default = 0.3
                       gamma=1,
                       max_depth=6, #default=6
                       min_child_weight=4, #default=1
                       subsample=1,
                       colsample_bytree=1,
                       nround=500)

XGboost_prediction<-predict(model_XGboost,newdata=house.data.gb.testing[,-1], type="response")
XGboost_prediction[which(is.na(XGboost_prediction))] <- mean(XGboost_prediction,na.rm=T)

str(XGboost_prediction)

write.table(cbind(testing.x$Id, XGboost_prediction), file = "Predicted Prices XgBoost.csv", 
            row.names = F, col.names = c("Id","SalePrice"), sep = ",")

#============================================================


