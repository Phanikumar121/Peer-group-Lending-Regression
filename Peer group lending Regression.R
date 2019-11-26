
#Setting the working directory
setwd("C:\\Users\\PHANI KUMAR\\Desktop\\Machine Learning Projects\\1.PEER GROUP LENDING - REGRESSION")

#Importing required packages
library(caret)
library(car)
library(MASS)
library(Hmisc)
library(psych)
library(dplyr)
library(tidyr)
library(sqldf)

options(scipen = 999)

#Reading the data file

mydata <- read.csv("LoansData.csv")

#Understanding the data

describe(mydata)
str(mydata)
summary(mydata)

############################Data Preparation#########################

#Renaming the variables to avoid confusion

names(mydata) <- gsub(".","",names(mydata),fixed = T)

#Converting charecter variables into factor variables

mydata$InterestRate <- gsub("%","",mydata$InterestRate)

mydata$InterestRate <- as.numeric(mydata$InterestRate)

mydata$DebtToIncomeRatio <- gsub("%","",mydata$DebtToIncomeRatio)

mydata$DebtToIncomeRatio <- as.numeric(mydata$DebtToIncomeRatio)

#When ever a variable is given in the form of range it is better to take the average of the upper count
#and lower count of that range

mydata <- mydata %>% separate(FICORange,c("Ficolow","FicoHigh",sep = "-"))

mydata$Ficolow <- as.numeric(mydata$Ficolow)
mydata$FicoHigh <- as.numeric(mydata$FicoHigh)

mydata$fico_average <- (mydata$FicoHigh+mydata$Ficolow)/2

#Removing unnecessary columns
mydata$`-` <- NULL
mydata$FicoHigh <- NULL
mydata$Ficolow <- NULL

############################Descriptive Statistics#########################

#splitting the data into numeric and categorical variables to perform data cleaning

numerical_Vars <- names(mydata)[sapply(mydata,FUN = is.numeric)]

Categorical_Vars <- names(mydata)[sapply(mydata,FUN = is.factor)]

#User defined function to get descriptive stats

mystats_numeric <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}

#User defined function to get Descriptive stats for categorical variables

mystats_categorical =function(x){
  Var_Type = class(x)
  n = length(x)
  nmiss = sum(is.na(x))
  freq = list(table(x))
  prop = list(prop.table(table(x)))
  return(c(Var_Type=Var_Type,n = n, miss = nmiss,frequency= freq,proportion= prop))
} 

#Descriptive stats for continuous variables

descriptive_stats_numeric = t(apply(mydata[numerical_Vars], 2, FUN=mystats_numeric))

write.csv(descriptive_stats_numeric,"descriptive_stats_numeric.csv")

#Descriptive stats for categorical variables

descriptive_stats_categorical = t(apply(mydata[Categorical_Vars], 2, FUN=mystats_categorical))

write.csv(descriptive_stats_categorical,"descriptive_stats_categorical.csv")


#counting missing values

colSums(is.na(mydata))

replace_missing_cont <- function(x){
  x <- replace(x, is.na(x), mean(x, na.rm=TRUE))
}

replace_missing_cat <- function(x){
  x <- as.character(x)
  x[is.na(x)] <- names(which.max(table(x)))
  return(factor(x))
}

#missing value treatment for continuous variables

continuous_vars <- apply(data.frame(mydata[numerical_Vars]), 2,FUN = replace_missing_cont)
continuous_vars <- data.frame(continuous_vars)


categorical_vars <- apply(data.frame(mydata[Categorical_Vars]), 2,FUN = replace_missing_cat)
categorical_vars <- data.frame(categorical_vars)

colSums(is.na(continuous_vars))
colSums(is.na(categorical_vars))

#######################################Outlier Treatment##################################

#User defined function for Outlier Treatment

outlier_treat <- function(x){
  Uc1 = quantile(x, p= 0.99,na.rm = T)
  LC1 = quantile(x, p= 0.01,na.rm = T)
  x= ifelse(x>Uc1,Uc1,x)
  x= ifelse(x<LC1,LC1,x)
  return(x)
}

#Outlier treatment for continuous variables

continuous_vars <- data.frame(apply(continuous_vars,2,FUN = outlier_treat))

colSums(is.na(continuous_vars))
colSums(is.na(categorical_vars))

#Final data preparation
my_finaldata <- cbind(continuous_vars,categorical_vars)

######################################Linear Regression##############################

hist(my_finaldata$InterestRate)

boxplot(my_finaldata$InterestRate)

#Preparing correlation matrix

corr_matrix <- cor(continuous_vars,method = "pearson")
write.csv(corr_matrix,"corelation matrix.csv")

#Performing ANOVA for continuous variable Interest rate and the categorical variables

categorical_vars$InterestRate <- continuous_vars$InterestRate

ANOVA_1 <- aov(InterestRate~.,data = categorical_vars)

summary(ANOVA_1)

summary(stepAIC(ANOVA_1,direction = "both"))

#From the above anova results,Loan length,loanpurpose,homeownership are the significant variables.
#Creating dummy variables for the categorical variables

#Dummy variable for categorical variable LoanLength(n-1 dummies)

dv = caret::dummyVars(~LoanLength , data=my_finaldata)

dummy_cat = data.frame(predict(dv, my_finaldata))

my_finaldata <- cbind(my_finaldata, dummy_cat)
my_finaldata$LoanLength <- NULL
my_finaldata$LoanLength.36.months <- NULL

#Dummy variable for categorical variable LoanPurpose(n-1 dummies)

dv1 = caret::dummyVars(~LoanPurpose  , data=my_finaldata)

dummy_cat1 = data.frame(predict(dv1, my_finaldata))

my_finaldata <- cbind(my_finaldata, dummy_cat1)
my_finaldata$LoanPurpose <- NULL
my_finaldata$LoanPurpose.car <- NULL

#Dummy variable for categorical variable HomeOwnership(n-1 dummies)

dv2 = caret::dummyVars(~HomeOwnership  , data=my_finaldata)

dummy_cat2 = data.frame(predict(dv2, my_finaldata))

my_finaldata <- cbind(my_finaldata, dummy_cat2)
my_finaldata$HomeOwnership <- NULL
my_finaldata$HomeOwnership.MORTGAGE <- NULL

my_finaldata$State <- NULL
my_finaldata$EmploymentLength <- NULL

#############################Splitting the data########################################

#splitingdata for building linear regression model
#data is split in the ratio of 70% and 30%
#training - 70%
#testing - 30%

set.seed(1111)
training_split <- sample(1:nrow(my_finaldata), size = floor(0.70 * nrow(my_finaldata)))

training_sample <- my_finaldata[training_split,]

testing_sample <- my_finaldata[-training_split,]

#Building linear model

model2 <- lm(InterestRate~.,data = training_sample)

step_regression <- stepAIC(model2,direction = "both")

summary(step_regression)

vif(model2)      #Very high VIF for many variable so rerun the model with another set of variables

#After trying different combination of variables this model is the overall best fit model

model3 <- lm(InterestRate~AmountFundedByInvestors+OpenCREDITLines+InquiriesintheLast6Months+
               fico_average+LoanLength.60.months+LoanPurpose.other+LoanPurpose.small_business+
               HomeOwnership.OWN+HomeOwnership.NONE,data = training_sample)

step_regression2 <- stepAIC(model3,direction = "both")

summary(step_regression2)

vif(model3)

##########################Predictions on training and testing##################

#------------------------------------------------------
# Error Metrics - SSE and MSE and RMSE
#------------------------------------------------------
actual_interestRate_train <- training_sample$InterestRate
predicted_interestRate_train <- Pred_int_rate1

errors_train <- actual_interestRate_train - predicted_interestRate_train

errors_train_square <- errors_train ^ 2

SSE_train <- sum(errors_train_square)
MSE_train <- mean(errors_train_square)
RMSE_train <- sqrt(MSE_train)

actual_interestRate_test <- testing_sample$InterestRate
predicted_interestRate_test <- Pred_int_rate2

errors_test <- actual_interestRate_test - predicted_interestRate_test

errors_test_square <- errors_test ^ 2

SSE_test <- sum(errors_test_square)
MSE_test <- mean(errors_test_square)
RMSE_test <- sqrt(MSE_test)

# Mean absolute percentage error for training

Pred_int_rate1 = predict(model3,newdata = training_sample)

finaltraining_predictions <- cbind(training_sample,Pred_int_rate1)

absolute_pct_error_train <- abs(finaltraining_predictions$InterestRate-finaltraining_predictions$Pred_int_rate1)/finaltraining_predictions$InterestRate

MAPE_train <- round(mean(absolute_pct_error_train)*100,2)

# Mean absolute percentage error for testing

Pred_int_rate2 = predict(object = model3,newdata = testing_sample)

finaltesting_predictions <- cbind(testing_sample,Pred_int_rate2)

absolute_pct_error_test <- abs(finaltesting_predictions$InterestRate-finaltesting_predictions$Pred_int_rate2)/finaltesting_predictions$InterestRate

MAPE_test <- round(mean(absolute_pct_error_test)*100,2)

#Residuals are normally distributed
hist(model3$residuals,main = "Residuals for model 3")

#correlation

cor(x=dev1$InterestRate,y=dev1$Pred_int_rate)

cor(x=val1$InterestRate,y=val1$pred_int_rate)

###########################################Decile Analysis##########################

# Decile analysis for training data(-Inf and Inf as upper and lower bounds)

decile_limits <- quantile(finaltraining_predictions$Pred_int_rate1,probs = seq(0.1,0.9,by = 0.1))

finaltraining_predictions$decile <- findInterval(finaltraining_predictions$Pred_int_rate1,
                                                 c(-Inf,decile_limits, Inf))

training_decile_Analysis <- sqldf("select decile, count(decile) as count, avg(Pred_int_rate1) as avg_Pred_int_rate1,   
               avg(InterestRate) as avg_InterestRate
               from finaltraining_predictions
               group by decile
               order by decile desc")

write.csv(training_decile_Analysis,"Training decile analysis.csv")

# Decile analysis for testing data(-Inf and Inf as upper and lower bounds)

decile_limits_test <- quantile(finaltesting_predictions$Pred_int_rate2,probs = seq(0.1,0.9,by = 0.1))

finaltesting_predictions$decile <- findInterval(finaltesting_predictions$Pred_int_rate2,
                                                 c(-Inf,decile_limits, Inf))

training_decile_Analysis_test <- sqldf("select decile, count(decile) as count, avg(Pred_int_rate2) as avg_Pred_int_rate2,   
               avg(InterestRate) as avg_InterestRate
               from finaltesting_predictions
               group by decile
               order by decile desc")

write.csv(training_decile_Analysis_test,"Testing decile analysis.csv")

####################################End of Regression####################################