#reading in our dataset as a Data Frame
library(readr)
loanData = read_csv("loandata.csv")

library (ggplot2)


"
The below code is the initial exploration of the structure of the dataset
"

install.packages('ggplot2')
library(ggplot2)
install.packages('DataExplorer')
library(DataExplorer)

str(loanData)
plot_str(loanData)
plot_missing(loanData)


#Fill all 'NA' values with 0
loanData[is.na(loanData)] = 0

#Remove spacing in colNames
library(janitor)
loanData = clean_names(loanData)
head(loanData)


#Checking formating issues with categorial variables

unique(loanData$finance_product)
unique(loanData$industry)

#Declearing categorical variables as a factor

loanData$finance_product=factor(loanData$finance_product)
loanData$industry=factor(loanData$industry)
loanData$default=factor(loanData$default)
loanData$us_deal=factor(loanData$us_deal)
loanData$homeowner=factor(loanData$homeowner)
loanData$pac_required=factor(loanData$pac_required)

str(loanData)

# EDA & Viz----

# Uni-variate----


# Finance product------------------------------------------------------

#histogram finance product
ggplot(data = loanData, aes(finance_product)) +
  geom_histogram(stat = "count", aes(fill = finance_product)) +
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 1),size=5)

#Finance product by default
ggplot(loanData, aes(x = finance_product, group=default)) +
  geom_bar(position = "stack", aes(fill = default))+
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 1),size=4)


# Industry---------------------------------------------------------------------
ggplot(data = loanData, aes(industry)) +
  geom_histogram(stat = "count", aes(fill = industry)) +
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 1),size=5)

#Industry by default
ggplot(loanData, aes(x = industry, group=default)) +
  geom_bar(position = "stack", aes(fill = default))+
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 1),size=4)

#original balance---------------------------------------------------------------------

summary(loanData$original_balance)

library(psych)

#statistic summary by default
describeBy(loanData$original_balance, group = loanData$default)

#boxplot group by default
ggplot(loanData, aes(x=loanData$original_balance, fill=default)) + 
  geom_boxplot() +
  facet_wrap(~default)+
  xlab("Original Balance")

#Histogram

hist(loanData$original_balance, xlab = "Original Balance", main="Histogram Original Balance", col = "blue")

#Residual Amount-----------------------------------------------------------------------

summary(loanData$residual_amt)

#statistic summary by default
describeBy(loanData$residual_amt, group = loanData$default)

#boxplot group by default
ggplot(loanData, aes(x=loanData$residual_amt, fill=default)) + 
  geom_boxplot() +
  facet_wrap(~default)+
  xlab("Residual Amount")

#Histogram

hist(loanData$residual_amt, xlab = "Residual Amount", main="Histogram Residual Amount", col = "blue")


#payment---------------------------------------------------------------------------------------

summary(loanData$payment)

#statistic summary by default
describeBy(loanData$payment, group = loanData$default)

#boxplot group by default
ggplot(loanData, aes(x=loanData$payment, fill=default)) + 
  geom_boxplot() +
  facet_wrap(~default)+
  xlab("Payment")

#Histogram

hist(loanData$payment, xlab = "Payment", main="Histogram Payment", col = "blue")

#Term-----------------------------------------------------------------------------------------------

summary(loanData$term)

#statistic summary by default
describeBy(loanData$term, group = loanData$default)

#boxplot group by default
ggplot(loanData, aes(x=loanData$term, fill=default)) + 
  geom_boxplot() +
  facet_wrap(~default)+
  xlab("Term")

#Histogram

hist(loanData$term, xlab = "Term", main="Histogram Term", col = "blue")

#Rate-------------------------------------------------------------------------------------------------
summary(loanData$rate)

#statistic summary by default
describeBy(loanData$rate, group = loanData$default)

#boxplot group by default
ggplot(loanData, aes(x=loanData$rate, fill=default)) + 
  geom_boxplot() +
  facet_wrap(~default)+
  xlab("Rate")

#Histogram

hist(loanData$rate, xlab = "Rate", main="Histogram Rate", col = "blue")

#Us_deal -----------------------------------------------------------------------------------------------

ggplot(loanData, aes(x = us_deal, group=default)) +
  geom_bar(position = "stack", aes(fill = default))+
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 1),size=4)

#pac_requiered---------------------------------------------------------------------------------------------

ggplot(loanData, aes(x = pac_required, group=default)) +
  geom_bar(position = "stack", aes(fill = default))+
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 1),size=4)

#pg_age----------------------------------------------------------------------------------------------------

summary(loanData$pg_age)

#statistic summary by default
describeBy(loanData$pg_age, group = loanData$default)

#boxplot group by default
ggplot(loanData, aes(x=loanData$pg_age, fill=default)) + 
  geom_boxplot() +
  facet_wrap(~default)+
  xlab("Age")

#Histogram

hist(loanData$pg_age, xlab = "Age", main="Histogram Age", col = "blue")


#Fico Score--------------------------------------------------------------------------------------------------


summary(loanData$fico_score)

#statistic summary by default
describeBy(loanData$fico_score, group = loanData$default)

#boxplot group by default
ggplot(loanData, aes(x=loanData$fico_score, fill=default)) + 
  geom_boxplot() +
  facet_wrap(~default)+
  xlab("Fico Score")

#Histogram

hist(loanData$fico_score, xlab = "Fico Score", main="Histogram Fico Score", col = "blue")

#Homeowner---------------------------------------------------------------------------------------------

ggplot(loanData, aes(x = homeowner, group=default)) +
  geom_bar(position = "stack", aes(fill = default))+
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 1),size=4)

#pg_network---------------------------------------------------------------------------------------------


summary(loanData$pg_net_worth)

#statistic summary by default
describeBy(loanData$pg_net_worth, group = loanData$default)

#boxplot group by default
ggplot(loanData, aes(x=loanData$pg_net_worth, fill=default)) + 
  geom_boxplot()+ 
  facet_wrap(~default)+
  xlab("Net Worth")

#Histogram

hist(loanData$pg_net_worth, xlab = "Net Worth", main="Histogram Net Worth", col = "blue")


#DTI----------------------------------------------------------------------------------------------

summary(loanData$dti)

#statistic summary by default
describeBy(loanData$dti, group = loanData$default)

#boxplot group by default
ggplot(loanData, aes(x=loanData$dti, fill=default)) + 
  geom_boxplot()+ 
  facet_wrap(~default)+
  xlab("DTI")

#Histogram

hist(loanData$dti, xlab = "DTI", main="Histogram DTI",  col = "blue")

#30_daysdelinq----------------------------------------------------------------------------

summary(loanData$x30day_delinq)

#statistic summary by default
describeBy(loanData$x30day_delinq, group = loanData$default)

#boxplot group by default
ggplot(loanData, aes(x=loanData$x30day_delinq, fill=default)) + 
  geom_boxplot()+ 
  facet_wrap(~default)+
  xlab("30 Days")

#Histogram

hist(loanData$x30day_delinq, xlab = "30 Days", main="Histogram 30 Days",  col = "blue")

#60_daysdelinq----------------------------------------------------------------------------

summary(loanData$x60day_delinq)

#statistic summary by default
describeBy(loanData$x60day_delinq, group = loanData$default)

#boxplot group by default
ggplot(loanData, aes(x=loanData$x60day_delinq, fill=default)) + 
  geom_boxplot()+ 
  facet_wrap(~default)+
  xlab("60 Days")

#Histogram

hist(loanData$x60day_delinq, xlab = "60 Days", main="Histogram 60 Days",  col = "blue")

#90_daysdelinq----------------------------------------------------------------------------

summary(loanData$x90day_delinq)

#statistic summary by default
describeBy(loanData$x90day_delinq, group = loanData$default)

#boxplot group by default
ggplot(loanData, aes(x=loanData$x90day_delinq, fill=default)) + 
  geom_boxplot()+ 
  facet_wrap(~default)+
  xlab("90 Days")

#Histogram

hist(loanData$x60day_delinq, xlab = "90 Days", main="Histogram 90 Days",  col = "blue")


# Default-----------------------------------------------------------------------------------

ggplot(data = loanData, aes(default)) +
  geom_histogram(stat = "count", aes(fill = default)) +
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 1),size=5)
  


# Bi-variate----

# FICO score vs Rate, by Default
ggplot(loanData, aes(x = fico_score, y = rate, colour = default)) +
  geom_point() 

# Pg_networth vs dti, by Default

ggplot(loanData, aes(x = dti, y = pg_net_worth, colour = default)) +
  geom_point() 
  
#rate vs dti, by default

ggplot(loanData, aes(x = dti, y = rate, colour = default)) +
  geom_point() 


##Linear Regression-----------------------------------------------------------------------

#test for linearity of x and y variables
pairs(~original_balance + residual_amt + payment
      + term + rate + us_deal + pac_required + pg_age + fico_score + homeowner + pg_net_worth
      + dti + default, data=loanData)

loanData$finance_product <- factor(loanData$finance_product)
loanData$industry <- factor(loanData$industry)

linmodel = lm(default ~ finance_product + industry + original_balance + residual_amt + payment
              + term + rate + us_deal + pac_required + pg_age + fico_score + homeowner + pg_net_worth
              + dti + x30day_delinq + x60day_delinq + x90day_delinq, data = loanData)
summary(linmodel)


#test for independence of errors
library(car)
durbinWatsonTest(linmodel)
plot(loanData$transaction_number, linmodel$residuals)

#test for normality of errors
qqnorm(linmodel$residuals)

#test for constant variance of errors
plot(linmodel$fitted.values, linmodel$residuals)
plot(loanData$default, linmodel$residuals)

#test for no multicollinearity
vif(linmodel)

# Logistic Regression----
head(loanData)
moodel1 <- glm(default ~ finance_product + industry + rate, data = loanData, family = "binomial")
summary(model1)

install.packages('logistf')
library(logistf)
model2 = logistf(default ~ finance_product, data=loanData)
summary(model2)

model3 = logistf(default ~ original_balance + residual_amt + payment + rate, data=loanData)
summary(model3)

model6 = logistf(default ~ original_balance + residual_amt + payment + pg_net_worth, data=loanData)
summary(model6)

#test for independence of errors
library(car)
plot(loanData$transaction_number, model6$residuals)

#test for linearity of independent variables and log odds
odds = model6$predict
log_odds = log(odds)
plot(loanData$original_balance, log_odds)
plot(loanData$residual_amt, log_odds)

#ln transformation
plot(log(loanData$payment),log_odds)
plot(log(loanData$pg_net_worth), log_odds)



##KNN--------------------------------------------------------------------------------

## Taking back-up of the input file.

loanData2=loanData

##Normalize the numeric variables.

num.vars <- sapply(loanData2, is.numeric)
loanData2[num.vars] <- lapply(loanData2[num.vars], scale)

#Variables selected to run the KNN

myvars <- c("dti", "fico_score", "homeowner","pg_net_worth")
loanData2.subset=loanData2[myvars]

#Model will be trained with 50% of the data and tested with the other 50%

set.seed(123) 
test <- 1:3700
test.loanData2 <- loanData2.subset[-test,]
train.loanData2 <- loanData2.subset[test,]

test.def <- loanData2$default[-test]
train.def <- loanData2$default[test]

#K values as 1, 5, and 10 will be used to see how they perform in terms of correct proportion of classification.

library(class)

knn.1 <-  knn(train.loanData2, test.loanData2, train.def, k=1)
knn.5 <-  knn(train.loanData2, test.loanData2, train.def, k=5)
knn.10 <- knn(train.loanData2, test.loanData2, train.def, k=10)


#Evaluates the model performance

library(gmodels)

CrossTable(x=test.def,y=knn.1,prop.chisq = FALSE)

CrossTable(x=test.def,y=knn.5,prop.chisq = FALSE)

CrossTable(x=test.def,y=knn.10,prop.chisq = FALSE)

#Model Validation (Cross Validation)
#This uses leave-one-out cross validation.

n=knn.cv(loanData2.subset,loanData2$default, k=5)

sum(loanData2$default == n)/7403

#KNN not including homeowner variable.-----------------------------------------------


myvars <- c("dti", "fico_score","pg_net_worth")
loanData2.subset=loanData2[myvars]

#Model will be trained with 50% of the data and tested with the other 50%

set.seed(123) 
test <- 1:3700
test.loanData2 <- loanData2.subset[-test,]
train.loanData2 <- loanData2.subset[test,]

test.def <- loanData2$default[-test]
train.def <- loanData2$default[test]

#K values as 1, 5, and 10 will be used to see how they perform in terms of correct proportion of classification.

library(class)

knn.1 <-  knn(train.loanData2, test.loanData2, train.def, k=1)
knn.5 <-  knn(train.loanData2, test.loanData2, train.def, k=5)
knn.10 <- knn(train.loanData2, test.loanData2, train.def, k=10)


#Evaluates the model performance

library(gmodels)

CrossTable(x=test.def,y=knn.1,prop.chisq = FALSE)

CrossTable(x=test.def,y=knn.5,prop.chisq = FALSE)

CrossTable(x=test.def,y=knn.10,prop.chisq = FALSE)

#Model Validation (Cross Validation)
#This uses leave-one-out cross validation.

n=knn.cv(loanData2.subset,loanData2$default, k=5)

sum(loanData2$default == n)/7403



##Random Forest-------------------------------------------------------------------------

# Removing transaction number & PAC required
selData = loanData [c(-1, -10)]
str(selData)

#Scaling only numeric data (optional)
library(dplyr)

selData = selData %>%
  mutate_if(is.numeric, scale)

str(loanData)

# Random Forest ----
install.packages("randomForest")
library (randomForest)


# Data partition (Considering a 80-20 split)
set.seed (234)
indices = sample(2, nrow(selData), replace = TRUE, prob = c (0.8,0.2))
train = selData [indices == 1,]
test = selData [indices == 2,]

# Run randomforest
set.seed (456)
rf = randomForest(default ~ ., data = train)
print(rf)
attributes(rf)
rf$err.rate


# Prediction
library(caret)
p1 = predict(rf, train)
head(p1)
confusionMatrix(p1, train$default)

p2 = predict (rf, test)
head(p2)
confusionMatrix(p2, test$default)

# Plot error tate
plot (rf)

# Tuning RF
tuneRF(train[,-17], train[,17], stepFactor = 1, plot = TRUE, ntreeTry = 100, trace = TRUE)

# Hist
hist(treesize(rf),
     main = "Number of Nodes for Trees",
     col = "blue")

# Variable Importance
varImpPlot(rf)
importance (rf)
varUsed(rf)

MDSplot(rf, train$default)
rf$proximity

