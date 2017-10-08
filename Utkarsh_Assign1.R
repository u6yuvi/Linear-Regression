# #The code below covers the assumption testing and evaluation of model performance :


#Business Problem
#A bank wants to understand how customer banking habits
#contribute to revenues and profitability.


#Analytical Problem
#Build a model that allows the bank to predict profitability for a given customer

# Data Preparation
#Univariate Analysi
#Bivariate Analysis
#Missing Value Treatment/Data Cleaning /Data Transformation
#Outlier Treatment
#Variable Transformation
#Variable Creation
#Model Building
#Model Evaluation

#Packages
library(xlsx)
library(ggplot2)
library(car)
library(caret)
library(corrplot)
library(caret)
library(caret)
library(DAAG)
options("scipen" =100,digits = 2)

#Loading data
dt <-read.xlsx("bank_revenue.xlsx",sheetName="bank_revenue")
#Backup
dt_backup <- dt
#Dimension of dataset
dim(dt)
#Looking at variables
str(dt)
#Summarizing the dataset
summary(dt)

#Data Preparation
#Converting numeric to factor for categorical variable
dt[,c(5:15)] <- data.frame(lapply(dt[,c(5:15)], function(x) as.factor(as.numeric(x))))
dt$Offer <- as.factor(dt$Offer)



#Univariate Analysis
summary(dt$Rev_Total)
boxplot(dt$Rev_Total)
hist(dt$Rev_Total,breaks= c(0:95))
ggplot(dt,aes(x=Rev_Total))+geom_histogram(bins =30,fill="Blue",col="Black")
# Comments:
#No missing Values
#Rev_Total value ranges from 0.010 to 94.100 with median 1.175
#Highly Right Skewed
#Too many outliers

#Bal_Total
summary(dt$Bal_Total)
boxplot(dt$Bal_Total)
ggplot(dt,aes(x=Bal_Total))+geom_histogram(bins =30,fill="Blue",col="Black")
#Comments
#No missing Values
#Bal_Total values ranges from 0.03 to 166461.00 with median 1117.96
#highly Right Skewed
#Too many outliers

#Offer
table(dt$Offer)
prop.table(table(dt$Offer))
#Comments:
#No of 0s:3357
#No of 1's:4063

#Age
summary(dt$AGE)
boxplot(dt$AGE)
hist(dt$AGE)
#Comments:
#No missing Values
#Value ranges from 0 to 98 with median 46
#Slightly Right skewed

#CHQ
table(dt$CHQ)
prop.table(table(dt$CHQ))
#Comments:
#No of 0s:3655
#No of 1's:3761

#CARD
table(dt$CARD)
prop.table(table(dt$CARD))
#Comments:
#No of 0s:3663
#No of 1's:3757

#SAV1
table(dt$SAV1)
prop.table(table(dt$SAV1))
#Comments:
#No of 0s:6029
#No of 1's:1391

#LOAN
table(dt$LOAN)
prop.table(table(dt$LOAN))
#Comments:
#No of 0s:6592
#No of 1's:828


#MORT
table(dt$MORT)
prop.table(table(dt$MORT))
#Comments:
#No of 0s:4670
#No of 1's:

#INSUR
table(dt$INSUR)
prop.table(table(dt$INSUR))
#Comments:
#No of 0s:5170
#No of 1's:2250

#PENS
table(dt$PENS)
prop.table(table(dt$PENS))
#Comments:
#No of 0s:3560
#No of 1's:3860

#CHECK
table(dt$Check)
prop.table(table(dt$INSUR))
#Comments:
#No of 0s:1644
#No of 1's:5776

#CD
table(dt$CD)
prop.table(table(dt$CD))
#Comments:
#No of 0s:6592
#No of 1's:828


#MM
table(dt$MM)
prop.table(table(dt$MM))
#Comments:
#No of 0s:5170
#No of 1's:2250


#Savings
table(dt$Savings)
prop.table(table(dt$Savings))
#Comments:
#No of 0s:5170
#No of 1's:2250


#Account_Age
summary(dt$AccountAge)
boxplot(dt$AccountAge)
hist(dt$AccountAge)

#Comments:
#Right Skewed
#Outliers present

#Bivariate Analysis

#Continuos vs Continuos

#Rev_Total vs Bal_Total

ggplot(dt,aes(Bal_Total))+geom_histogram()
ggplot(dt,aes(Bal_Total,Rev_Total))+geom_point()

#Applying log transformation
ggplot(dt,aes(log(Bal_Total),log(Rev_Total)))+geom_point()


#Rev_Total vs AccountAge
ggplot(dt,aes(AccountAge,Rev_Total))+geom_point()
ggplot(dt,aes(log(AccountAge),log(Rev_Total)))+geom_point()


#Continuos vs Categorical

#As Rev_Total is higly skewed,ignoring the values above 95 percentile for visualization
quantile(dt$Rev_Total[dt$Offer=="1"],probs = seq(0,1,.05))
quantile(dt$Rev_Total[dt$Offer=="0"],probs = seq(0,1,.05))

#Offer
#Histogram
ggplot(dt[dt$Rev_Total<8,],aes(log(Rev_Total)))+geom_histogram(aes(fill=Offer),bins = 500)+facet_grid(Offer~.)

#Boxplot
ggplot(dt[dt$Rev_Total<8,],aes(Offer,Rev_Total))+geom_boxplot(aes(fill=Offer))
#Applying log
ggplot(dt[dt$Rev_Total<8,],aes(Offer,log(Rev_Total)))+geom_boxplot(aes(fill=Offer))
#Outlier present.

#Rev_Total based on CHQ

quantile(dt$Rev_Total[dt$CHQ=="1"],probs = seq(0,1,.05))
quantile(dt$Rev_Total[dt$CHQ=="0"],probs = seq(0,1,.05))

ggplot(dt[dt$Rev_Total<8,],aes(Rev_Total))+geom_histogram(aes(fill=CHQ),bins = 500)+facet_grid(CHQ~.)

#Applying log
ggplot(dt[dt$Rev_Total<8,],aes(log(Rev_Total)))+geom_histogram(aes(fill=CHQ),bins = 500)+facet_grid(CHQ~.)
#Boxplot
ggplot(dt[dt$Rev_Total<8,],aes(CHQ,Rev_Total))+geom_boxplot(aes(fill=CHQ))
#Applying log
ggplot(dt[dt$Rev_Total<8,],aes(CHQ,log(Rev_Total)))+geom_boxplot(aes(fill=CHQ))

#T test
t.test(dt$Rev_Total[dt$CHQ=="1"],dt$Rev_Total[dt$CHQ=="0"],alternative = "two.sided",paired = F)
#Comments:
#There is a significant difference in the Rev_Total between active Debit Card account and non active
#Debit Card account


#Rev_Total based on CARD
quantile(dt$Rev_Total[dt$CARD=="1"],probs = seq(0,1,.05))
quantile(dt$Rev_Total[dt$CARD=="0"],probs = seq(0,1,.05))

ggplot(dt[dt$Rev_Total<8,],aes(Rev_Total))+geom_histogram(aes(fill=CARD),bins = 500)+facet_grid(CARD~.)
ggplot(dt[dt$Rev_Total<8,],aes(CARD,Rev_Total))+geom_boxplot(aes(fill=CARD))

#T test
t.test(dt$Rev_Total[dt$CARD=="1"],dt$Rev_Total[dt$CARD=="0"],alternative = "two.sided",paired = F)
#Comments:
#There is a significant difference in the Rev_Total between active Credit Card account and non active
#Credit Card account

#Rev_Total based on SAV1
quantile(dt$Rev_Total[dt$SAV1=="1"],probs = seq(0,1,.05))
quantile(dt$Rev_Total[dt$SAV1=="0"],probs = seq(0,1,.05))

ggplot(dt[dt$Rev_Total<8,],aes(Rev_Total))+geom_histogram(aes(fill=SAV1),bins = 500)+facet_grid(SAV1~.)
ggplot(dt[dt$Rev_Total<8,],aes(SAV1,Rev_Total))+geom_boxplot(aes(fill=SAV1))

#T test
t.test(dt$Rev_Total[dt$SAV1=="1"],dt$Rev_Total[dt$SAV1=="0"],alternative = "two.sided",paired = F)
#Comments:
#There is a NO significant difference in the Rev_Total  between people with higher saving account activity
#and low or zero  saving account activity


#Rev_Total based on LOAN
quantile(dt$Rev_Total[dt$LOAN=="1"],probs = seq(0,1,.05))
quantile(dt$Rev_Total[dt$LOAN=="0"],probs = seq(0,1,.05))

ggplot(dt[dt$Rev_Total<8,],aes(Rev_Total))+geom_histogram(aes(fill=LOAN),bins = 500)+facet_grid(LOAN~.)
ggplot(dt[dt$Rev_Total<8,],aes(LOAN,Rev_Total))+geom_boxplot(aes(fill=LOAN))

#T test
t.test(dt$Rev_Total[dt$LOAN=="1"],dt$Rev_Total[dt$LOAN=="0"],alternative = "two.sided",paired = F)
#Comments:
#There is a  significant difference in the Rev_Total  between people with higher personal account activity
#and low or zero  personal account activity

#Rev_Total based on MORT
quantile(dt$Rev_Total[dt$MORT=="1"],probs = seq(0,1,.05))
quantile(dt$Rev_Total[dt$MORT=="0"],probs = seq(0,1,.05))

ggplot(dt[dt$Rev_Total<8,],aes(Rev_Total))+geom_histogram(aes(fill=MORT),bins = 500)+facet_grid(MORT~.)
ggplot(dt[dt$Rev_Total<8,],aes(MORT,Rev_Total))+geom_boxplot(aes(fill=MORT))

#T test
t.test(dt$Rev_Total[dt$MORT=="1"],dt$Rev_Total[dt$MORT=="0"],alternative = "two.sided",paired = F)
#Comments:
#Significance Level:.001
#There is a NO significant difference in the Rev_Total  between people with higher saving account activity
#and low or zero  saving account activity

#Rev_Total based on INSUR
quantile(dt$Rev_Total[dt$INSUR=="1"],probs = seq(0,1,.05))
quantile(dt$Rev_Total[dt$INSUR=="0"],probs = seq(0,1,.05))

ggplot(dt[dt$Rev_Total<8,],aes(Rev_Total))+geom_histogram(aes(fill=INSUR),bins = 500)+facet_grid(INSUR~.)
ggplot(dt[dt$Rev_Total<8,],aes(INSUR,Rev_Total))+geom_boxplot(aes(fill=INSUR))


#Rev_Total based on PENS
quantile(dt$Rev_Total[dt$PENS=="1"],probs = seq(0,1,.05))
quantile(dt$Rev_Total[dt$PENS=="0"],probs = seq(0,1,.05))

ggplot(dt[dt$Rev_Total<8,],aes(Rev_Total))+geom_histogram(aes(fill=PENS),bins = 500)+facet_grid(PENS~.)
ggplot(dt[dt$Rev_Total<8,],aes(PENS,Rev_Total))+geom_boxplot(aes(fill=PENS))

#Rev_Total based on Check
quantile(dt$Rev_Total[dt$Check=="1"],probs = seq(0,1,.05))
quantile(dt$Rev_Total[dt$Check=="0"],probs = seq(0,1,.05))

ggplot(dt[dt$Rev_Total<8,],aes(Rev_Total))+geom_histogram(aes(fill=Check),bins = 500)+facet_grid(Check~.)
ggplot(dt[dt$Rev_Total<8,],aes(Check,Rev_Total))+geom_boxplot(aes(fill=Check))

#Rev_Total based on CD
quantile(dt$Rev_Total[dt$CD=="1"],probs = seq(0,1,.05))
quantile(dt$Rev_Total[dt$CD=="0"],probs = seq(0,1,.05))

ggplot(dt[dt$Rev_Total<8,],aes(Rev_Total))+geom_histogram(aes(fill=CD),bins = 500)+facet_grid(CD~.)
ggplot(dt[dt$Rev_Total<8,],aes(CD,Rev_Total))+geom_boxplot(aes(fill=CD))


#Rev_Total based on MM
quantile(dt$Rev_Total[dt$MM=="1"],probs = seq(0,1,.05))
quantile(dt$Rev_Total[dt$MM=="0"],probs = seq(0,1,.05))

ggplot(dt[dt$Rev_Total<8,],aes(Rev_Total))+geom_histogram(aes(fill=MM),bins = 500)+facet_grid(MM~.)
ggplot(dt[dt$Rev_Total<8,],aes(MM,Rev_Total))+geom_boxplot(aes(fill=MM))

#Rev_Total based on Savings
quantile(dt$Rev_Total[dt$Savings=="1"],probs = seq(0,1,.05))
quantile(dt$Rev_Total[dt$Savings=="0"],probs = seq(0,1,.05))

ggplot(dt[dt$Rev_Total<8,],aes(Rev_Total))+geom_histogram(aes(fill=Savings),bins = 500)+facet_grid(Savings~.)
ggplot(dt[dt$Rev_Total<8,],aes(Savings,Rev_Total))+geom_boxplot(aes(fill=Savings))

dt_train <- dt_backup



#Transformation

#Rev_Total
hist(dt$Rev_Total)
#Log Transformation
dt_train$Rev_Total <- log(dt_train$Rev_Total) 
hist(dt_train$Rev_Total)
summary(dt_train$Rev_Total)
quantile(dt_train$Rev_Total,seq(0,1,.1))

#After log transformation Rev_Total looks to be normally distributed

#Bal_Total
hist(dt$Bal_Total)
summary(dt$Bal_Total)
#Log Transformation
dt_train$Bal_Total <- log(dt_train$Bal_Total)
summary(dt_train$Bal_Total)
hist(dt_train$Bal_Total)
boxplot(dt_train$Bal_Total)
quantile(dt_train$Bal_Total,seq(0,1,.1))
ggplot(dt_train,aes(x=Bal_Total,y=Rev_Total))+geom_point()
#Correlation
cor(dt_train$Rev_Total,dt_train$Bal_Total)


#Age
summary(dt$AGE)
boxplot(dt$AGE)
hist(dt$AGE)
cor(dt_train$Rev_Total,dt_train$AGE)
#Almost no linear relationship

#Account_Age
summary(dt_train$AccountAge)
quantile(dt_train$AccountAge,seq(0,1,.10))
hist(dt_train$AccountAge)
boxplot(dt$AccountAge)
dt_train$AccountAge <- log1p(dt_train$AccountAge)
summary(dt_train$AccountAge)
hist(dt_train$AccountAge)
boxplot(dt_train$AccountAge)
ggplot(dt_train,aes(x=AccountAge,y=Rev_Total))+geom_point()
cor(dt_train$Rev_Total,dt_train$AccountAge)
#Almost no linear relationship

#Identifying and Correcting Collinearity

#Dropping dependent variable for calculating Multicollinearity
dt_subset = subset(dt, select = -c(Rev_Total))

#Identifying numeric variables
numericData <- dt_subset[sapply(dt_subset, is.numeric)]

#Calculating Correlation
descrCor <- cor(numericData)

# Print correlation matrix and look at max correlation
print(descrCor)

# Visualize Correlation Matrix
corrplot(descrCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))

#Correlation between the variables seems to be under the threshold limit.Will continue without
#dropping any variables


#Regrression model for Checking assumptions
fit = lm(Rev_Total~., data=dt_train)

#Check Model Performance
summary(fit)

#Removing perfectly correlated variables 
fit = lm(Rev_Total~.-CD-MM-Savings, data=dt_train)
summary(fit)

#Extracting Coefficients
summary(fit)$coeff
anova(fit)

par(mfrow=c(2,2))
plot(fit)

par(mfrow=c(1,1))

#Checking Multicollinearity
vif(fit)

#Removing CARD variable
fit = lm(Rev_Total~.-CD-MM-Savings-CARD, data=dt_train)
summary(fit)

par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

#Checking Multicollinearity
vif(fit)

#Removing Check variable
fit = lm(Rev_Total~.-CD-MM-Savings-CARD-Check, data=dt_train)
summary(fit)
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))
#Checking Multicollinearity
vif(fit)

#Removing MORT variable
fit = lm(Rev_Total~.-CD-MM-Savings-CARD-Check-MORT, data=dt_train)
summary(fit)

par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

#Checking Multicollinearity
vif(fit)



#Splitting data into training and testing data
dt1 = sort(sample(nrow(dt_train), nrow(dt_train)*.7))
train<-dt_train[dt1,]
test<-dt_train[-dt1,]


#Building regression model
model1 <- lm(Rev_Total~.-CD-MM-Savings-CARD-Check-MORT,data = train)



#Stepwise Selection based on AIC
library(MASS)
step <- stepAIC(model1, direction="both")
summary(step)
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))
vif(step)


#Backward Selection based on AIC
back_step_model <- stepAIC(model1, direction="backward")
summary(back_step_model)
par(mfrow=c(2,2))
plot(back_step_model)
par(mfrow=c(1,1))
vif(back_step_model)


#Forward Selection based on AIC
Forward_step_model <- stepAIC(model1, direction="forward")
summary(Forward_step_model)
vif(Forward_step_model)


#Stepwise Selection with BIC
n = dim(train)[1]
stepBIC_model = stepAIC(model1,k=log(n))
summary(stepBIC_model)
vif(stepBIC_model)

# Influential Observations
# added variable plots 
av.plots(stepBIC_model)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(train)-length(stepBIC_model$coefficients)-2)) 
plot(stepBIC_model, which=4, cook.levels=cutoff)

# Influence Plot 
#influencePlot(stepBIC_model,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
w <- abs(rstudent(stepBIC_model)) < 3 & abs(cooks.distance(stepBIC_model)) < 4/nrow(stepBIC_model$model)
stepBIC_model_updated <- update(stepBIC_model, weights=as.numeric(w))
summary(stepBIC_model_updated)

plot(stepBIC_model_updated, which=4, cook.levels=cutoff)
av.plots(stepBIC_model_updated)

#Autocorrelation Test
durbinWatsonTest(stepBIC_model_updated)

#As p value greater than .05 ,we are good to go


#Testing for heteroscedasticity (Should be > 0.05)
ncvTest(stepBIC_model_updated)

#As p value is greater than .05,we are good to go

#Outliers – Bonferonni test
outlierTest(stepBIC_model_updated)

#See Residuals
resid = residuals(stepBIC_model_updated)

#Relative Importance
install.packages("relaimpo")
library(relaimpo)
calc.relimp(stepBIC_model_updated)


#Prediction on test data
pred_updated = predict(stepBIC_model_updated,test)

#Calculating RMSE
rmse = sqrt(mean((test$Rev_Total - pred_updated)^2))
print(rmse)


#Calculating Rsquared manually
y = test[,c("Rev_Total")]
R.squared = 1 - sum((y-pred_updated)^2)/sum((y-mean(y))^2)
print(R.squared)


#Calculating Adj. Rsquared manually
n = dim(test)[1]
p = dim(summary(step)$coeff)[1] - 1
adj.r.squared = 1 - (1 - R.squared) * ((n - 1)/(n-p-1))
print(adj.r.squared)

#Actual vs Predicted over quantiles
Test_Ranked_group<- quantile(test$Rev_Total,seq(0,1,.1))
Predicted_Rank_group <- quantile(pred_updated,seq(0,1,.1))
TestvsPred <- cbind(Test_Ranked_group,Predicted_Rank_group)
TestvsPred