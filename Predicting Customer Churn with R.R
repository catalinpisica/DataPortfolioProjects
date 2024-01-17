# -------- Predicting Customer Churn in the Dutch Energy Sector --------

# Author: Catalin Pisica
# Email: catalinpisica5@gmail.com
# Phone Number: +31 687 627 954
# LinkedIn: https://www.linkedin.com/in/catalinpisica5
# Portfolio: https://catalinpisica.github.io/

# Project Information

#The primary concern of the Dutch energy supplier is the fact that there is a churn factor that is
#quite prominent in its relationship with its customers. What is more, competition in the energy
#market itself is quite rigid, consisting of more people who switch from one provider to another,
#and fewer people entering with an influx of new customers. This project aims to build several 
#classification models and evaluate them through different evaluation metrics.
#In this project, 7 types of modeling techniques are implemented: Logistic Regression, Step-Wise
#Logistic Regression, Support Vector Machines, Random Forest, Boosting, Bagging and CART.
#All types of models were first estimated on an Estimation Dataset which consisted of 75% of all
#observations and then validated on a Validation Dataset (25%).

#The validation process is a process through which the quality of a model is evaluated. For all
#models, this process consisted of calculating the Hit-Rate, Top Decile Lift and GINI Coefficient.
#The values for the Hit-Rate should be higher than 0.5, Top Decile Lift is good to have values
#close to 2 and the GINI Coefficient to 1 as well. 

# Project Outcome
#Based on these coefficients, the best model that
#can be used for churn prediction is the Boosting technique, that obtained a 0.7643414 Hit-Rate,
#1.920015 Top Decile Lift and 0.6901542 GINI Coefficient which means that the model is almost
#twice as good than a random selection. 



# Install and load required packages
required_packages <- c("ggplot2", "dplyr", "tidyr", "visdat", "stringr", "ISOweek", "moments", "outliers", "ggpubr", "corrplot", "Hmisc", "ROCR", "readr", "MASS", "rpart", "partykit", "ipred", "caret", "gbm", "randomForest", "e1071")

# Install missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages) > 0) install.packages(missing_packages)

# Load required packages
lapply(required_packages, library, character.only = TRUE)

# Clean the workspace
rm(list = ls())

# First steps ----
## Import the data ----
data <- read.csv(".../data.csv")

## Get an overview of the data ----
head(data)
str(data)
summary(data)

# Convert data types to factors and numeric
data <- data %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.numeric, as.numeric)

# Data cleaning ----
## Check for duplicates ----
sum(duplicated(data)) # there are no duplicates

## Check for missings ----
sum(is.na(data)) # no missing in the data

## Check for outliers ---
#Check for Age outliers

boxplot(data$Age)
#represent 2% so can be just removed
#further check by using IQR
55 + 1.5*IQR(data$Age) 

data = data[data$Age != 17, ]

### Check for Income outliers

###Investigate the normality of Income
ggqqplot(data$Income) #normality is violated

### Investigate skewness of Income
skewness(data$Income) # 16.58 -> highly skewed


summary(data$Income) #3rd quartile value is 6302, mean is 5537 but max is 235350. Might be a case for outliers
plot(data$Income, ylab = "Income", main = "Income Scatterplot")
boxplot(data$Income, main="Income Boxplot")
hist(data$Income,
     xlab = "Income",
     main = "Histogram of Income",
     breaks = sqrt(nrow(data))
)
### The plots suggest that this variable contains quite a few outliers
### Run Grubbs's Test to check for Income outliers
Income_outlier <- grubbs.test(data$Income)
Income_outlier #p-value < 0.05 => highest value is an outlier


### Correct income values
data$Income <- ifelse(data$Income>30042,data$Income/12,data$Income)

### Check histogram after outlier correction
hist(data$Income,
     xlab = "Income",
     main = "Histogram of Income",
     breaks = sqrt(nrow(data))) #Histogram looks better
plot(data$Income, ylab = "Income", main = "Income Scatterplot")


### Check for Relation Length outliers. Result: no outliers

###Investigate the normality of Relation Length
ggqqplot(data$Relation_length) #normality is violated
summary(data$Relation_length)
plot(data$Relation_length, ylab = "Relation Length")
boxplot(data$Relation_length, main="Relation Length Boxplot")
hist(data$Relation_length,
     xlab = "Relation Length",
     main = "Histogram of Relation Length",
     breaks = sqrt(nrow(data))) 

### Run Grubbs's Test to check for Relation Length outliers
Relation_length_outlier <- grubbs.test(data$Relation_length)
Relation_length_outlier #p-value = 0.1738 => highest value is not an outlier


### START CHANNEL
#create dummy for start_channel (1 for online, 0 for phone)
data$Start_channel = ifelse(data$Start_channel == "Online",1,0)
sum(data$Start_channel == 1) #84% use an online start channel
start_channel = factor(c(rep("online channel", 16401), rep("phone", 3151)))
data.frame(start_channel)
cols =  hcl.colors(length(levels(start_channel)), "Fall")
PieChart(start_channel, data = data, hole = 0,fill = cols,labels_cex = 0.6)


### CONTRACT LENGTH
summary(data$Contract_length)
sum(data$Contract_length == 0) #7786 customers can leave without paying any fee
table(data$Contract_length)


### Check for Gas Usage Outliers
summary(data$Gas_usage)
plot(data$Gas_usage, ylab = "Gas Usage")
boxplot(data$Gas_usage, main="Gas Usage Boxplot")
hist(data$Gas_usage,
     xlab = "Gas Usage",
     main = "Histogram of Gas Usage",
     breaks = sqrt(nrow(data))) 

### The plots suggest that this variable contains quite a few outliers

### Run Grubbs's Test to check for Gas Usage outliers
Gas_Usage_outlier <- grubbs.test(data$Gas_usage)
Gas_Usage_outlier #p-value < 0.05 => highest value is an outlier

###Correct Gas usage values that are higher than 3209 by dividing by 10
data$Gas_usage <- ifelse(data$Gas_usage>3209, data$Gas_usage/10, data$Gas_usage)

### Check Gas Usage histogram after outlier correction
hist(data$Gas_usage,
     xlab = "Gas Usage",
     main = "Histogram of Gas Usage",
     breaks = sqrt(nrow(data))) #Histogram looks better



### Check for Electricity_Usage Outliers
summary(data$Electricity_usage)
plot(data$Electricity_usage, ylab = "Electricity Usage")
boxplot(data$Electricity_usage, main="Electricity Usage Boxplot")
hist(data$Electricity_usage,
     xlab = "Electricity Usage",
     main = "Histogram of Electricity Usage",
     breaks = sqrt(nrow(data))) 

### The plots suggest that this variable contains quite a few outliers

### Run Grubbs's Test to check for Gas Usage outliers
Electricity_Usage_outlier <- grubbs.test(data$Electricity_usage)
Electricity_Usage_outlier #p-value < 0.05 => highest value is an outlier

###Correct Electricity usage values that are higher than 4578 by dividing by 10
data$Electricity_usage <- ifelse(data$Electricity_usage>4578, data$Electricity_usage/10, data$Electricity_usage)

### Check Electricity Usage histogram after outlier correction
hist(data$Electricity_usage,
     xlab = "Electricity Usage",
     main = "Histogram of Electricity Usage",
     breaks = sqrt(nrow(data))) #Histogram looks better



### EXIT FEE
#create dummy exit_fee
data$exit_fee = ifelse(data$Contract_length == 0,0,1)




#Exploratory Analysis-----------------------------------------------------------------------------------------------------------------------------------------------
## Reporting
## Descriptive statistics for continuous variables
by(data, data$Churn, summary)
hist(data$Income, main="Histogram of Income of Customer", xlab='# Income')
hist(data$Relation_length, main="Histogram of Relationship length", xlab='# years customer')
hist(data$Gas_usage, main="Histogram of Gas Usage", xlab='# gas usage')
hist(data$Age, main="Histogram of Age", xlab='# gas usage')
hist(data$Gas_usage, main="Histogram of Gas Usage", xlab='# gas usage')
hist(data$Contract_length, main="Histogram of Contract Length", xlab='# gas usage')

## Descriptive statistics for categorical variables
gender_freq <- table(data$Churn, data$Gender)
prop.table(gender_freq)

exit_fee_freq<- table(data$Churn, data$exit_fee)
prop.table(exit_fee_freq)

start_channel_freq <- table(data$Churn, data$Start_channel)
prop.table(start_channel_freq)

email_list_freq <- table(data$Churn, data$Email_list)
prop.table(email_list_freq)  

Province_freq <- table(data$Churn, data$Province)
prop.table(Province_freq)

home_label_freq <- table(data$Churn, data$Home_label)
prop.table(home_label_freq)
  
## 1-to-1 relationships

#### Age:KPI categorical (churn), DRIVER numerical (age)
age_model = glm(Churn ~ Age, family = "binomial", data = data)
summary(age_model)
#p value < 0.05, churn and age are negatively related


### Income: KPI categorical(Churn), driver numerical => Simple Logistic Regression (k=2)
data %>%                               
  group_by(Churn) %>% 
  summarize(min = min(Income),
            q1 = quantile(Income, 0.25),
            median = median(Income),
            mean = mean(Income),
            q3 = quantile(Income, 0.75),
            max = max(Income),
            sd = sd(Income))

boxplot(Income ~ Churn, data = data,
        col = c("#FFE0B2", "#FFA726", "#F57C00"))


model1 <- glm(Churn ~ Income, family = "binomial", data = data)
summary(model1) #Estimate negative, p value < 0.05 => Churn and Income are negatively related

### Relation_Length: KPI categorical(Churn), driver numerical => Simple Logistic Regression (k=2)
data %>%                               
  group_by(Churn) %>% 
    summarize(min = min(Relation_length),
            q1 = quantile(Relation_length, 0.25),
            median = median(Relation_length),
            mean = mean(Relation_length),
            q3 = quantile(Relation_length, 0.75),
            max = max(Relation_length),
            sd = sd(Relation_length))

boxplot(Relation_length ~ Churn, data = data,
        col = c("#FFE0B2", "#FFA726", "#F57C00"))

model2 <- glm(Churn ~ Relation_length, family = "binomial", data = data)
summary(model2) #Estimate negative, p value < 0.05 => Churn and Relation Length are negatively related

### Gas Usage: KPI categorical(Churn), driver numerical => Simple Logistic Regression (k=2)
data %>%                               
  group_by(Churn) %>% 
  summarize(min = min(Gas_usage),
            q1 = quantile(Gas_usage, 0.25),
            median = median(Gas_usage),
            mean = mean(Gas_usage),
            q3 = quantile(Gas_usage, 0.75),
            max = max(Gas_usage),
            sd = sd(Gas_usage))

boxplot(Gas_usage ~ Churn, data = data,
        col = c("#FFE0B2", "#FFA726", "#F57C00"))

model3 <- glm(Churn ~ Gas_usage, family = "binomial", data = data)
summary(model3) #Estimate positive, p value < 0.05 => Churn and Gas Usage are positively related

###Start Channel: KPI categorical (churn), DRIVER categorical (start channel)
chisq.test(data$Churn, data$Start_channel, correct=FALSE)
#p value < 0.05, X-squared = 304.19

### Email List: 2 categorical variables => Chi-Square Test


chisq.test(data$Churn, data$Email_list, correct=FALSE) #Results: variables are siggnificantly related, p-value <0.05, X-squared = 356.3

### Contract length: KPI categorical (churn), DRIVER numerical (contract length)
contract_length_model = glm(Churn ~ Contract_length, family = "binomial", data = data)
summary(contract_length_model)
#p value < 0.05, churn and contract length are negatively related

###Exit fee: KPI categorical (churn), DRIVER categorical (exit fee)
chisq.test(data$Churn, data$exit_fee, correct=FALSE)
#p value < 0.05, X-squared = 2482.1

#1-to-1 relationship 
##KPI categorical (churn), DRIVER numerical (electricity usage)
electricity_usage_model= glm(Churn ~ Electricity_usage, family = "binomial", data = data)
summary(electricity_usage_model)
#p value < 0.05, churn and electricity usage are positively related


### Calculate correlation between variables
data_cor<-data[,c(3,4,5,11,12,14,15)]
res2 <- rcorr(as.matrix(data_cor))
res2
corrplot(res2, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)



# MODELING -----------------------------------------------------------------------------------------------------------------------------------------------

## Split the dataset into a 75% Estimation and a 25% Validation Sample
set.seed(1234)
### Create estimation sample and remove columns Customer Id, Contract Length
dadata = subset(data, select = -c(Customer_ID,Contract_length) )
ta$estimation_sample <-rbinom(nrow(data), 1,
                                     0.75)
### Create validation data set
validation_data <- data[data$estimation_sample==0,]

## Logistic Regression ----------------------------------------------------

### Estimate logistic regression
Logistic_regression <- glm(Churn ~ Age +
                              Income + Relation_length + Electricity_usage + Gas_usage + exit_fee, family=binomial, data=data,
                            subset=estimation_sample==1)
summary(Logistic_regression)

### Create prediction model
predictions_model1 <- predict(Logistic_regression, type = "response", newdata= validation_data)

### Model Validation
### Hit-rate
predicted_model1 <- ifelse(predictions_model1>.5,1,0)
hit_rate_model1 <- table(validation_data$Churn,
                         predicted_model1, dnn= c("Observed", "Predicted"))
hit_rate_model1
hit_rate_1 <- (hit_rate_model1[1,1]+hit_rate_model1[2,2])/sum(hit_rate_model1) 
hit_rate_1 # hit-rate = 0.74

### Top-decile lift
decile_predicted_model1 <- ntile(predictions_model1, 10)

decile_model1 <- table(validation_data$Churn, decile_predicted_model1, dnn=
                         c("Observed", "Decile"))
decile_model1

tdl_1 <- (decile_model1[2,10] / (decile_model1[1,10]+ decile_model1[2,10])) / mean(validation_data$Churn)
tdl_1 ### tdl = 1.9251 => model is better than a random selection, more specifically, is 70% better than a random selection


### GINI coefficient

pred_model1 <- prediction(predictions_model1,
                          validation_data$Churn)
perf_model1 <- performance(pred_model1,"tpr","fpr")
plot(perf_model1,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc_model1 <- performance(pred_model1,"auc")
gini_1 <-as.numeric(auc_model1@y.values)*2-1
gini_1 ### gini = 0.6287 which is closer to 1 than to 0, making the model better than a random selection


## Step-Wise Regression ----------------------------------------------------

###Estimate full and null model
Logistic_regression_full <- glm(Churn ~ ., data = data, family = binomial, subset=estimation_sample==1)
Logistic_regression_null <- glm(Churn ~ 0, data = data, family = binomial, subset=estimation_sample==1)

### AIC
### Fit the model backward
Logistic_regression_backward <- stepAIC(Logistic_regression_full, direction="backward", trace = TRUE)

### Fit the model forward
Logistic_regression_forward <- stepAIC(Logistic_regression_null, direction="forward", scope=list(lower=Logistic_regression_null, upper=Logistic_regression_full), trace = TRUE)

### Fit the model both directions
Logistic_regression_both <- stepAIC(Logistic_regression_full, direction="both", trace = TRUE)

### BIC

### Fit the model backward using BIC
Logistic_regression_backward_BIC <- stepAIC(Logistic_regression_full, direction="backward", trace = TRUE, k = log(sum(data$estimation_sample)))

### Fit the model forward using BIC

Logistic_regression_forward_BIC <- stepAIC(Logistic_regression_null, direction="forward", scope=list(lower=Logistic_regression_null, upper=Logistic_regression_full), trace = TRUE, k = log(sum(data$estimation_sample)))

### Fit the model both directions
Logistic_regression_both_BIC <- stepAIC(Logistic_regression_full, direction="both", trace = TRUE,k = log(sum(data$estimation_sample)) )



### Create prediction model
predictions_model_backward_AIC <- predict(Logistic_regression_backward, type = "response", newdata= validation_data)
predictions_model_forward_AIC <- predict(Logistic_regression_forward, type = "response", newdata= validation_data)
predictions_model_both_AIC <- predict(Logistic_regression_both, type = "response", newdata= validation_data)

predictions_model_backward_BIC <- predict(Logistic_regression_backward_BIC, type = "response", newdata= validation_data)
predictions_model_forward_BIC <- predict(Logistic_regression_forward_BIC, type = "response", newdata= validation_data)
predictions_model_both_BIC <- predict(Logistic_regression_both_BIC, type = "response", newdata= validation_data)

### Model Validation
### Hit-rate
predicted_model_backward_AIC <- ifelse(predictions_model_backward_AIC>.5,1,0)
predicted_model_forward_AIC <- ifelse(predictions_model_forward_AIC>.5,1,0)
predicted_model_both_AIC <- ifelse(predictions_model_both_AIC>.5,1,0)

predicted_model_backward_BIC <- ifelse(predictions_model_backward_BIC>.5,1,0)
predicted_model_forward_BIC <- ifelse(predictions_model_forward_BIC>.5,1,0)
predicted_model_both_BIC <- ifelse(predictions_model_both_BIC>.5,1,0)


hit_rate_model_backward_AIC <- table(validation_data$Churn,
                         predicted_model_backward_AIC, dnn= c("Observed", "Predicted"))

hit_rate_model_forward_AIC <- table(validation_data$Churn,
                                     predicted_model_forward_AIC, dnn= c("Observed", "Predicted"))

hit_rate_model_both_AIC <- table(validation_data$Churn,
                                     predicted_model_both_AIC, dnn= c("Observed", "Predicted"))

hit_rate_model_backward_BIC <- table(validation_data$Churn,
                                     predicted_model_backward_BIC, dnn= c("Observed", "Predicted"))

hit_rate_model_forward_BIC <- table(validation_data$Churn,
                                     predicted_model_forward_BIC, dnn= c("Observed", "Predicted"))

hit_rate_model_both_BIC <- table(validation_data$Churn,
                                     predicted_model_both_BIC, dnn= c("Observed", "Predicted"))


hit_rate_model_backward_AIC
hit_rate_model_backward_BIC
hit_rate_model_both_AIC
hit_rate_model_both_BIC
hit_rate_model_forward_AIC
hit_rate_model_forward_BIC

hit_rate_backward_AIC <- (hit_rate_model_backward_AIC[1,1]+hit_rate_model_backward_AIC[2,2])/sum(hit_rate_model_backward_AIC) 
hit_rate_backward_AIC # hit-rate backward AIC = 0.7561616

hit_rate_forward_AIC <- (hit_rate_model_forward_AIC[1,1]+hit_rate_model_forward_AIC[2,2])/sum(hit_rate_model_forward_AIC) 
hit_rate_forward_AIC # hit-rate forward AIC = 0.7545455

hit_rate_both_AIC <- (hit_rate_model_both_AIC[1,1]+hit_rate_model_both_AIC[2,2])/sum(hit_rate_model_both_AIC) 
hit_rate_both_AIC # hit-rate both AIC = 0.7561616

hit_rate_backward_BIC <- (hit_rate_model_backward_BIC[1,1]+hit_rate_model_backward_BIC[2,2])/sum(hit_rate_model_backward_BIC) 
hit_rate_backward_BIC # hit-rate backward BIC = 0.7484848

hit_rate_forward_BIC <- (hit_rate_model_forward_BIC[1,1]+hit_rate_model_forward_BIC[2,2])/sum(hit_rate_model_forward_BIC) 
hit_rate_forward_BIC # hit-rate forward BIC = 0.7545455

hit_rate_both_BIC <- (hit_rate_model_both_BIC[1,1]+hit_rate_model_both_BIC[2,2])/sum(hit_rate_model_both_BIC) 
hit_rate_both_BIC # hit-rate both BIC = 0.7484848

### Top-decile lift
### TDL for backward AIC
decile_predicted_model_backward_AIC <- ntile(predictions_model_backward_AIC, 10)

decile_model_backward_AIC <- table(validation_data$Churn, decile_predicted_model_backward_AIC, dnn=
                         c("Observed", "Decile"))

tdl_backward_AIC <- (decile_model_backward_AIC[2,10] / (decile_model_backward_AIC[1,10]+ decile_model_backward_AIC[2,10])) / mean(validation_data$Churn)
tdl_backward_AIC ### tdl backward AIC = 1.880903 => model is close to 2 and is better than a random selection


### TDL for forward AIC
decile_predicted_model_forward_AIC <- ntile(predictions_model_forward_AIC, 10)

decile_model_forward_AIC <- table(validation_data$Churn, decile_predicted_model_forward_AIC, dnn=
                                     c("Observed", "Decile"))

tdl_forward_AIC <- (decile_model_forward_AIC[2,10] / (decile_model_forward_AIC[1,10]+ decile_model_forward_AIC[2,10])) / mean(validation_data$Churn)
tdl_forward_AIC ### tdl forward AIC = 1.880903 => model is close to 2 and is better than a random selection

### TDL for both AIC
decile_predicted_model_both_AIC <- ntile(predictions_model_both_AIC, 10)

decile_model_both_AIC <- table(validation_data$Churn, decile_predicted_model_both_AIC, dnn=
                                     c("Observed", "Decile"))

tdl_both_AIC <- (decile_model_both_AIC[2,10] / (decile_model_both_AIC[1,10]+ decile_model_both_AIC[2,10])) / mean(validation_data$Churn)
tdl_both_AIC ### tdl both AIC = 1.880903 => model is close to 2 and is better than a random selection

### TDL for backward BIC
decile_predicted_model_backward_BIC <- ntile(predictions_model_backward_BIC, 10)

decile_model_backward_BIC <- table(validation_data$Churn, decile_predicted_model_backward_BIC, dnn=
                                 c("Observed", "Decile"))

tdl_backward_BIC <- (decile_model_backward_BIC[2,10] / (decile_model_backward_BIC[1,10]+ decile_model_backward_BIC[2,10])) / mean(validation_data$Churn)
tdl_backward_BIC ### tdl backward BIC = 1.897331 => model is close to 2 and is better than a random selection

### TDL for forward BIC
decile_predicted_model_forward_BIC <- ntile(predictions_model_forward_BIC, 10)

decile_model_forward_BIC <- table(validation_data$Churn, decile_predicted_model_forward_BIC, dnn=
                                     c("Observed", "Decile"))

tdl_forward_BIC <- (decile_model_forward_BIC[2,10] / (decile_model_forward_BIC[1,10]+ decile_model_forward_BIC[2,10])) / mean(validation_data$Churn)
tdl_forward_BIC ### tdl forward BIC = 1.880903 => model is close to 2 and is better than a random selection

### TDL for both BIC
decile_predicted_model_both_BIC <- ntile(predictions_model_both_BIC, 10)

decile_model_both_BIC <- table(validation_data$Churn, decile_predicted_model_both_BIC, dnn=
                                    c("Observed", "Decile"))

tdl_both_BIC <- (decile_model_both_BIC[2,10] / (decile_model_both_BIC[1,10]+ decile_model_both_BIC[2,10])) / mean(validation_data$Churn)
tdl_both_BIC ### tdl both BIC = 1.897331 => model is close to 2 and is better than a random selection


### GINI coefficient

### GINI for backward AIC
pred_model_backward_AIC <- prediction(predictions_model_backward_AIC,
                          validation_data$Churn)
perf_model_backward_AIC <- performance(pred_model_backward_AIC,"tpr","fpr")
plot(perf_model_backward_AIC,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc_model_backward_AIC <- performance(pred_model_backward_AIC,"auc")
gini_backward_AIC <-as.numeric(auc_model_backward_AIC@y.values)*2-1
gini_backward_AIC ### gini = 0.6620585 which is closer to 1 than to 0, making the model better than a random selection

### GINI for forward AIC
pred_model_forward_AIC <- prediction(predictions_model_forward_AIC,
                                      validation_data$Churn)
perf_model_forward_AIC <- performance(pred_model_forward_AIC,"tpr","fpr")
plot(perf_model_forward_AIC,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc_model_forward_AIC <- performance(pred_model_forward_AIC,"auc")
gini_forward_AIC <-as.numeric(auc_model_forward_AIC@y.values)*2-1
gini_forward_AIC ### gini = 0.6617231 which is closer to 1 than to 0, making the model better than a random selection

### GINI for both AIC
pred_model_both_AIC <- prediction(predictions_model_both_AIC,
                                     validation_data$Churn)
perf_model_both_AIC <- performance(pred_model_both_AIC,"tpr","fpr")
plot(perf_model_both_AIC,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc_model_both_AIC <- performance(pred_model_both_AIC,"auc")
gini_both_AIC <-as.numeric(auc_model_both_AIC@y.values)*2-1
gini_both_AIC ### gini = 0.6620585 which is closer to 1 than to 0, making the model better than a random selection


### GINI for backward BIC
pred_model_backward_BIC <- prediction(predictions_model_backward_BIC,
                                      validation_data$Churn)
perf_model_backward_BIC <- performance(pred_model_backward_BIC,"tpr","fpr")
plot(perf_model_backward_BIC,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc_model_backward_BIC <- performance(pred_model_backward_BIC,"auc")
gini_backward_BIC <-as.numeric(auc_model_backward_BIC@y.values)*2-1
gini_backward_BIC ### gini = 0.6559485 which is closer to 1 than to 0, making the model better than a random selection

### GINI for forward BIC
pred_model_forward_BIC <- prediction(predictions_model_forward_BIC,
                                     validation_data$Churn)
perf_model_forward_BIC <- performance(pred_model_forward_BIC,"tpr","fpr")
plot(perf_model_forward_BIC,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc_model_forward_BIC <- performance(pred_model_forward_BIC,"auc")
gini_forward_BIC <-as.numeric(auc_model_forward_BIC@y.values)*2-1
gini_forward_BIC ### gini = 0.6617241 which is closer to 1 than to 0, making the model better than a random selection

### GINI for both BIC
pred_model_both_BIC <- prediction(predictions_model_both_BIC,
                                  validation_data$Churn)
perf_model_both_BIC <- performance(pred_model_both_BIC,"tpr","fpr")
plot(perf_model_both_BIC,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc_model_both_BIC <- performance(pred_model_both_BIC,"auc")
gini_both_BIC <-as.numeric(auc_model_both_BIC@y.values)*2-1
gini_both_BIC ### gini = 0.6559485 which is closer to 1 than to 0, making the model better than a random selection

## CART tree ----------------------------------------------------
Cart_tree_main <- rpart(Churn ~ Gender + Age + Income + Relation_length + Start_channel + Email_list + Home_age + Home_label + Electricity_usage + Gas_usage + Province + exit_fee, data=data, method="class", subset=estimation_sample==1)
Cart_tree_main_visual <- as.party(Cart_tree_main)
plot(Cart_tree_main_visual , type="simple", gp = gpar(fontsize = 10))


### Changing settings
newsettings1 <- rpart.control(minsplit = 100, minbucket = 50, cp = 0.01, maxdepth = 3)

Cart_tree2 <- rpart(Churn ~ Gender + Age + Income + Relation_length + Start_channel + Email_list + Home_age + Home_label + Electricity_usage + Gas_usage + Province + exit_fee, data=data, method="class", subset=estimation_sample==1, control=newsettings1)
Cart_tree2_visual <- as.party(Cart_tree2)
plot(Cart_tree2_visual , type="simple")


### Save predictions
predictions_cart <- predict(Cart_tree_main, newdata=validation_data, type ="prob")[,2]

### Model Validation
### Hit-rate
predicted_model_cart <- ifelse(predictions_cart>.5,1,0)
hit_rate_model_cart <- table(validation_data$Churn,
                             predicted_model_cart, dnn= c("Observed", "Predicted"))
hit_rate_model_cart
hit_rate_cart <- (hit_rate_model_cart[1,1]+hit_rate_model_cart[2,2])/sum(hit_rate_model_cart) 
hit_rate_cart # hit-rate = 0.69902232

### Top-decile lift
decile_predicted_model_cart <- ntile(predictions_cart, 10)

decile_model_cart <- table(validation_data$Churn, decile_predicted_model_cart, dnn=
                         c("Observed", "Decile"))
decile_model_cart

tdl_cart <- (decile_model_cart[2,10] / (decile_model_cart[1,10]+ decile_model_cart[2,10])) / mean(validation_data$Churn)
tdl_cart ### tdl = 1.602802 => model is better than a random selection, more specifically, is 70% better than a random selection


### GINI coefficient

pred_model_cart <- prediction(predictions_cart,
                          validation_data$Churn)
perf_model_cart <- performance(pred_model_cart,"tpr","fpr")
plot(perf_model_cart,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc_model_cart <- performance(pred_model_cart,"auc")
gini_cart <-as.numeric(auc_model_cart@y.values)*2-1
gini_cart ### gini = 0.4828453 

## Baggging ----------------------------------------------------
newsettings2 <- rpart.control(minsplit = 2, cp = 0.0)

### Model estimation
Bagging_tree <- bagging(Churn ~ Gender + Age + Income + Relation_length + Start_channel + Email_list + Home_age + Home_label + Electricity_usage + Gas_usage + Province + exit_fee, data=data, method="class", nbagg=1000, subset=estimation_sample==1, control=newsettings2, coob=TRUE)

### Model Predictions
predictions_bagging <- predict(Bagging_tree, newdata=validation_data, type ="response")


### Calculate the importance of the variables
pred.imp <- varImp(Bagging_tree)
pred.imp


### Plotting the results
barplot(pred.imp$Overall, names.arg = row.names(pred.imp))

### Model validation
### Hit-rate
predicted_model_bagging <- ifelse(predictions_bagging>.5,1,0)
hit_rate_model_bagging <- table(validation_data$Churn,
                             predicted_model_bagging, dnn= c("Observed", "Predicted"))
hit_rate_model_bagging
hit_rate_bagging <- (hit_rate_model_bagging[1,1]+hit_rate_model_bagging[2,2])/sum(hit_rate_model_bagging) 
hit_rate_bagging # hit-rate =  0.7523486

### Top-decile lift
decile_predicted_model_bagging <- ntile(predictions_bagging, 10)

decile_model_bagging <- table(validation_data$Churn, decile_predicted_model_bagging, dnn=
                             c("Observed", "Decile"))
decile_model_bagging

tdl_bagging <- (decile_model_bagging[2,10] / (decile_model_bagging[1,10]+ decile_model_bagging[2,10])) / mean(validation_data$Churn)
tdl_bagging ### tdl = 1.867245 => model is better than a random selection, more specifically, is 70% better than a random selection


### GINI coefficient

pred_model_bagging <- prediction(predictions_bagging,
                              validation_data$Churn)
perf_model_bagging <- performance(pred_model_bagging,"tpr","fpr")
plot(perf_model_bagging,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc_model_bagging <- performance(pred_model_bagging,"auc")
gini_bagging <-as.numeric(auc_model_bagging@y.values)*2-1
gini_bagging ### gini = 0.6609248

## Boosting ----------------------------------------------------------------

estimation_sample_boosting <- data[data$estimation_sample==1,]

#Estimate the model
boost_tree <- gbm(Churn ~ Gender + Age + Income + Relation_length + Start_channel + Email_list + Home_age + Home_label + Electricity_usage + Gas_usage + Province + exit_fee, data=estimation_sample_boosting, distribution = "bernoulli", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)

#Get model output (summary also provides a graph)
boost_tree
best.iter <- gbm.perf(boost_tree, method = "OOB")
summary(boost_tree, n.trees = best.iter, las = 1, cex.names=0.5)
#Save predictions
predictions_boosting <- predict(boost_tree, newdata=validation_data, n.trees = best.iter, type ="response")

### Model validation
### Hit-rate
predicted_model_boosting <- ifelse(predictions_boosting>.5,1,0)
hit_rate_model_boosting <- table(validation_data$Churn,
                                predicted_model_boosting, dnn= c("Observed", "Predicted"))
hit_rate_model_boosting
hit_rate_boosting <- (hit_rate_model_boosting[1,1]+hit_rate_model_boosting[2,2])/sum(hit_rate_model_boosting) 
hit_rate_boosting # hit-rate = 0.7643414

### Top-decile lift
decile_predicted_model_boosting <- ntile(predictions_boosting, 10)

decile_model_boosting <- table(validation_data$Churn, decile_predicted_model_boosting, dnn=
                                c("Observed", "Decile"))
decile_model_boosting

tdl_boosting <- (decile_model_boosting[2,10] / (decile_model_boosting[1,10]+ decile_model_boosting[2,10])) / mean(validation_data$Churn)
tdl_boosting ### tdl = 1.920015 => model is better than a random selection, more specifically, is 70% better than a random selection


### GINI coefficient

pred_model_boosting <- prediction(predictions_boosting,
                                 validation_data$Churn)
perf_model_boosting <- performance(pred_model_boosting,"tpr","fpr")
plot(perf_model_boosting,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc_model_boosting <- performance(pred_model_boosting,"auc")
gini_boosting <-as.numeric(auc_model_boosting@y.values)*2-1
gini_boosting ### gini = 0.6901542

## Random forest -----------------------------------------------------------


Random_forest <- randomForest(as.factor(Churn) ~ ., data=data, subset=estimation_sample==1, importance=TRUE)

predictions_forest <- predict(Random_forest, newdata=validation_data, type ="prob")[,2]

varImpPlot(Random_forest)

### Model validation
### Hit-rate
predicted_model_forest <- ifelse(predictions_forest>.5,1,0)
hit_rate_model_forest <- table(validation_data$Churn,
                                 predicted_model_forest, dnn= c("Observed", "Predicted"))
hit_rate_model_forest
hit_rate_forest <- (hit_rate_model_forest[1,1]+hit_rate_model_forest[2,2])/sum(hit_rate_model_forest) 
hit_rate_forest # hit-rate = 0.7495503

### Top-decile lift
decile_predicted_model_forest <- ntile(predictions_forest, 10)

decile_model_forest <- table(validation_data$Churn, decile_predicted_model_forest, dnn=
                                 c("Observed", "Decile"))
decile_model_forest

tdl_forest <- (decile_model_forest[2,10] / (decile_model_forest[1,10]+ decile_model_forest[2,10])) / mean(validation_data$Churn)
tdl_forest ### tdl = 1.883482 => model is better than a random selection, more specifically, is 70% better than a random selection


### GINI coefficient

pred_model_forest <- prediction(predictions_forest,
                                  validation_data$Churn)
perf_model_forest <- performance(pred_model_forest,"tpr","fpr")
plot(perf_model_forest,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc_model_forest <- performance(pred_model_forest,"auc")
gini_forest <-as.numeric(auc_model_forest@y.values)*2-1
gini_forest ### gini = 0.6686457


## Support Vector Machine --------------------------------------------------

### Linear SVM
svm_linear <- svm(Churn ~  Gas_usage + Electricity_usage + Age + Relation_length, data = data[,1:13], subset=data$estimation_sample==1,
             type = 'C-classification', probability = TRUE,
             kernel = 'linear')

plot(svm_linear, data, Gas_usage~Electricity_usage)

### Get linear predictions
predictions_svm_linear <- predict(svm_linear, newdata=validation_data, probability=TRUE)
predictions_svm_linear <- attr(predictions_svm_linear,"probabilities")[,1]


### Polynomial SVM
svm_polynomial <- svm(Churn ~  Gas_usage + Electricity_usage + Age + Relation_length, data = data[,1:13], subset=data$estimation_sample==1,
                      type = 'C-classification', probability = TRUE,
                      kernel = 'polynomial')

plot(svm_polynomial, data, Gas_usage~Electricity_usage)

### Get polynomial predictions
predictions_svm_polynomial <- predict(svm_polynomial, newdata=validation_data, probability=TRUE)
predictions_svm_polynomial <- attr(predictions_svm_polynomial,"probabilities")[,1]

### Radial SVM
svm_radial <- svm(Churn ~  Gas_usage + Electricity_usage+ Age + Relation_length, data = data[,1:13], subset=data$estimation_sample==1,
                  type = 'C-classification', probability = TRUE,
                  kernel = 'radial')

plot(svm_radial, data, Gas_usage~Electricity_usage)

### Get radial predictions
predictions_svm_radial <- predict(svm_radial, newdata=validation_data, probability=TRUE)
predictions_svm_radial <- attr(predictions_svm_radial,"probabilities")[,1]

### Sigmoid SVM
svm_sigmoid <- svm(Churn ~  Gas_usage + Electricity_usage+ Age + Relation_length, data = data[,1:13], subset=data$estimation_sample==1,
                   type = 'C-classification', probability = TRUE,
                   kernel = 'sigmoid')

plot(svm_sigmoid, data, Gas_usage~Electricity_usage)

### Get sigmoid predictions
predictions_svm_sigmoid <- predict(svm_sigmoid, newdata=validation_data, probability=TRUE)
predictions_svm_sigmoid <- attr(predictions_svm_sigmoid,"probabilities")[,1]

### Model validation
### Hit-rate
### Hit-rate Linear SVM
predicted_model_svm_linear <- ifelse(predictions_svm_linear>.5,1,0)
hit_rate_model_svm_linear <- table(validation_data$Churn,
                               predicted_model_svm_linear, dnn= c("Observed", "Predicted"))
hit_rate_model_svm_linear
hit_rate_svm_linear <- (hit_rate_model_svm_linear[1,1]+hit_rate_model_svm_linear[2,2])/sum(hit_rate_model_svm_linear) 
hit_rate_svm_linear # hit-rate = 0.6777933

### Hit-rate Polynomial SVM
predicted_model_svm_polynomial <- ifelse(predictions_svm_polynomial>.5,1,0)
hit_rate_model_svm_polynomial <- table(validation_data$Churn,
                                   predicted_model_svm_polynomial, dnn= c("Observed", "Predicted"))
hit_rate_model_svm_polynomial
hit_rate_svm_polynomial <- (hit_rate_model_svm_polynomial[1,1]+hit_rate_model_svm_polynomial[2,2])/sum(hit_rate_model_svm_polynomial) 
hit_rate_svm_polynomial # hit-rate = 0.6739956

### Hit-rate Radial SVM

predicted_model_svm_radial <- ifelse(predictions_svm_radial>.5,1,0)
hit_rate_model_svm_radial <- table(validation_data$Churn,
                                       predicted_model_svm_radial, dnn= c("Observed", "Predicted"))
hit_rate_model_svm_radial
hit_rate_svm_radial <- (hit_rate_model_svm_radial[1,1]+hit_rate_model_svm_radial[2,2])/sum(hit_rate_model_svm_radial) 
hit_rate_svm_radial # hit-rate = 0.681591

### Hit-rate Sigmoid SVM

predicted_model_svm_sigmoid <- ifelse(predictions_svm_sigmoid>.5,1,0)
hit_rate_model_svm_sigmoid <- table(validation_data$Churn,
                                   predicted_model_svm_sigmoid, dnn= c("Observed", "Predicted"))
hit_rate_model_svm_sigmoid
hit_rate_svm_sigmoid <- (hit_rate_model_svm_sigmoid[1,1]+hit_rate_model_svm_sigmoid[2,2])/sum(hit_rate_model_svm_sigmoid) 
hit_rate_svm_sigmoid # hit-rate = 0.5590646



### Top-decile lift Linear SVM
decile_predicted_model_svm_linear <- ntile(predictions_svm_linear, 10)

decile_model_svm_linear <- table(validation_data$Churn, decile_predicted_model_svm_linear, dnn=
                               c("Observed", "Decile"))
decile_model_svm_linear

tdl_svm_linear <- (decile_model_svm_linear[2,10] / (decile_model_svm_linear[1,10]+ decile_model_svm_linear[2,10])) / mean(validation_data$Churn)
tdl_svm_linear ### tdl = 1.692699 => model is better than a random selection

### Top-decile lift Polynomial SVM
decile_predicted_model_svm_polynomial <- ntile(predictions_svm_polynomial, 10)

decile_model_svm_polynomial <- table(validation_data$Churn, decile_predicted_model_svm_polynomial, dnn=
                                   c("Observed", "Decile"))
decile_model_svm_polynomial

tdl_svm_polynomial <- (decile_model_svm_polynomial[2,10] / (decile_model_svm_polynomial[1,10]+ decile_model_svm_polynomial[2,10])) / mean(validation_data$Churn)
tdl_svm_polynomial ### tdl = 1.63181 => model is better than a random selection

### Top-decile lift Radial SVM
decile_predicted_model_svm_radial <- ntile(predictions_svm_radial, 10)

decile_model_svm_radial <- table(validation_data$Churn, decile_predicted_model_svm_radial, dnn=
                                       c("Observed", "Decile"))
decile_model_svm_radial

tdl_svm_radial <- (decile_model_svm_radial[2,10] / (decile_model_svm_radial[1,10]+ decile_model_svm_radial[2,10])) / mean(validation_data$Churn)
tdl_svm_radial ### tdl = 1.566862 => model is better than a random selection


### Top-decile lift Sigmoid SVM
decile_predicted_model_svm_sigmoid <- ntile(predictions_svm_sigmoid, 10)

decile_model_svm_sigmoid <- table(validation_data$Churn, decile_predicted_model_svm_sigmoid, dnn=
                                   c("Observed", "Decile"))
decile_model_svm_sigmoid

tdl_svm_sigmoid <- (decile_model_svm_sigmoid[2,10] / (decile_model_svm_sigmoid[1,10]+ decile_model_svm_sigmoid[2,10])) / mean(validation_data$Churn)
tdl_svm_sigmoid ### tdl = 1.022926 => model is not better than a random selection

### GINI coefficient Linear SVM

pred_model_svm_linear <- prediction(predictions_svm_linear,
                                validation_data$Churn)
perf_model_svm_linear <- performance(pred_model_svm_linear,"tpr","fpr")
plot(perf_model_svm_linear,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc_model_svm_linear <- performance(pred_model_svm_linear,"auc")
gini_svm_linear <-as.numeric(auc_model_svm_linear@y.values)*2-1
gini_svm_linear ### gini = 0.4797216

### GINI coefficient Polynomial SVM
pred_model_svm_polynomial <- prediction(predictions_svm_polynomial,
                                    validation_data$Churn)
perf_model_svm_polynomial <- performance(pred_model_svm_polynomial,"tpr","fpr")
plot(perf_model_svm_polynomial,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc_model_svm_polynomial <- performance(pred_model_svm_polynomial,"auc")
gini_svm_polynomial <-as.numeric(auc_model_svm_polynomial@y.values)*2-1
gini_svm_polynomial ### gini = 0.4730316

### GINI coefficient Radial SVM
pred_model_svm_radial <- prediction(predictions_svm_radial,
                                        validation_data$Churn)
perf_model_svm_radial <- performance(pred_model_svm_radial,"tpr","fpr")
plot(perf_model_svm_radial,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc_model_svm_radial <- performance(pred_model_svm_radial,"auc")
gini_svm_radial <-as.numeric(auc_model_svm_radial@y.values)*2-1
gini_svm_radial ### gini = 0.4788064

### GINI coefficient Sigmoid SVM
pred_model_svm_sigmoid <- prediction(predictions_svm_sigmoid,
                                    validation_data$Churn)
perf_model_svm_sigmoid <- performance(pred_model_svm_sigmoid,"tpr","fpr")
plot(perf_model_svm_sigmoid,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc_model_svm_sigmoid <- performance(pred_model_svm_sigmoid,"auc")
gini_svm_sigmoid <-as.numeric(auc_model_svm_sigmoid@y.values)*2-1
gini_svm_sigmoid ### gini = 0.156264

