############################################## Business objective #########################################

# Help reduce attrition rates
## Identify key variables to focus on to curb attrition
## Identify the most important variable in causing attrition

getwd()

######################################## Data cleaning and preparation ####################################

#---------------------------------------------- Data preparation -----------------------------------------#

## Collating the data frames

attrtion_edit <- read.csv('Attrition.csv', na.strings = c(""," ","?","NA","na"))

View(attrtion_edit)

str(attrtion_edit)

dim(attrtion_edit)

#--------------------------------------------- Data cleaning ---------------------------------------------#

## Checking for unnecessary rows and columns

## Checking for NAs

apply(is.na(attrtion_edit),2,sum) 



### Data Preprocesing  (Categorical to Numeric Value)

attrtion_edit$Attrition <- ifelse(attrtion_edit$Attrition == 'Yes',0,1)

attrtion_edit$Gender <- ifelse(attrtion_edit$Gender == 'Male',1,0)

attrtion_edit$OverTime <- ifelse(attrtion_edit$OverTime == 'No',0,1)

View(attrtion_edit)

################################## Data preparation for model building ################################

## Converting categorical varibales into factors

# Observations:
# 1. Environment satisfaction, job satisfaction, work life balance, job involvement, 
#    performance rating need to be made into factors
# 2. Attrition, business travel, department, education, education field, gender need to be made factors
# 3. Job level, job role, marital status, over 18 and stock option level need to be converted

attrtion_edit$Education <- as.factor(attrtion_edit$Education)
attrtion_edit$EnvironmentSatisfaction <- as.factor(attrtion_edit$EnvironmentSatisfaction)
attrtion_edit$JobInvolvement <- as.factor(attrtion_edit$JobInvolvement)
attrtion_edit$JobSatisfaction <- as.factor(attrtion_edit$JobSatisfaction)
attrtion_edit$PerformanceRating <- as.factor(attrtion_edit$PerformanceRating)
attrtion_edit$RelationshipSatisfaction <- as.factor(attrtion_edit$RelationshipSatisfaction)
attrtion_edit$WorkLifeBalance <- as.factor(attrtion_edit$WorkLifeBalance)

str(attrtion_edit)

library(fastDummies)


attrtion_edit <- dummy_cols(attrtion_edit, select_columns = c('BusinessTravel','Department','Education','EducationField',
                                            'EnvironmentSatisfaction','JobInvolvement','JobRole','JobSatisfaction',
                                            'MaritalStatus','PerformanceRating','RelationshipSatisfaction',
                                            'WorkLifeBalance'))

View(attrtion_edit)

dim(attrtion_edit)


# There are someof variables which either have just one level or are not required for analysis, 
# these can be dropped

#Employee ID    - Id of an employee
#Employee Count - The value is 1 for the entire range
#Over 18        - All the employees are over 18
#Standard Hours - The value is same for all the employees (8 hours) etc..



attrtion_edit <- attrtion_edit[c(-3,-5,-7,-8,-9,-10,-11,-14,-15,-16,-17,-18,-22,-25,-26,-31)]

str(attrtion_edit)


########################################## Exploratory data analysis ######################################

##Checking Outliers


library(ggplot2)

boxplot(attrtion_edit$Age)

boxplot(attrtion_edit$DailyRate)

boxplot(attrtion_edit$DistanceFromHome)

boxplot(attrtion_edit$HourlyRate)

boxplot(attrtion_edit$MonthlyIncome)         # Outliers Detected.

boxplot(attrtion_edit$MonthlyRate)

boxplot(attrtion_edit$NumCompaniesWorked)   # Outlier detected (1).

boxplot(attrtion_edit$PercentSalaryHike)

boxplot(attrtion_edit$StandardHours)

boxplot(attrtion_edit$StockOptionLevel)     # Outlier Detected (1).

boxplot(attrtion_edit$TotalWorkingYears)     # Outliers Detected.

boxplot(attrtion_edit$TrainingTimesLastYear)    # Outliers Detected.

boxplot(attrtion_edit$YearsAtCompany)          # Outliers Detected.

boxplot(attrtion_edit$YearsInCurrentRole)        # Outliers Detected.

boxplot(attrtion_edit$YearsSinceLastPromotion)    # Outliers Detected.

boxplot(attrtion_edit$YearsWithCurrManager)        # Outliers Detected.


# Defining functions for categorical and continous variables

out_std_check = function(x,f){
  q1 = quantile(x,0.25, na.rm = TRUE)
  q3 = quantile(x,0.75, na.rm = TRUE)
  iqr = q3-q1
  lc=q1-f*iqr
  uc=q3+f*iqr
  n=sum(x>uc | x<lc)
  return n
  val=list(num=n,lower_cutoff=lc,upper_cutoff=uc)
  return(val)
}


out_std_check(attrtion_edit$MonthlyIncome,1.5)

out_std_check(attrtion_edit$NumCompaniesWorked,1.5)

out_std_check(attrtion_edit$StockOptionLevel,1.5)

out_std_check(attrtion_edit$TotalWorkingYears,1.5)

out_std_check(attrtion_edit$TrainingTimesLastYear,1.5)

out_std_check(attrtion_edit$YearsAtCompany,1.5)

out_std_check(attrtion_edit$YearsInCurrentRole,1.5)

out_std_check(attrtion_edit$YearsSinceLastPromotion,1.5)

out_std_check(attrtion_edit$YearsWithCurrManager,1.5)



attr_out_impute <- function(x,f){
  
  q1 = quantile(x,0.25, na.rm = TRUE)
  q3 = quantile(x,0.75, na.rm = TRUE)
  iqr = q3-q1
  lc = q1-f*iqr
  uc = q3+f*iqr
  
  for (j in 1:length(x)) {
    if(x[j]<lc)
    {x[j] <- lc}
    else if(x[j]>uc)
    {x[j] <- uc}
    else
    {}
  }
  return(x) 
}


attrtion_edit$MonthlyIncome <- attr_out_impute(attrtion_edit$MonthlyIncome,1.5)
out_std_check(attrtion_edit$MonthlyIncome,1.5)


attrtion_edit$NumCompaniesWorked <- attr_out_impute(attrtion_edit$NumCompaniesWorked,1.5)
out_std_check(attrtion_edit$NumCompaniesWorked,1.5)


attrtion_edit$StockOptionLevel <- attr_out_impute(attrtion_edit$StockOptionLevel,1.5)
out_std_check(attrtion_edit$StockOptionLevel,1.5)

attrtion_edit$TotalWorkingYears <- attr_out_impute(attrtion_edit$TotalWorkingYears,1.5)
out_std_check(attrtion_edit$TotalWorkingYears,1.5)


attrtion_edit$TrainingTimesLastYear <- attr_out_impute(attrtion_edit$TrainingTimesLastYear,1.5)
out_std_check(attrtion_edit$TrainingTimesLastYear,1.5)

attrtion_edit$YearsAtCompany <- attr_out_impute(attrtion_edit$YearsAtCompany,1.5)
out_std_check(attrtion_edit$YearsAtCompany,1.5)


attrtion_edit$YearsInCurrentRole <- attr_out_impute(attrtion_edit$YearsInCurrentRole,1.5)
out_std_check(attrtion_edit$YearsInCurrentRole,1.5)

attrtion_edit$YearsSinceLastPromotion <- attr_out_impute(attrtion_edit$YearsSinceLastPromotion,1.5)
out_std_check(attrtion_edit$YearsSinceLastPromotion,1.5)


## Normalization of Data

attrtion_edit$YearsWithCurrManager <- attr_out_impute(attrtion_edit$YearsWithCurrManager,1.5)
out_std_check(attrtion_edit$YearsWithCurrManager,1.5)


attrtion_edit$Age <- (attrtion_edit$Age - min(attrtion_edit$Age))/(max(attrtion_edit$Age)-min(attrtion_edit$Age))

attrtion_edit$DailyRate <- (attrtion_edit$DailyRate - min(attrtion_edit$DailyRate))/(max(attrtion_edit$DailyRate) - min(attrtion_edit$DailyRate))

attrtion_edit$DistanceFromHome <- (attrtion_edit$DistanceFromHome - min(attrtion_edit$DistanceFromHome))/(max(attrtion_edit$DistanceFromHome) - min(attrtion_edit$DistanceFromHome))

attrtion_edit$HourlyRate <- (attrtion_edit$HourlyRate - min(attrtion_edit$HourlyRate))/(max(attrtion_edit$HourlyRate)-min(attrtion_edit$HourlyRate))

attrtion_edit$MonthlyIncome <- (attrtion_edit$MonthlyIncome - min(attrtion_edit$MonthlyIncome))/(max(attrtion_edit$MonthlyIncome)-min(attrtion_edit$MonthlyIncome))

attrtion_edit$MonthlyRate <- (attrtion_edit$MonthlyRate - min(attrtion_edit$MonthlyRate))/(max(attrtion_edit$MonthlyRate)-min(attrtion_edit$MonthlyRate))

attrtion_edit$NumCompaniesWorked <- (attrtion_edit$NumCompaniesWorked - min(attrtion_edit$NumCompaniesWorked))/(max(attrtion_edit$NumCompaniesWorked)-min(attrtion_edit$NumCompaniesWorked))

attrtion_edit$PercentSalaryHike <- (attrtion_edit$PercentSalaryHike - min(attrtion_edit$PercentSalaryHike))/(max(attrtion_edit$PercentSalaryHike)-min(attrtion_edit$PercentSalaryHike))

attrtion_edit$StockOptionLevel <- (attrtion_edit$StockOptionLevel - min(attrtion_edit$StockOptionLevel))/(max(attrtion_edit$StockOptionLevel)-min(attrtion_edit$StockOptionLevel))

attrtion_edit$TotalWorkingYears <- (attrtion_edit$TotalWorkingYears - min(attrtion_edit$TotalWorkingYears))/(max(attrtion_edit$TotalWorkingYears)-min(attrtion_edit$TotalWorkingYears))

attrtion_edit$TrainingTimesLastYear <- (attrtion_edit$TrainingTimesLastYear - min(attrtion_edit$TrainingTimesLastYear))/(max(attrtion_edit$TrainingTimesLastYear)-min(attrtion_edit$TrainingTimesLastYear))

attrtion_edit$YearsAtCompany <- (attrtion_edit$YearsAtCompany - min(attrtion_edit$YearsAtCompany))/(max(attrtion_edit$YearsAtCompany)-min(attrtion_edit$YearsAtCompany))

attrtion_edit$YearsInCurrentRole <- (attrtion_edit$YearsInCurrentRole - min(attrtion_edit$YearsInCurrentRole))/(max(attrtion_edit$YearsInCurrentRole)-min(attrtion_edit$YearsInCurrentRole))

attr$YearsSinceLastPromotion <- (attr$YearsSinceLastPromotion - min(attr$YearsSinceLastPromotion))/(max(attr$YearsSinceLastPromotion)-min(attrtion_edit$YearsSinceLastPromotion))

attrtion_edit$YearsWithCurrManager <- (attrtion_edit$YearsWithCurrManager - min(attrtion_edit$YearsWithCurrManager))/(max(attrtion_edit$YearsWithCurrManager)-min(attrtion_edit$YearsWithCurrManager))

attrtion_edit <- attrtion_edit[-12]


View(attrtion_edit)

colnames(attrtion_edit)[21] <- "BusinessTravel_Non_Travel"
colnames(attrtion_edit)[23] <- "Department_Research_Development"
colnames(attrtion_edit)[24] <- "Department_Human_Resources"
colnames(attrtion_edit)[30] <- "EducationField_Life_Sciences"
colnames(attrtion_edit)[34] <- "EducationField_Technical_Degree"
colnames(attrtion_edit)[35] <- "EducationField_Human_Resources"
colnames(attrtion_edit)[44] <- "JobRole_Sales_Executive"
colnames(attrtion_edit)[45] <- "JobRole_Research_Scientist"
colnames(attrtion_edit)[46] <- "JobRole_Laboratory_Technician"
colnames(attrtion_edit)[47] <- "JobRole_Manufacturing_Director"
colnames(attrtion_edit)[48] <- "JobRole_Healthcare_Representative"
colnames(attrtion_edit)[50] <- "JobRole_Sales_Representative"
colnames(attrtion_edit)[51] <- "JobRole_Research_Director"
colnames(attrtion_edit)[52] <- "JobRole_Human_Resources"

## Making corr plot of data 


library(corrplot)

attr_cor <- cor(attrtion_edit)
View(attr_cor)
corrplot(attr_cor)

# most departments show uniform values of employee satisfaction metrics
# this not the cause of high attrition in HR department


############################################## Model Building #############################################

## creating testing and training datasets

set.seed(18)
s <- sample(1:nrow(attrtion_edit),0.8*nrow(attrtion_edit))
train <- attrtion_edit[s,]
test <- attrtion_edit[-s,]

View(train)

View(test)


library(MASS)


mod_1 <- glm(Attrition~.,data = train, family = 'binomial')
summary(mod_1)



pred_train <- predict(mod_1,train,type = 'response')


head(pred_train)


pred_1 <- ifelse(pred_train > 0.5,1,0)
tab_1 <- table(Predicted = pred_1,Actual = train$Attrition)
tab_1


pred_train_accuracy <- sum(diag(tab_1))/sum(tab_1)
pred_train_accuracy


pred_test <- predict(mod_1,test,type = 'response')

head(pred_test)

pred_2 <- ifelse(pred_test > 0.5,1,0)
tab_2 <- table(Predicted = pred_2, Actual = test$Attrition)
tab_2

pred_test_accuracy <- sum(diag(tab_2))/sum(tab_2)
pred_test_accuracy




mod_2 <- glm(Attrition~.-BusinessTravel_Non_Travel -Department_Human_Resources -Education_5 -EducationField_Human_Resources 
             -EnvironmentSatisfaction_1 -JobInvolvement_1 -JobRole_Human_Resources -JobSatisfaction_1
             -MaritalStatus_Divorced -PerformanceRating_4 -RelationshipSatisfaction_3
             -WorkLifeBalance_4 ,data = train, family = 'binomial')
summary(mod_2)

pred_train <- predict(mod_2,train,type = 'response')
head(pred_train)

pred_1 <- ifelse(pred_train > 0.5,1,0)
tab_1 <- table(Predicted = pred_1,Actual = train$Attrition)
tab_1

pred_train_accuracy <- sum(diag(tab_1))/sum(tab_1)
pred_train_accuracy



pred_test <- predict(mod_2,test,type = 'response')
head(pred_test)

pred_2 <- ifelse(pred_test > 0.5,1,0)
tab_2 <- table(Predicted = pred_2, Actual = test$Attrition)
tab_2

pred_test_accuracy <- sum(diag(tab_2))/sum(tab_2)
pred_test_accuracy


mod_3 <- glm(Attrition~.-BusinessTravel_Non_Travel -Department_Human_Resources -Education_5 -EducationField_Human_Resources 
             -EnvironmentSatisfaction_1 -JobInvolvement_1 -JobRole_Human_Resources -JobSatisfaction_1
             -MaritalStatus_Divorced -PerformanceRating_4 -RelationshipSatisfaction_3
             -WorkLifeBalance_4 -JobRole_Sales_Executive ,data = train, family = 'binomial')
summary(mod_3)


mod_4 <- glm(Attrition~.-BusinessTravel_Non_Travel -Department_Human_Resources -Education_5 -EducationField_Human_Resources 
             -EnvironmentSatisfaction_1 -JobInvolvement_1 -JobRole_Human_Resources -JobSatisfaction_1
             -MaritalStatus_Divorced -PerformanceRating_4 -RelationshipSatisfaction_3
             -WorkLifeBalance_4 -JobRole_Sales_Executive -WorkLifeBalance_2 ,data = train, family = 'binomial')
summary(mod_4)


mod_5 <- glm(Attrition~.-BusinessTravel_Non_Travel -Department_Human_Resources -Education_5 -EducationField_Human_Resources 
             -EnvironmentSatisfaction_1 -JobInvolvement_1 -JobRole_Human_Resources -JobSatisfaction_1
             -MaritalStatus_Divorced -PerformanceRating_4 -RelationshipSatisfaction_3
             -WorkLifeBalance_4 -JobRole_Sales_Executive -WorkLifeBalance_2 -Education_3 ,data = train, family = 'binomial')
summary(mod_5)


mod_6 <- glm(Attrition~.-BusinessTravel_Non_Travel -Department_Human_Resources -Education_5 -EducationField_Human_Resources 
             -EnvironmentSatisfaction_1 -JobInvolvement_1 -JobRole_Human_Resources -JobSatisfaction_1
             -MaritalStatus_Divorced -PerformanceRating_4 -RelationshipSatisfaction_3
             -WorkLifeBalance_4 -JobRole_Sales_Executive -WorkLifeBalance_2 -Education_3 
             -Education_2,data = train, family = 'binomial')
summary(mod_6)


mod_7 <- glm(Attrition~.-BusinessTravel_Non_Travel -Department_Human_Resources -Education_5 -EducationField_Human_Resources 
             -EnvironmentSatisfaction_1 -JobInvolvement_1 -JobRole_Human_Resources -JobSatisfaction_1
             -MaritalStatus_Divorced -PerformanceRating_4 -RelationshipSatisfaction_3
             -WorkLifeBalance_4 -JobRole_Sales_Executive -WorkLifeBalance_2 -Education_3 
             -Education_2 -Department_Sales,data = train, family = 'binomial')
summary(mod_7)


############################################## Conclusion ################################################


# The model contains only highly significant variables with little to no multicollinearity

# Key variables indicative of employee attrition
# 1. Employee age - young staff (18 to 25) have a high chance of attrition
# Make the workplace attractive to young staff - competitive pay, broadening opportunities etc
# 2. Senior staff are also prone to attrition, as expected due to voluntary or mandatory retirement
# No action can be taken, unless senior employees can be retained on contractor basis post retirement
# 3. Staff who spend a lot of hours ( > 8) at work are likely to quit
# discourage working overtime at the office, encourage managers to distribute work more evenly among staff
# 4. Employees who rate "low" on employee satisfaction surveys are likely to quit
# Engage staff with “low” survey ratings and incorporate feedback into improvement plans
# 5. Single staff are more likely to quit
# Requires further investigation, target employee survey for single staff?
# 6. the HR department has also shows a relatively high attrition rate
# Requires further investigation and engagement with staff in HR department
# 7. Staff that travel frequently are also likely to quit
# Distribute work that requires travel more evenly among employees, incentivise business travel with bonuses/allowances

# The single most important variable is age, with young employees having an attrition rate of 50%























































































