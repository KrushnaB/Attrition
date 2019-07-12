############################################## Business objective #########################################

# Help reduce attrition rates
## Identify key variables to focus on to curb attrition
## Identify the most important variable in causing attrition

getwd()

######################################## Data cleaning and preparation ####################################

#---------------------------------------------- Data preparation -----------------------------------------#

## Collating the data frames


attrition_edit <- read.csv("Attrition.csv", na.strings = c(""," ","?","NA","na"))

View(attrition_edit)

str(attrition_edit)

#--------------------------------------------- Data cleaning ---------------------------------------------#

## Checking for unnecessary rows and columns

## Checking for NAs


apply(is.na(attrition_edit),2,sum)

## Converting Categorical to numeric Value
attrition_edit$Attrition <- ifelse(attrition_edit$Attrition == 'Yes',0,1)
attrition_edit$Gender <- ifelse(attrition_edit$Gender == 'Male',1,0)
attrition_edit$OverTime <- ifelse(attrition_edit$OverTime == 'Yes',1,0)

## Creating Dummies

library(fastDummies)


attrition_edit <- dummy_columns(attrition_edit, select_columns = c('BusinessTravel','Department','EducationField','JobRole','MaritalStatus'))

View(attrition_edit)

dim(attrition_edit)


# There are someof variables which either have just one level or are not required for analysis, 
# these can be dropped

#Employee ID    - Id of an employee
#Employee Count - The value is 1 for the entire range
#Over 18        - All the employees are over 18
#Standard Hours - The value is same for all the employees (8 hours) etc..


attrition_edit <- attrition_edit[c(-3,-5,-8,-9,-16,-18,-22,-27)]

View(attrition_edit)

str(attrition_edit)

########################################## Exploratory data analysis ######################################

## Checking Plots(Outliers) .......


library(ggplot2)

boxplot(attrition_edit$Age)

boxplot(attrition_edit$DailyRate)

boxplot(attrition_edit$DistanceFromHome)

boxplot(attrition_edit$Education)

boxplot(attrition_edit$EnvironmentSatisfaction)

boxplot(attrition_edit$HourlyRate)

boxplot(attrition_edit$JobInvolvement)

boxplot(attrition_edit$JobLevel)

boxplot(attrition_edit$JobSatisfaction)

boxplot(attrition_edit$MonthlyIncome)             # Outliers

boxplot(attrition_edit$MonthlyRate)

boxplot(attrition_edit$NumCompaniesWorked)            # Outliers

boxplot(attrition_edit$PercentSalaryHike)

boxplot(attrition_edit$PerformanceRating)                      # Outliers

boxplot(attrition_edit$RelationshipSatisfaction)

boxplot(attrition_edit$StockOptionLevel)              # Outliers

boxplot(attrition_edit$TotalWorkingYears)           # Outliers

boxplot(attrition_edit$TrainingTimesLastYear)       # Outliers

boxplot(attrition_edit$WorkLifeBalance)

boxplot(attrition_edit$YearsAtCompany)              # Outliers

boxplot(attrition_edit$YearsInCurrentRole)                    # Outliers

boxplot(attrition_edit$YearsSinceLastPromotion)      # Outliers

boxplot(attrition_edit$YearsWithCurrManager)   # Outliers


## Defining functions for categorical and continous variables

out_std_check <- function(x,f){
  
  q1 = quantile(x,0.25,na.rm = TRUE)
  q3 = quantile(x,0.75,na.rm = TRUE)
  iqr = q3 - q1
  
  uc <- q3 + f*iqr
  lc <- q1 - f*iqr
  
  n <- sum(x>uc | x<lc)
  val <- c(num = n,upper = uc,lower = lc)
  return(val)
}

out_std_check(attrition_edit$MonthlyIncome,1.5)   

out_std_check(attrition_edit$NumCompaniesWorked,1.5) 

out_std_check(attrition_edit$PerformanceRating,1.5)

out_std_check(attrition_edit$StockOptionLevel,1.5) 

out_std_check(attrition_edit$TotalWorkingYears,1.5)

out_std_check(attrition_edit$TrainingTimesLastYear,1.5)

out_std_check(attrition_edit$YearsAtCompany,1.5)   

out_std_check(attrition_edit$YearsInCurrentRole,1.5)

out_std_check(attrition_edit$YearsSinceLastPromotion,1.5)

out_std_check(attrition_edit$YearsWithCurrManager,1.5)



attri_out_impute <- function(x,f){
  
  q1 = quantile(x,0.25,na.rm = TRUE)
  q3 = quantile(x, 0.75, na.rm = TRUE)
  iqr = q3-q1
  
  uc = q3 + f*iqr
  lc = q1 - f*iqr
  
  for (i in 1:length(x)) {
    if (x[i] > uc){
      x[i] <- uc
    }
    else if(x[i] < lc){
      x[i] <- lc
    }
    else {}
  }
  return(x)
}

attrition_edit$MonthlyIncome <- attri_out_impute(attrition_edit$MonthlyIncome,1.5)
out_std_check(attrition_edit$MonthlyIncome,1.5)


attrition_edit$NumCompaniesWorked <- attri_out_impute(attrition_edit$NumCompaniesWorked,1.5)
out_std_check(attrition_edit$NumCompaniesWorked,1.5) 


attrition_edit$PerformanceRating <- attri_out_impute(attrition_edit$PerformanceRating,1.5)
out_std_check(attrition_edit$PerformanceRating,1.5)


attrition_edit$StockOptionLevel <- attri_out_impute(attrition_edit$StockOptionLevel,1.5)
out_std_check(attrition_edit$StockOptionLevel,1.5) 

attrition_edit$TotalWorkingYears <- attri_out_impute(attrition_edit$TotalWorkingYears,1.5)
out_std_check(attrition_edit$TotalWorkingYears,1.5)

attrition_edit$TrainingTimesLastYear <- attri_out_impute(attrition_edit$TrainingTimesLastYear,1.5)
out_std_check(attrition_edit$TrainingTimesLastYear,1.5)

attrition_edit$YearsAtCompany <- attri_out_impute(attrition_edit$YearsAtCompany,1.5)
out_std_check(attrition_edit$YearsAtCompany,1.5)

attrition_edit$YearsInCurrentRole <- attri_out_impute(attrition_edit$YearsInCurrentRole,1.5)
out_std_check(attrition_edit$YearsInCurrentRole,1.5)

attrition_edit$YearsSinceLastPromotion <- attri_out_impute(attrition_edit$YearsSinceLastPromotion,1.5)
out_std_check(attrition_edit$YearsSinceLastPromotion,1.5)

attrition_edit$YearsWithCurrManager <- attri_out_impute(attrition_edit$YearsWithCurrManager,1.5)
out_std_check(attrition_edit$YearsWithCurrManager,1.5)

attrition_edit <- attrition_edit[c(-6,-18)]


str(attrition_edit)


 ## Normalization Data
attrition_edit$Age <- (attrition_edit$Age - min(attrition_edit$Age))/(max(attrition_edit$Age)-min(attrition_edit$Age))

attrition_edit$DailyRate <- (attrition_edit$DailyRate - min(attrition_edit$DailyRate))/(max(attrition_edit$DailyRate) - min(attrition_edit$DailyRate))

attrition_edit$DistanceFromHome <- 
  (attrition_edit$DistanceFromHome - min(attrition_edit$DistanceFromHome))/(max(attrition_edit$DistanceFromHome)-min(attrition_edit$DistanceFromHome))

attrition_edit$Education <- (attrition_edit$Education - min(attrition_edit$Education))/(max(attrition_edit$Education)-min(attrition_edit$Education))

attrition_edit$HourlyRate <- (attrition_edit$HourlyRate - min(attrition_edit$HourlyRate))/(max(attrition_edit$HourlyRate)-min(attrition_edit$HourlyRate))

attrition_edit$EnvironmentSatisfaction <- 
  (attrition_edit$EnvironmentSatisfaction-min(attrition_edit$EnvironmentSatisfaction))/(max(attrition_edit$EnvironmentSatisfaction)-
                                                                        min(attrition_edit$EnvironmentSatisfaction))

attrition_edit$JobInvolvement <- (attrition_edit$JobInvolvement - min(attrition_edit$JobInvolvement))/(max(attrition_edit$JobInvolvement)-min(attrition_edit$JobInvolvement))

attrition_edit$JobLevel <- (attrition_edit$JobLevel - min(attrition_edit$JobLevel))/(max(attrition_edit$JobLevel)-min(attrition_edit$JobLevel))

attrition_edit$JobSatisfaction <- (attrition_edit$JobSatisfaction - min(attrition_edit$JobSatisfaction))/(max(attrition_edit$JobSatisfaction)-min(attrition_edit$JobSatisfaction))

attrition_edit$MonthlyIncome <- (attrition_edit$MonthlyIncome - min(attrition_edit$MonthlyIncome))/(max(attrition_edit$MonthlyIncome)-min(attrition_edit$MonthlyIncome))

attrition_edit$MonthlyRate <- (attrition_edit$MonthlyRate - min(attrition_edit$MonthlyRate))/(max(attrition_edit$MonthlyRate)-min(attrition_edit$MonthlyRate))

attrition_edit$NumCompaniesWorked <- (attrition_edit$NumCompaniesWorked - min(attrition_edit$NumCompaniesWorked))/(max(attrition_edit$NumCompaniesWorked)-min(attrition_edit$NumCompaniesWorked))

attrition_edit$PercentSalaryHike <- (attrition_edit$PercentSalaryHike - min(attrition_edit$PercentSalaryHike))/(max(attrition_edit$PercentSalaryHike)-min(attrition_edit$PercentSalaryHike))

attrition_edit$RelationshipSatisfaction <- (attrition_edit$RelationshipSatisfaction - min(attrition_edit$RelationshipSatisfaction))/(max(attrition_edit$RelationshipSatisfaction)-min(attrition_edit$RelationshipSatisfaction))

attrition_edit$StockOptionLevel <- (attrition_edit$StockOptionLevel - min(attrition_edit$StockOptionLevel))/(max(attrition_edit$StockOptionLevel)-min(attrition_edit$StockOptionLevel))

attrition_edit$TotalWorkingYears <- (attrition_edit$TotalWorkingYears - min(attrition_edit$TotalWorkingYears))/(max(attrition_edit$TotalWorkingYears)-min(attrition_edit$TotalWorkingYears))

attrition_edit$TrainingTimesLastYear <- (attrition_edit$TrainingTimesLastYear - min(attrition_edit$TrainingTimesLastYear))/(max(attrition_edit$TrainingTimesLastYear)-min(attrition_edit$TrainingTimesLastYear))

attrition_edit$WorkLifeBalance <- (attrition_edit$WorkLifeBalance - min(attrition_edit$WorkLifeBalance))/(max(attrition_edit$WorkLifeBalance)-min(attrition_edit$WorkLifeBalance))

attrition_edit$YearsAtCompany <- (attrition_edit$YearsAtCompany - min(attrition_edit$YearsAtCompany))/(max(attrition_edit$YearsAtCompany)-min(attrition_edit$YearsAtCompany))

attrition_edit$YearsInCurrentRole <- (attrition_edit$YearsInCurrentRole - min(attrition_edit$YearsInCurrentRole))/(max(attrition_edit$YearsInCurrentRole)-min(attrition_edit$YearsInCurrentRole))

attrition_edit$YearsSinceLastPromotion <- (attrition_edit$YearsSinceLastPromotion - min(attrition_edit$YearsSinceLastPromotion))/(max(attrition_edit$YearsSinceLastPromotion)-min(attrition_edit$YearsSinceLastPromotion))

attrition_edit$YearsWithCurrManager <- (attrition_edit$YearsWithCurrManager - min(attrition_edit$YearsWithCurrManager))/(max(attrition_edit$YearsWithCurrManager)-min(attrition_edit$YearsWithCurrManager))

View(attrition_edit)



attrition_edit$Attrition <- as.factor(attrition_edit$Attrition)

############################################## Model Building #############################################

## creating testing and training datasets

set.seed(108)
s <- sample(1:nrow(attrition_edit), 0.8*nrow(attrition_edit))
train <- attrition_edit[s,]
test <- attrition_edit[-s,]

View(train)

View(test)

dim(train)


dim(test)


library(e1071)

library(caret)


svmod_1 <- svm(Attrition~. , data = train, method = "C-classification", kernal = "radial", 
               cost = 100, gamma = 0.5)

summary(svmod_1)


pred_mod_1 <- predict(svmod_1, test)
tab_1 <- table(Predicted = pred_mod_1, Actual = test$Attrition)
tab_1


## Cheking Accuracy of Model

accuracy=sum(svmod_1==train$Attrition)/sum(train)
accuracy

