############################################## Business objective #########################################

# Help reduce attrition rates
## Identify key variables to focus on to curb attrition
## Identify the most important variable in causing attrition


getwd()

######################################## Data cleaning and preparation ####################################

#---------------------------------------------- Data preparation -----------------------------------------#

## Collating the data frames
attrition_edit <- read.csv("Attrition.csv", na.strings = c(' ', '', '?','NA','na'))

str(attrition_edit)

#--------------------------------------------- Data cleaning ---------------------------------------------#

## Checking for unnecessary rows and columns

## Checking for NAs

apply(is.na(attrtion_edit), 2, sum)


View(attrtion_edit)

summary(attrtion_edit)

########################################## Exploratory data analysis ######################################

### Checking Coorplot

library(corrplot)

numeric <- attrtion_edit[sapply(attrtion_edit, is.numeric)]
descrCor <- cor(numeric)

View(descrCor)
print(descrCor)

corrplot(descrCor)


## Standardization

attrtion_edit$Age <- ifelse(attrtion_edit$Age >= 40,1,0)
attrtion_edit$Attrition <- ifelse(attrtion_edit$Attrition == 'No',1,0)
attrtion_edit$DailyRate <- ifelse(attrtion_edit$DailyRate >= 1000,1,0)
attrtion_edit$DistanceFromHome <- ifelse(attrtion_edit$DistanceFromHome >= 15, 1,0)
attrtion_edit$Education <- ifelse(attrtion_edit$Education >= 3,1,0)
attrtion_edit$EnvironmentSatisfaction <- ifelse(attrtion_edit$EnvironmentSatisfaction >= 3,1,0)
attrtion_edit$Gender <- ifelse(attrtion_edit$Gender == 'Male',1,0)
attrtion_edit$HourlyRate <- ifelse(attrtion_edit$HourlyRate >= 60,1,0)
attrtion_edit$JobInvolvement <- ifelse(attrtion_edit$JobInvolvement >= 3,1,0)
attrtion_edit$JobLevel <- ifelse(attrtion_edit$JobLevel >= 3,1,0)
attrtion_edit$JobSatisfaction <- ifelse(attrtion_edit$JobSatisfaction >= 3,1,0)
attrtion_edit$MonthlyIncome <- ifelse(attrtion_edit$MonthlyIncome >= 8000,1,0)
attrtion_edit$MonthlyRate <- ifelse(attrtion_edit$MonthlyRate >= 15000,1,0)
attrtion_edit$NumCompaniesWorked <- ifelse(attrtion_edit$NumCompaniesWorked >= 3,1,0)
attrtion_edit$OverTime <- ifelse(attrtion_edit$OverTime == 'Yes',1,0)
attrtion_edit$PercentSalaryHike <- ifelse(attrtion_edit$PercentSalaryHike >= 16,1,0)
attrtion_edit$PerformanceRating <- ifelse(attrtion_edit$PerformanceRating == 4,1,0)
attrtion_edit$RelationshipSatisfaction <- ifelse(attrtion_edit$RelationshipSatisfaction >= 3,1,0)
attrtion_edit$StockOptionLevel <- ifelse(attrtion_edit$StockOptionLevel >= 3,1,0)
attrtion_edit$TotalWorkingYears <- ifelse(attrtion_edit$TotalWorkingYears >= 12,1,0)
attrtion_edit$TrainingTimesLastYear <- ifelse(attrtion_edit$TrainingTimesLastYear >= 3,1,0)
attrtion_edit$WorkLifeBalance <- ifelse(attrtion_edit$WorkLifeBalance >= 3, 1,0)
attrtion_edit$YearsAtCompany <- ifelse(attrtion_edit$YearsAtCompany >= 7,1,0)
attrtion_edit$YearsInCurrentRole <- ifelse(attrtion_edit$YearsInCurrentRole >= 5,1,0)
attrtion_edit$YearsSinceLastPromotion <- ifelse(attrtion_edit$YearsSinceLastPromotion >= 3,1,0)
attrtion_edit$YearsWithCurrManager <- ifelse(attrtion_edit$YearsWithCurrManager >= 5,1,0)

View(attrtion_edit)


# There are someof variables which either have just one level or are not required for analysis, 
# these can be dropped

#Employee ID    - Id of an employee
#Employee Count - The value is 1 for the entire range
#Over 18        - All the employees are over 18
#Standard Hours - The value is same for all the employees (8 hours) etc..


attrtion_edit <- attrtion_edit[c(-9,-10,-22,-27)]


################################## Data preparation for model building ################################

## Converting categorical varibales into factors

# Observations:
# 1. Environment satisfaction, job satisfaction, work life balance, job involvement, 
#    performance rating need to be made into factors
# 2. Attrition, business travel, department, education, education field, gender need to be made factors
# 3. Job level, job role, marital status, over 18 and stock option level need to be converted


attrtion_edit$Age <- as.factor(attrtion_edit$Age)
attrtion_edit$Attrition <- as.factor(attrtion_edit$Attrition)
attrtion_edit$DailyRate <- as.factor(attrtion_edit$DailyRate)
attrtion_edit$DistanceFromHome <- as.factor(attrtion_edit$DistanceFromHome)
attrtion_edit$Education <- as.factor(attrtion_edit$Education)
attrtion_edit$EnvironmentSatisfaction <- as.factor(attrtion_edit$EnvironmentSatisfaction)
attrtion_edit$Gender <- as.factor(attrtion_edit$Gender)
attrtion_edit$HourlyRate <- as.factor(attrtion_edit$HourlyRate)
attrtion_edit$JobInvolvement <- as.factor(attrtion_edit$JobInvolvement)
attrtion_edit$JobLevel <- as.factor(attrtion_edit$JobLevel)
attrtion_edit$JobSatisfaction <- as.factor(attrtion_edit$JobSatisfaction)
attrtion_edit$MonthlyIncome <- as.factor(attrtion_edit$MonthlyIncome)
attrtion_edit$MonthlyRate <- as.factor(attrtion_edit$MonthlyRate)
attrtion_edit$NumCompaniesWorked <- as.factor(attrtion_edit$NumCompaniesWorked)
attrtion_edit$OverTime <- as.factor(attrtion_edit$OverTime)
attrtion_edit$PercentSalaryHike <- as.factor(attrtion_edit$PercentSalaryHike)
attrtion_edit$PerformanceRating <- as.factor(attrtion_edit$PerformanceRating)
attrtion_edit$RelationshipSatisfaction <- as.factor(attrtion_edit$RelationshipSatisfaction)
attrtion_edit$StockOptionLevel <- as.factor(attrtion_edit$StockOptionLevel)
attrtion_edit$TotalWorkingYears <- as.factor(attrtion_edit$TotalWorkingYears)
attrtion_edit$TrainingTimesLastYear <- as.factor(attrtion_edit$TrainingTimesLastYear)
attrtion_edit$WorkLifeBalance <- as.factor(attrtion_edit$WorkLifeBalance)
attrtion_edit$YearsAtCompany <- as.factor(attrtion_edit$YearsAtCompany)
attrtion_edit$YearsInCurrentRole <- as.factor(attrtion_edit$YearsInCurrentRole)
attrtion_edit$YearsSinceLastPromotion <- as.factor(attrtion_edit$YearsSinceLastPromotion)
attrtion_edit$YearsWithCurrManager <- as.factor(attrtion_edit$YearsWithCurrManager)

str(attrtion_edit)

############################################## Model Building #############################################

## creating testing and training datasets

s <- sample(1:nrow(attrtion_edit), 0.8*nrow(attrtion_edit))

train <- attrtion_edit[s,]
test <- attrtion_edit[-s,]


library(rpart)
library(rpart)


model1 <- rpart(Attrition~. , data = train, method = 'class')

plot(model1)

text(model1)

