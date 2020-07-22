rm(list=ls())

# Dataframe Creation
source("DataWrangling/createCompleteDataframe.R")



# Load Data (Player Salary: 2017)
playerSalay_2017<-read.csv("Data/PlayerSalary_Season/salary_2017.csv", stringsAsFactors = FALSE)

# Load Data (Player Season Metrics)
playerSeasonMetrics<-read.csv("Data/PlayerMetrics_Season/player_metric_season.csv", stringsAsFactors = FALSE)

completeDataframe <- createCompleteDataframe(playerSalaryData=playerSalay_2017, playerSeasonMetricsData=playerSeasonMetrics)

# Grouping
# Gruping by Position

# Grouping by Age.
source("Classification/createAgeGroupingDataframe.R")
ageGroupedDataframe_18_22 <- createAgeGroupingDataframe(completeDataframe, ageGroup='18 - 22')
ageGroupedDataframe_23_26 <- createAgeGroupingDataframe(completeDataframe, ageGroup='23 - 26')
ageGroupedDataframe_27_30 <- createAgeGroupingDataframe(completeDataframe, ageGroup='27 - 30')
ageGroupedDataframe_31_35 <- createAgeGroupingDataframe(completeDataframe, ageGroup='31 - 35')
ageGroupedDataframe_over35 <- createAgeGroupingDataframe(completeDataframe, ageGroup='>35')

# Variable Selection
# Group (position)

# Group (etc.)

# No Grouping


# Prediction
source("Prediction/predictions_based_on_position.R")

player_csv <- cleanPlayerSalary(completeDataframe)



# Analysis
# Accuracy

# Significant Variable Analysis

# Case Study


