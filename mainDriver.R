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


# Create X-DF
source("DataWrangling/createPartialDataframe.R")
variables = c("Pos", "Age", "G", "GS", "MP", "PTS", "AST", "TRB", "ORB", "DRB",
              "STL", "BLK", "TOV", "PF", "FG", "FGA", "X2P", "X2PA", "X3P",
              "FT", "FTA", "PER", "ORB_perc", "DRB_perc", "TRB_perc", "AST_perc",
              "STL_perc", "BLK_perc", "TOV_perc", "USG_perc", "OWS", "DWS",
              "WS", "WS_48", "OBPM", "DBPM", "BPM", "VORP", "TSA", "TS_perc",
              "X3P_perc", "X2P_perc", "eFG_perc", "FT_perc")
xDF = createParitalDataframe(df = completeDataframe, colNames = variables)

# Create Y-DF
variables = c("Salary")
yDF = createParitalDataframe(df = completeDataframe, colNames = variables)

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


