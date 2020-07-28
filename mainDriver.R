rm(list=ls())

# Dataframe Creation
source("DataWrangling/createCompleteDataframe.R")

# Load Data (Player Salary)
playerSalaryData<-read.csv("Data/PlayerSalary_Season/salaries_1985to2018.csv", stringsAsFactors = FALSE)

# Load Data (Player Season Metrics)
playerSeasonMetricsData<-read.csv("Data/PlayerMetrics_Season/player_metric_season.csv", stringsAsFactors = FALSE)



completeDataframe <- createCompleteDataframeTotal(playerSalaryData=playerSalaryData, 
                                                  playerSeasonMetricsData=playerSeasonMetricsData,
                                                  year_start=2010,
                                                  year_end=2017)

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
colnames(xDF) <- variables

# Create Y-DF
variables = c("Salary")
yDF = createParitalDataframe(df = completeDataframe, colNames = variables)



summary(yDF)
boxplot(yDF)
plot(range(length((yDF))),yDF)

# Variable Selection
source("Prediction/predictions_based_on_position.R")

# Group (position)

# Group (etc.)

# Create data frame (for easier use)

player_csv <- completeDataframe
player_csv[,c( "Tm", "Player", "Salary")] <- list(NULL)


Perform_Linear_regression(player_csv,"Overall")


# Variable Selection
source("VariableSelection/variable_selectionfunction.R")

Perform_penaltyreg(player_csv,a = 1)
Perform_penaltyreg(player_csv,a = 0)


# Prediction
player_csv <- cleanPlayerSalary(completeDataframe)

# Analysis
# Accuracy
# Significant Variable Analysis
# Case Study
