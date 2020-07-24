library(docstring)


source("DataWrangling/CleaningSalary.R")
source("DataWrangling/cleaningSeasonMetrics.R")

createCompleteDataframe <- function(playerSalaryData, playerSeasonMetricsData){
  #' @description This function creates a complete and clean dataframe
  #'
  #' @return Complete Dataframe
  #' 

  cleanPlayerSalay_2017 <- cleanPlayerSalary(playerSalary=playerSalaryData)
  
  
  # Get Clean Metric Dataset
  cleanPlayerSeasonMetrics_2017 <- cleanPlayerSeasonMetrics(playerSeasonMetrics=playerSeasonMetricsData,
                                                            subsetYear=2017
                                                            )
  
  completeDataframe <- merge(x = cleanPlayerSeasonMetrics_2017, 
                                             y = cleanPlayerSalay_2017[,c("Player", "Salary")], 
                                             by = "Player", 
                                             all.x=TRUE
                                             )
  
  # Remove NAs (Players in metrics but not in salary dataset)
  completeDataframe<-completeDataframe[complete.cases(completeDataframe), ]
  
  return(completeDataframe)
}

createCompleteDataframeTotal <- function(playerSalaryData, playerSeasonMetricsData,year_start,year_end){
  #' @description This function creates a complete and clean dataframe
  #'
  #' @return Complete Dataframe
  #' 
  
  cleanPlayerSalayDataframe <- cleanPlayerSalaryTotal(playerSalaryData, 
                                                      year_start=year_start, 
                                                      year_end=year_end
                                                      )
  
  
  # Get Clean Metric Dataset
  cleanPlayerSeasonMetricsDataframe <- cleanPlayerSeasonMetricsTotal(playerSeasonMetricsData, 
                                                                     year_start=year_start, 
                                                                     year_end=year_end
                                                                     )
  
  completeDataframe <- merge(x = cleanPlayerSeasonMetricsDataframe, 
                             y = cleanPlayerSalayDataframe[,c("Player", "Salary", "Year")], 
                             by = c("Player", "Year"), 
                             all.x=TRUE
  )
  
  # completeDataframe <- left_join(cleanPlayerSalayDataframe, DF2, by=c("col1","col2")) %>%
  #   rowwise() %>%
  #   mutate(data = ifelse(between(value,min,max), data, NA)) %>%
  #   select(-min, -max)
  # )
  
  # Remove NAs (Players in metrics but not in salary dataset)
  completeDataframe<-completeDataframe[complete.cases(completeDataframe), ]
  
  return(completeDataframe)
}

# Testing
# # Load Data (Player Salary: 2017)
# playerSalaryData<-read.csv("Data/PlayerSalary_Season/salaries_1985to2018.csv", stringsAsFactors = FALSE)
# 
# # Load Data (Player Season Metrics)
# playerSeasonMetricsData<-read.csv("Data/PlayerMetrics_Season/player_metric_season.csv", stringsAsFactors = FALSE)
# 
# 
# test <- createCompleteDataframeTotal(playerSalaryData=playerSalaryData, playerSeasonMetricsData=playerSeasonMetricsData)
# 






