library(docstring)


source("DataWrangling/CleaningSalary.R")
source("DataWrangling/cleaningSeasonMetrics.R")

createCompleteDataframe <- function(playerSalaryData, playerSeasonMetricsData){
  #' @description This function creates a complete and clean dataframe
  #'
  #' @return Complete Dataframe
  #' 

  print("here")
  cleanPlayerSalay_2017 <- cleanPlayerSalary(playerSalary=playerSalaryData)
  
  
  # Get Clean Metric Dataset
  cleanPlayerSeasonMetrics_2017 <- cleanPlayerSeasonMetrics(playerSeasonMetrics=playerSeasonMetricsData,
                                                            subsetYear=2017
                                                            )
  
  completeDataframe <- playerSalary <- merge(x = cleanPlayerSeasonMetrics_2017, 
                                             y = cleanPlayerSalay_2017[ , c("Player", "Salary")], 
                                             by = "Player", 
                                             all.x=TRUE
                                             )
  # Remove NAs (Players in metrics but not in salary dataset)
  completeDataframe<-completeDataframe[complete.cases(completeDataframe), ]
  
  return(completeDataframe)
}

# test <- createCompleteDataframe()







