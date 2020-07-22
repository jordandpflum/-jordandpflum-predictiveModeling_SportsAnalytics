library(docstring)

createCompleteDataframe <- function(){
  #' @description This function creates a complete and clean dataframe
  #'
  #' @return Complete Dataframe
  #' 
  
  # Load Data (Player Salary: 2017)
  playerSalay_2017<-read.csv("Data/PlayerSalary_Season/salary_2017.csv", stringsAsFactors = FALSE)
  
  cleanPlayerSalay_2017 <- cleanPlayerSalary(playerSalary=playerSalay_2017)
  
  # Load Data (Player Season Metrics)
  playerSeasonMetrics<-read.csv("Data/PlayerMetrics_Season/player_metric_season.csv", stringsAsFactors = FALSE)
  
  # Get Clean Metric Dataset
  cleanPlayerSeasonMetrics_2017 <- cleanPlayerSeasonMetrics(playerSeasonMetrics=playerSeasonMetrics,
                                                            subsetYear=2017, 
                                                            rmNA=TRUE, 
                                                            rmDuplicates=TRUE, 
                                                            exportCSV=FALSE
  )
  
  completeDataframe <- playerSalary <- merge(x = cleanPlayerSeasonMetrics_2017, 
                                             y = cleanPlayerSalary_2017[ , c("Player", "Salary")], 
                                             by = "Player", 
                                             all.x=TRUE
                                             )
  
  return(completeDataframe)
}

test <- createCompleteDataframe()







