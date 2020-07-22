library(dplyr)
library(docstring)



cleanPlayerSalary <- function(playerSalary){
  #' @description This function cleans the salary CSV, dealing with
  #' player's who were traded
  #'
  #' 
  #' @param playerSalary dataframe. Read in salary_2017.csv file
  #' @return dataframe. A clean and complete salaries dataframe
  #' 
  
  
  # Consolidate Duplicate PlayerIDs by sum(Salary)
  playerSalary_subset <- playerSalary %>% 
    group_by(Player) %>% 
    summarise(Salary = sum(Salary))
  
  
  # Return Clean Dataframe
  return(data.frame(playerSalary_subset))
}

# Testing

# # Load Data (Player Salary: 1985-2018)
# playerSalay_2017<-read.csv("Data/PlayerSalary_Season/salary_2017.csv", stringsAsFactors = FALSE)
# 
# cleanPlayerSalary_2017 <- cleanPlayerSalary(playerSalary=playerSalay_2017)
# 
# # Confirm Duplicates are Gone
# cleanPlayerSalary_2017[duplicated(cleanPlayerSalary_2017$player_id),]




# 
# cleanPlayerSalary2 <- function(playerSalary, subsetYear, rmNA=TRUE, rmDuplicates=TRUE, exportCSV=FALSE){
#   
#   # Merge Dataframes to get player name
#   player_careerMetrics<-read.csv("Data/PlayerSalary_Season/players_career_metrics.csv", stringsAsFactors = FALSE)
#   playerSalary <- merge(x = playerSalary, y = player_careerMetrics[ , c("player_id", "Player")], by = "player_id", all.x=TRUE)
#   
#   # Subset Salary Data (Year = subsetYear)
#   playerSalary_subset <- playerSalary %>% 
#     filter(season_start==2017)
#   
#   # Consolidate Duplicate PlayerIDs by sum(Salary)
#   playerSalary_subset <- playerSalary_subset %>% 
#     group_by(Player) %>% 
#     summarise(salary = sum(salary))
#   
#   
#   # Return Clean Dataframe
#   return(data.frame(playerSalary_subset))
# }