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