library(dplyr)


# Clear Workspace
#rm(list=ls())

# Load Data (Player Salary: 1985-2018)
player_salary_1985_2018<-read.csv("Data/PlayerSalary_Season/salaries_1985to2018.csv", stringsAsFactors = FALSE)

cleanPlayerSalary <- function(playerSalary_csv, subsetYear, rmNA=TRUE, rmDuplicates=TRUE, exportCSV=FALSE){
  # Subset Salary Data (Year = subsetYear)
  playerSalary_subset <- subset(player_salary_1985_2018,season_start==subsetYear)
  # Remove NAs
  if(rmNA){
    # Count NAs
    countNAs<-FALSE
    if(countNAs){
      totalNAs <- 0
      for(col in names(playerSalary_subset)){
        totalNAs <- totalNAs + sum(is.na(playerSalary_subset$col))
      }
    }
    na.omit(playerSalary_subset)
  }
  # Consolidate Duplicate PlayerIDs by sum(Salary)
  playerSalary_subset <- playerSalary_subset %>% 
    group_by(player_id) %>% 
    summarise(salary = sum(salary))
  
  
  # Return Clean Dataframe
  return(data.frame(playerSalary_subset))
}
cleanPlayerSalary_2017 <- cleanPlayerSalary(playerSalary_csv=player_salary_1985_2018,
                                   subsetYear=2017, 
                                   rmNA=TRUE, 
                                   rmDuplicates=TRUE, 
                                   exportCSV=FALSE
                                   )

# Confirm Duplicates are Gone
cleanPlayerSalary_2017[duplicated(cleanPlayerSalary_2017$player_id),]


# rm(list=ls())
# player_salary_1985_2018<-read.csv("D:/rs/Data/PlayerSalary_Season/salaries_1985to2018.csv", stringsAsFactors = FALSE)
# edit <- subset(player_salary_1985_2018,season_start==2017)
# #write.csv(edit,"D:/rs/Data/PlayerSalary_Season/salaries_2017.csv")
# ## Find null value in the table
# is.na(edit)
# mydata[!complete.cases(mydata),]
# 
# ## Find duplicates because they were traded
# playerSalary_2017[duplicated(playerSalary_2017$player_id),]
# ## Find duplicate IDs
# count=1
# countd=1
# duplist<-c()
# while (count<551){
#   if (dup$player_id[countd]==playerSalary_2017$player_id[count]){
#     duplist<- c(duplist,count)
#     countd=countd+1
#   }
#   count=count+1
# }
# ## Find unduplicated duplicates IDs
# dupn=dup$player_id[!duplicated(dup$player_id)]
# ## Find the amount of total salary of each ID
# ama<-c()
# count=1
# countd=1
# amount=0
# while (countd<30){
#   while (count<552){
#     if (playerSalary_2017$player_id[count]==dupn[countd]){
#       amount=amount+playerSalary_2017$salary[count]
#     }
#     count=count+1
#   }
#   ama<-c(ama,amount)
#   countd=countd+1
#   amount=0
#   count=1
# }
# ##Add the total salary to the IDs
# count=1
# countd=1
# while (count<552){
#   while (countd<30){
#     if (playerSalary_2017$player_id[count]==dupn[countd]){
#       playerSalary_2017$salary[count]=ama[countd]
#     }
#   countd=countd+1
#   }
#   count=count+1
# }
# ##Delete all duplicates
# playerSalary_2017n=playerSalary_2017[!duplicated(playerSalary_2017$player_id),]