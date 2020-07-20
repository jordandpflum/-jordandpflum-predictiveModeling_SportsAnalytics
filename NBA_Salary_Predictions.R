rm(list=ls())
player_salary_1985_2018<-read.csv("Data/PlayerSalary_Season/salaries_1985to2018.csv", stringsAsFactors = FALSE)
player_metrics_season<-read.csv("Data/PlayerMetrics_Season/player_metric_season.csv", stringsAsFactors = FALSE)
#both datasets do have unique id but with different names
#Renaming it to player_id 
#player_metrics_season2017<- subset(player_metrics_season, Year == 2017)


#names(player_stats_1985_2018)[names(player_stats_1985_2018) == 'X_id'] <- 'player_id'

#player_salary_stats_1985_2018 <- merge(player_salary_1985_2018,player_stats_1985_2018,by="player_id")

# Single out 2017
edit <- subset(player_metrics_season, Year == 2017)

# Remove NA columns
edit[,c("blank2","blanl")] <- list(NULL)

player<-edit$Player
edit$firstName <- sapply(strsplit(edit$Player, ' '), function(x) x[1]) #Get first name
edit$lastName <- sapply(strsplit(edit$Player, ' '), function(x) x[length(x)]) #Get last name
edit$player_id <- apply(edit[,c('firstName', 'lastName')], 1, function(x) { paste(substring(tolower(x[2]),1,5),substring(tolower(x[1]),1,2),"01", sep = "", collapse = "") }) #Create unique id
edit$player_Year <- apply(edit[,c('player_id', 'Year')], 1, function(x) { paste(x[1],x[2], sep = "", collapse = "") })
edit$player_id <- apply(edit[,c('player_id', 'Tm', 'Year')], 1, function(x) { paste(x[1],x[2],x[3], sep = "", collapse = "") })

# The ID/Names of people have multiple observations because they were traded
ind <- which(duplicated(edit$player_Year))
length(unique(edit$Player[ind]))
edit <- edit[-c(ind),]
# The ID/Names of people with straight up missing stats AND weren't traded


full <- complete.cases(edit)       # find the players with all of their stats
ind2 <- which(full == FALSE)       # find which players dont have all their stats

edit <- edit[-c(ind2),]             # edit now has all unique players and players
                                    #with all the stats


player_salary_2017 <- subset(player_salary_1985_2018, season_end == 2017)
length(unique(player_salary_2017$player_id))

full_team=sort(unique(player_salary_2017$team))
abbr_team = sort(unique(edit$Tm))
abbr_team=replace(abbr_team, c(4, 5), abbr_team[c(5, 4)])
abbr_team = abbr_team[abbr_team!='TOT']
team_matrix=cbind(full_team, abbr_team)

player_salary_2017$player_id <- apply(player_salary_2017[,c('player_id', 'team', 'season_end')], 1, function(y) {paste(y[1], team_matrix[match(c(y[2]),team_matrix),2],y[3], sep = "", collapse = "")})


df <- merge(edit,player_salary_2017,by="player_id")

