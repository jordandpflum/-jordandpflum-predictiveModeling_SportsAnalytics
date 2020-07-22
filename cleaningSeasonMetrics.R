library(dplyr)
library(docstring)


consolidatePos <- function(posCol){
  #' @description This function consilidates position information for player's
  #' who were traded. Used in summarize during groupby. Takes first poisiton 
  #' that appears in the dataset for the player and considers that his position
  #' throughout the season
  #'
  #' 
  #' @param posCol array. column array called in dplyer
  #' @return array. Consolidated array of positions

  # Keep only first position for player
  return(posCol[1])
}

consolidateTeam <- function(teamCol){
  #' @description This function consilidates team information for player's
  #' who were traded. Used in summarize during groupby. Combines all teams that
  #' a player appeard in during that season
  #'
  #' 
  #' @param teamCol array. column array called in dplyer
  #' @return array. Consolidated array of teams
  
  # Combine Teams if played for more than one team
  teamVal = teamCol[1]
  if(length(teamCol) > 1){
    for(team in 2:length(teamCol)){
      teamVal<-paste(teamVal, teamCol[team], sep="/")
    }
  }
    
  return(teamVal)
}

cleanPlayerSeasonMetrics <- function(playerSeasonMetrics, subsetYear){
  #' @description This function cleans the metrics CSV, dealing with
  #' player's who were traded
  #'
  #' 
  #' @param playerSeasonMetrics dataframe. Read in player_metric_season.csv file
  #' @param subsetYear int. Year to subset data
  #' @return dataframe. A clean and complete metrics dataframe
  
  
  # Subset Data (Year = subsetYear)
  playerSeasonMetrics_subset <- playerSeasonMetrics %>% 
    filter(Year==subsetYear)
  
  # Remove NAs (should be no NAs, just a precaution)
  if(rmNA){
    # Count NAs
    countNAs<-FALSE
    if(countNAs){
      totalNAs <- 0
      for(col in names(playerSeasonMetrics_subset)){
        totalNAs <- totalNAs + sum(is.na(playerSeasonMetrics_subset$col))
      }
      print(totalNAs)
    }
    na.omit(playerSeasonMetrics_subset)
  }
  
  # Consolidate Duplicate PlayerIDs by sum(Salary)
  playerSeasonMetrics_subset <- playerSeasonMetrics_subset %>% 
    group_by(Player) %>% 
    summarise(Year = mean(Year),
              Pos = consolidatePos(Pos),
              Age = max(Age),
              Tm = consolidateTeam(Tm),
              G = sum(G),
              GS = sum(GS),
              MP = sum(MP),
              PTS = sum(PTS),
              AST = sum(AST),
              TRB = sum(TRB),
              ORB = sum(ORB),
              DRB = sum(DRB),
              STL = sum(STL),
              BLK = sum(BLK),
              TOV = sum(TOV),
              PF = sum(PF),
              FG = sum(FG),
              FGA = sum(FG),
              X2P = sum(X2P),
              X2PA = sum(X2PA),
              X3P = sum(X3P),
              X3PA = sum(X3PA),
              FT = sum(FT),
              FTA = sum(FT),
              PER = mean(PER),
              ORB_perc = mean(ORB_perc),
              DRB_perc = mean(DRB_perc),
              TRB_perc = mean(TRB_perc),
              AST_perc = mean(AST_perc),
              STL_perc = mean(STL_perc),
              BLK_perc = mean(BLK_perc),
              TOV_perc = mean(TOV_perc),
              USG_perc = mean(USG_perc),
              OWS = mean(OWS),
              DWS = mean(DWS),
              WS = mean(WS),
              WS_48 = mean(WS_48),
              OBPM = mean(OBPM),
              DBPM = mean(DBPM),
              BPM = mean(BPM),
              VORP = mean(VORP)
              )
  
  # Add Additional Metrics
  # TSA
  playerSeasonMetrics_subset$TSA = playerSeasonMetrics_subset$FGA + 0.44*playerSeasonMetrics_subset$FTA
  playerSeasonMetrics_subset$TSA[is.na(playerSeasonMetrics_subset$TSA)] <- 0
  
  # TS_perc
  playerSeasonMetrics_subset$TS_perc = playerSeasonMetrics_subset$PTS / (2*playerSeasonMetrics_subset$TSA)
  playerSeasonMetrics_subset$TS_perc[is.na(playerSeasonMetrics_subset$TS_perc)] <- 0
  playerSeasonMetrics_subset$TS_perc[is.infinite(playerSeasonMetrics_subset$TS_perc)] <- 0
  
  # 3PAr
  playerSeasonMetrics_subset$X3PAr = playerSeasonMetrics_subset$X3PA / playerSeasonMetrics_subset$FGA
  playerSeasonMetrics_subset$X3PAr[is.na(playerSeasonMetrics_subset$X3PAr)] <- 0
  playerSeasonMetrics_subset$X3PAr[is.infinite(playerSeasonMetrics_subset$X3PAr)] <- 0
  
  # FTr
  playerSeasonMetrics_subset$FTr = playerSeasonMetrics_subset$FTA / playerSeasonMetrics_subset$FGA
  playerSeasonMetrics_subset$FTr[is.na(playerSeasonMetrics_subset$FTr)] <- 0
  playerSeasonMetrics_subset$FTr[is.infinite(playerSeasonMetrics_subset$FTr)] <- 0
  
  # FG_perc
  playerSeasonMetrics_subset$FG_perc = playerSeasonMetrics_subset$FG / playerSeasonMetrics_subset$FGA
  playerSeasonMetrics_subset$FG_perc[is.na(playerSeasonMetrics_subset$FG_perc)] <- 0
  playerSeasonMetrics_subset$FG_perc[is.infinite(playerSeasonMetrics_subset$FG_perc)] <- 0
  
  # X3P_perc
  playerSeasonMetrics_subset$X3P_perc = playerSeasonMetrics_subset$X3P / playerSeasonMetrics_subset$X3PA
  playerSeasonMetrics_subset$X3P_perc[is.na(playerSeasonMetrics_subset$X3P_perc)] <- 0
  playerSeasonMetrics_subset$X3P_perc[is.infinite(playerSeasonMetrics_subset$X3P_perc)] <- 0
  
  # X2P_perc
  playerSeasonMetrics_subset$X2P_perc = playerSeasonMetrics_subset$X2P / playerSeasonMetrics_subset$X2PA
  playerSeasonMetrics_subset$X2P_perc[is.na(playerSeasonMetrics_subset$X2P_perc)] <- 0
  playerSeasonMetrics_subset$X2P_perc[is.infinite(playerSeasonMetrics_subset$X2P_perc)] <- 0
  
  # eFG_perc
  playerSeasonMetrics_subset$eFG_perc = (playerSeasonMetrics_subset$FG + 0.5 * playerSeasonMetrics_subset$X3P) / playerSeasonMetrics_subset$FGA
  playerSeasonMetrics_subset$eFG_perc[is.na(playerSeasonMetrics_subset$eFG_perc)] <- 0
  playerSeasonMetrics_subset$eFG_perc[is.infinite(playerSeasonMetrics_subset$eFG_perc)] <- 0
  
  # FT_perc
  playerSeasonMetrics_subset$FT_perc = playerSeasonMetrics_subset$FT / playerSeasonMetrics_subset$FTA
  playerSeasonMetrics_subset$FT_perc[is.na(playerSeasonMetrics_subset$FT_perc)] <- 0
  playerSeasonMetrics_subset$FT_perc[is.infinite(playerSeasonMetrics_subset$FT_perc)] <- 0
  
  
  
  # Return Clean Dataframe
  return(data.frame(playerSeasonMetrics_subset))
}

# Testing

# # Load Data (Player Salary: 1985-2018)
# playerSeasonMetrics<-read.csv("Data/PlayerMetrics_Season/player_metric_season.csv", stringsAsFactors = FALSE)
# 
# # Get Clean Metric Dataset
# cleanPlayerSeasonMetrics_2017 <- cleanPlayerSeasonMetrics(playerSeasonMetrics=playerSeasonMetrics,
#                                             subsetYear=2017, 
#                                             rmNA=TRUE, 
#                                             rmDuplicates=TRUE, 
#                                             exportCSV=FALSE
#                                           )
# 
# # Confirm Duplicates are Gone
# dim(cleanPlayerSeasonMetrics_2017[rowSums(is.na(cleanPlayerSeasonMetrics_2017)) > 0,])[1] == 0
