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
# Group (position)

# Group (etc.)

# No Grouping

library(MASS)

# Dummy for position
pos <- factor(xDF$Pos)
model.1 <- model.matrix(~pos)[,-c(1)]
posC <- ifelse(xDF$Pos == "C",1,0)
pos.1 <- cbind(posC,model.1)
edit <- cbind(xDF,pos.1)[,-c(1)]


# Create data frame (for easier use)

t.dta <- cbind(edit,yDF)
colnames(t.dta)[length(colnames(t.dta))] = "salary"


set.seed(40)
tr <- sample(1:nrow(edit),2400)

train <- t.dta[tr,]
test <- t.dta[-tr,]

model <- lm(salary~.,data = train)
steps <- stepAIC(model,direction = "both",k = log(nrow(t.dta)))

summary(steps)
# adj r of .5453

y.hat <- predict(steps,newdata = test)
MSE.BIC <- mean((test$salary-y.hat)**2)
sqrt(MSE.BIC)
# RMSE of 3.8 mil


# LASSO

library(glmnet)
scaled <- scale(t.dta[,-c(58)])
scaled <- cbind(scaled,t.dta$salary)
tr.s <- scaled[tr,]
t.s <- scaled[-tr,]
lasso.model <- cv.glmnet(tr.s[,-c(58)],tr.s[,c(58)],alpha = 1 )
sqrt(lasso.model$cvm[lasso.model$lambda == lasso.model$lambda.1se])
# 3.97 mil


# Plot lambdas
plot(log(lasso.model$lambda),sqrt(lasso.model$cvm),
     main="LASSO CV (k=10)",xlab="log(lambda)",
     ylab = "RMSE",col=4,type="b",cex.lab=1.2)
abline(v=log(lasso.model$lambda.1se),lty=2,col=2,lwd=2)

coefs.lasso <- predict(lasso.model, type = "coefficients", s = lasso.model$lambda.1se)
coefs.lasso

# Ridge 

ridge.model <- cv.glmnet(tr.s[,-c(58)],tr.s[,c(58)],alpha = 0)
sqrt(ridge.model$cvm[ridge.model$lambda == ridge.model$lambda.1se])
#3.98 mil


plot(log(ridge.model$lambda),sqrt(ridge.model$cvm),
     main="LASSO CV (k=10)",xlab="log(lambda)",
     ylab = "RMSE",col=4,type="b",cex.lab=1.2)
abline(v=log(ridge.model$lambda.1se),lty=2,col=2,lwd=2)

coefs.ridge <- predict(ridge.model, type = "coefficients", s = ridge.model$lambda.1se)
coefs.ridge

# Remove all the variables

rm(list = c("edit","lasso.model","model.1","pos","MSE.BIC","posC",
            "scaled","ridge.model","scaled.tr","t.s","t.dta","tr",
            "train","y.hat","test","pos.1","steps","tr.s","model"))



# Prediction
source("Prediction/predictions_based_on_position.R")

player_csv <- cleanPlayerSalary(completeDataframe)



# Exploratory Data Analysis
library(ggplot2)
library(plyr)

lildata <- ddply(completeDataframe, "Pos", summarise, grp.mean=mean(Age))
#make density plot 
ggplot(completeDataframe, aes(x=Age, color=Pos)) +
  geom_density()
# Add mean lines
p<-ggplot(completeDataframe, aes(x=Age, color=Pos)) +
  geom_density()+
  geom_vline(data=lildata, aes(xintercept=grp.mean, color=Pos),
             linetype="dashed")
p


#side by side histogram by team
max(completeDataframe$Salary)
maxindex<- which.max(completeDataframe$salaryPercSalaryCap) 



sidebyside <- function(data, team1, team2){
  #someteams <- completeDataframe[which(completeDataframe$Tm==team1),]
  #otherteams <- completeDataframe[which(completeDataframe$Tm==team2),]  
  #both <- rbind(someteams,otherteams)
  
  top5sal<- top_n(data, 20, Salary)  
  
  bothgroups<- ggplot(data=top5sal, aes(x=Tm, y=Salary, fill=Pos)) +
    geom_bar(stat="identity", position=position_dodge()) +
      ggtitle("Comparison of Salary by Team and Position")
  bothgroups
}
sidebyside(completeDataframe, 'MIA', 'HOU')#practice

teams<- c(unique(completeDataframe$Tm))
avgsal <- data.frame(Doubles=double(),
                       Ints=integer(),
                       Factors=factor(),
                       Logicals=logical(),
                       Characters=character(),
                       stringsAsFactors=FALSE)
for (x in 1:length(teams)){
  teamx <- rbind(completeDataframe[which(completeDataframe$Tm==teams[x]),])
  avg <- mean(teamx$Salary)
  avgsal[x,]<- data.frame("Tm" =teams[x], "AvgSal" = avg)
}

#THIS WORKS-----------------DENSITY PLOT WITH MEAN LINES------------------
#Parameters 
#df - dataframe
#xaxis - column of data frame that is categories for x axis boxes
#yaxis - column of data frame that is count for y axis
# labelstr - string value for the y axis label 
barchart <- function(df, xaxis, yaxis){
  attach(df)
  labelstr <- max(df$yaxis)
  bar<-ggplot(data=df, aes(x=xaxis, y=yaxis)) +
    geom_bar(stat="identity", fill="steelblue")+
    theme_minimal()
  bar
}
barchart(completeDataframe, Pos, FGA)

# Accuracy

# Significant Variable Analysis

# Case Study


