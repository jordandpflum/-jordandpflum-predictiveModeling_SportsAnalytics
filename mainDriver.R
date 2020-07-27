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

t.dta <- cbind(edit,completeDataframe$salaryPercSalaryCap)
colnames(t.dta)[length(colnames(t.dta))] = "salary"


set.seed(50)
tr <- sample(1:nrow(edit),2400)

train <- t.dta[tr,]
test <- t.dta[-tr,]
model_base <- lm(salary~ 1 , data= train)  # base intercept only model
model_all <- lm(salary~. , data= train) # full model with all predictors
stepMod <- step(model_base, scope = list(lower = model_base, upper = model_all), direction = "both", trace = 0, steps = 1000, k=log(length(tr)))  # perform step-wise algorithm
shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept
myForm <- as.formula(paste("salary ~ ", paste (shortlistedVars, collapse=" + "), sep=""))
len = length(shortlistedVars)
model <- lm(myForm,data = train)
summary(model)
#Adj r2 0.537
y.hat <- predict(model,newdata = test)
MSE.BIC <- mean((test$salary-y.hat)**2)
sqrt(MSE.BIC)
# RMSE of .03932 perc salary cap


# LASSO

library(glmnet)
scaled <- scale(t.dta[,-c(58)])
scaled <- cbind(scaled,t.dta$salary)
tr.s <- scaled[tr,]
t.s <- scaled[-tr,]
lasso.model <- cv.glmnet(tr.s[,-c(58)],tr.s[,c(58)],alpha = 1 )
sqrt(lasso.model$cvm[lasso.model$lambda == lasso.model$lambda.1se])
#.040 perc salary cap

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
# .04063

plot(log(ridge.model$lambda),sqrt(ridge.model$cvm),
     main="LASSO CV (k=10)",xlab="log(lambda)",
     ylab = "RMSE",col=4,type="b",cex.lab=1.2)
abline(v=log(ridge.model$lambda.1se),lty=2,col=2,lwd=2)

coefs.ridge <- predict(ridge.model, type = "coefficients", s = ridge.model$lambda.1se)
coefs.ridge

# Remove all the variables

#rm(list = c("edit","lasso.model","model.1","pos","MSE.BIC","posC",
#            "scaled","ridge.model","scaled.tr","t.s","t.dta","tr",
#            "train","y.hat","test","pos.1","steps","tr.s","model"))



# Prediction
source("Prediction/predictions_based_on_position.R")
player_csv <- cleanPlayerSalary(completeDataframe)

# Analysis
# Accuracy

# Significant Variable Analysis

# Case Study

case <- data.frame(rbind(c(28,70,70,6.2,513,140,9.3,1026,900,6,24,1.2,30.6,11,3.5,.9,6.4,0)))
colnames(case) <- c("Age","G","GS","AST","TRB","TOV","FGA","X2P","X2PA","FTA","AST_perc","STL_perc","USG_perc","OWS","DWS","DBPM","VORP","posPG")
# Kevin Durant's real salary in 2017 was 25000000
y.hat <- predict(steps,newdata = case)
#y.hat <- y.hat * 99093000 #convert back to salary
e <- y.hat - 0.2522883
e

case <- rbind(case,c(28,75,75,9.1,614,303,1344,612,1002,531,41.3,1.8,30,9.8,3,1.6,7.3,0))
y.hat <- predict(steps,newdata = case[2,])
#Lebron got paid 33285709 in 2017
e <- y.hat - .3359037
e

