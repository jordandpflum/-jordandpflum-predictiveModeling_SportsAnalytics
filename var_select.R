rm(list = ls())
data <- read.csv("C:/Users/timot/Documents/Github/predictiveModelingSportsAnalytics/2017_player_salary_and_metrics.csv", header = T)

# This code uses stepwide-regression to find the best variables for multiple linear regression

# Still need to do: lasso/ridge/maybe PCA&PCR/fit KNN


# ONLY LOOK AT STATS and remove columns that have no info
edit <- data[,c(6:53,58)]
rownames(edit) <- data$player_id

# Create dummy variable for each team/position
tm <- factor(edit$Tm)
pos <- factor(edit$Pos)
dummies.1 <- model.matrix(~tm)
dummies.1 <- dummies.1[,-c(1)]
dummies.2 <- model.matrix(~pos)
dummies.2 <- dummies.2[,-c(1)]

edit$posC <- ifelse(edit[,1] == "C",1,0)
edit$tmATL <- ifelse(edit[,3] == "ATL",1,0)
edit <- cbind(edit,dummies.1)
edit <- cbind(edit,dummies.2)

edit <- edit[,-c(1,3)]


# Write file
#write.csv(edit, "C:/Users/timot/Desktop/2017_sm_with_dummy.csv")


# try to compensate for overfit
#set.seed(1)
set.seed(2)
#set.seed(3)
train <- sample(1:nrow(edit),size = 250)
train.data <- edit[train,]
test.data <- edit[-train,]
model.1 <- lm(salary~.,data = train.data)

library(MASS)

# Stepwise linear regression with AIC and BIC
# large MSEs due to the large range of salaries
# some ppl have a ton of money, some have no money
# but everyone has similar stats

# Using AIC as criteria
# has many variables but some without significance

steps1 <- stepAIC(model.1, direction = "both", k = 2)

# Fit MLR model
final.AIC <- lm(salary~Age + GS + MP + TRB_perc + STL_perc + BLK_perc + USG_perc + 
                  OWS + DWS + OBPM + BPM + VORP + FG + FGA + X3P + X3PA + X3P_perc + 
                  FT + FTA + DRB + AST + PF + tmATL + tmGSW + tmMIA + tmPOR + 
                  tmUTA + posPG + tmDEN, data = train.data)
# Has an adjusted R-squared of 0.62
summary(final.AIC)
y.hat <- predict(final.AIC,newdata = test.data)
MSE.AIC <- mean((test.data$salary-y.hat)**2)
MSE.AIC

# Using BIC as the criteria

# Has only 5 variables, all high significance.
# Age, GS, FGA, DRB, PF
steps2 <- stepAIC(model.1, direction = "both", k = log(nrow(edit)))


# Fit MLR model
final.BIC <- lm(salary~Age + GS + USG_perc + BPM + VORP + FTA + DRB + AST + 
                  PF + tmPOR, data = train.data)
summary(final.BIC)
# Has an adjusted R-squared of 0.5965

y.hat <- predict(final.BIC,newdata = test.data)
MSE.BIC <- mean((test.data$salary-y.hat)**2)
MSE.BIC