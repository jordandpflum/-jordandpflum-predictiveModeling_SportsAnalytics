library(kknn) ## knn library
library('fastDummies')
library(leaps)
library(docstring)
library(nnet)
library(randomForest)
library(gbm)

player_2017_salary_metrics<-read.csv("2017_player_salary_and_metrics.csv", stringsAsFactors = FALSE)

MultipleLinearRMSE = c()
RandomForestRMSE = c()
BoostingRMSE = c()

Trees <- function(pos_dataset, name){
    pos_dataset[,c("Pos")] <- list(NULL)
    # print(summary(pos_dataset$salaryPercSalaryCap))
    n=dim(pos_dataset)[1]
    set.seed(123)
    ind <- sample(1:n,size=n*0.80,replace = FALSE)
    train = pos_dataset[ind,]
    test = pos_dataset[-ind,]
    train_Year = train$Year
    test_Year = test$Year
    train[,c("Year")] <- list(NULL)
    test[,c("Year")] <- list(NULL)
    p=ncol(train)-1 #Number of covariates (-1 because one column is the response)
    mtryv = c(p,round(sqrt(p))) #Number of candidate variables for each split
    ntreev = c(100,500) #Number of trees
    parmrf = expand.grid(mtryv,ntreev) #Expanding grids of different models
    colnames(parmrf)=c('mtry','ntree')
    # print(parmrf)
    
    nset = nrow(parmrf) #Number of models
    olrf = rep(0,nset) #Out-of-sample loss
    ilrf = rep(0,nset) #In-sample loss
    rffitv = vector('list',nset) #List of the estimated models
    
    for(i in 1:nset) {
        # cat('Model ',i,' out of ',nset,'\n')
        temprf = randomForest(salaryPercSalaryCap~., #Formula
                              data=train, #Data frame
                              mtry=parmrf[i,1], #Number of candidate variables for each split
                              ntree=parmrf[i,2], #Number of trees
                              maxnodes = 15) #Maximum number of leaves (takes too much time if too big)
        ifit = predict(temprf) #In-sample prediction
        ofit=predict(temprf,newdata=test) #Out-of-sample prediction
        olrf[i] = sum(((test$salaryPercSalaryCap)-(ofit))^2) #Out-of-sample loss
        ilrf[i] = sum(((train$salaryPercSalaryCap)-(ifit))^2) #In-sample loss
        rffitv[[i]]=temprf #Saving the model
    }
    ilrf = round(sqrt(ilrf/nrow(train)),3) #In-sample RMSE
    olrf = round(sqrt(olrf/nrow(test)),3) #Out-of-sample RMSE

    iirf=which.min(olrf) #Find minimum oos loss
    therf = rffitv[[iirf]] #Get model which correspond to the minimum out-sample loss
    therfpred=predict(therf,newdata=test)

    pred_year = data.frame("Year"=test_Year, "Percentage"=therfpred)
    actualSal_year = data.frame("Year"=test_Year, "Percentage"=test$salaryPercSalaryCap)
    #Transform the percentages to actual Salary using the salary cap from each year
    pred_year=transform(pred_year, Salary=ifelse(Year==2010, Percentage*58044000,
                                                 ifelse(Year==2011, Percentage*58044000,
                                                        ifelse(Year==2012, Percentage*58044000,
                                                               ifelse(Year==2013, Percentage*58679000,
                                                                      ifelse(Year==2014, Percentage*63065000,
                                                                             ifelse(Year==2015, Percentage*70000000,
                                                                                    ifelse(Year==2016, Percentage*94143000, Percentage*99093000))))))))
    actualSal_year = transform(actualSal_year, Salary=ifelse(Year==2010, Percentage*58044000,
                                                             ifelse(Year==2011, Percentage*58044000,
                                                                    ifelse(Year==2012, Percentage*58044000,
                                                                           ifelse(Year==2013, Percentage*58679000,
                                                                                  ifelse(Year==2014, Percentage*63065000,
                                                                                         ifelse(Year==2015, Percentage*70000000,
                                                                                                ifelse(Year==2016, Percentage*94143000, Percentage*99093000))))))))
    #print(pred_year)
    #print(actualSal_year)
    #Calculate RMSE and adjusted R-squared
    SE=sum(((actualSal_year$Salary)-(pred_year$Salary))^2)
    RMSE=round(sqrt(SE/nrow(actualSal_year)),3)
    R2 = 1 - sum(((actualSal_year$Salary)-(pred_year$Salary))^2)/sum(((actualSal_year$Salary)-mean((pred_year$Salary)))^2)
    adjrsquared = 1 - (1 - R2) * ((dim(test)[1] - 1)/(dim(test)[1]-p))
    cat(RMSE," ",R2," ",name," ", adjrsquared, "\n")
    RandomForestRMSE <<- c(RandomForestRMSE, RMSE)
    #Print losses
    # print(cbind(parmrf,ilrf, olrf))
    
    # #Boosting trees
    idv = c(4,10) #tree depth
    ntv = c(1000,5000) #number of trees
    lamv=c(.001,.2) #Learning rates
    parmb = expand.grid(idv,ntv,lamv) #Expand the values to get different models
    colnames(parmb) = c('tdepth','ntree','lam')
    # print(parmb)

    nset = nrow(parmb) #Number of models
    olb = rep(0,nset) #Out-of-sample loss
    ilb = rep(0,nset) #In-sample loss
    bfitv = vector('list',nset) #List of the estimated models

    for(i in 1:nset) {
        # cat('Model ',i,'out of',nset,'\n')
        tempboost = gbm(salaryPercSalaryCap~.,#Formula
                        data=train, #Data frame
                        distribution='gaussian',
                        interaction.depth=parmb[i,1], #Maximum depth of each tree
                        n.trees=parmb[i,2], #Number of trees
                        shrinkage=parmb[i,3]) #Learning rate
        ifit = predict(tempboost,n.trees=parmb[i,2]) #In-sample fit
        ofit=predict(tempboost,newdata=test,n.trees=parmb[i,2]) #Out-of-sample fit
        olb[i] = sum(((test$salaryPercSalaryCap)-(ofit))^2) #Out-of-sample loss
        ilb[i] = sum(((train$salaryPercSalaryCap)-(ifit))^2) #In-sample loss
        bfitv[[i]]=tempboost #Saving the model
    }
    ilb = round(sqrt(ilb/nrow(train)),3) #Out-of-sample RMSE
    olb = round(sqrt(olb/nrow(test)),3) #In-sample RMSE
    iib=which.min(olb) #Find minimum oos loss
    theb = bfitv[[iib]] #Select the model with minimum oos loss
    thebpred = predict(theb,newdata=test,n.trees=parmb[iib,2])
    pred_year = data.frame("Year"=test_Year, "Percentage"=thebpred)
    actualSal_year = data.frame("Year"=test_Year, "Percentage"=test$salaryPercSalaryCap)
    #Transform the percentages to actual Salary using the salary cap from each year
    pred_year=transform(pred_year, Salary=ifelse(Year==2010, Percentage*58044000,
                                ifelse(Year==2011, Percentage*58044000,
                                ifelse(Year==2012, Percentage*58044000,
                                ifelse(Year==2013, Percentage*58679000,
                                ifelse(Year==2014, Percentage*63065000,
                                ifelse(Year==2015, Percentage*70000000,
                                ifelse(Year==2016, Percentage*94143000, Percentage*99093000))))))))
    actualSal_year = transform(actualSal_year, Salary=ifelse(Year==2010, Percentage*58044000,
                                                      ifelse(Year==2011, Percentage*58044000,
                                                     ifelse(Year==2012, Percentage*58044000,
                                                    ifelse(Year==2013, Percentage*58679000,
                                                    ifelse(Year==2014, Percentage*63065000,
                                                     ifelse(Year==2015, Percentage*70000000,
                                                    ifelse(Year==2016, Percentage*94143000, Percentage*99093000))))))))

    #Calculate RMSE and adjusted R-squared
    SE=sum(((actualSal_year$Salary)-(pred_year$Salary))^2)
    RMSE=round(sqrt(SE/nrow(actualSal_year)),3)
    R2 = 1 - sum(((actualSal_year$Salary)-(pred_year$Salary))^2)/sum(((actualSal_year$Salary)-mean((pred_year$Salary)))^2)
    adjrsquared = 1 - (1 - R2) * ((dim(test)[1] - 1)/(dim(test)[1]-p))
    cat(RMSE," ",R2," ",name," ", adjrsquared, "\n")
    BoostingRMSE <<- c(BoostingRMSE, RMSE)
    #Print losses
    #print(cbind(parmb,olb,ilb))

    #print(name)
}

Perform_Linear_regression <- function(pos_dataset, name){
    #' @description This function performs a linear regression using
    #' the most relevant variables found by stepwise algorithm
    #' on the different datasets provided 
    #'
    #' 
    #' @param pos_dataset dataframe. 
    #' @param name Identification for dataset
    pos_dataset[,c("Pos")] <- list(NULL)
    set.seed(123)
    n=dim(pos_dataset)[1]
    ind <- sample(1:n,size=n*0.80,replace = FALSE)
    train = pos_dataset[ind,]
    test = pos_dataset[-ind,]
    train_Year = train$Year
    test_Year = test$Year
    train[,c("Year")] <- list(NULL)
    test[,c("Year")] <- list(NULL)
    model_base <- lm(salaryPercSalaryCap~ 1 , data= train)  # base intercept only model
    model_all <- lm(salaryPercSalaryCap~. , data= train) # full model with all predictors
    stepMod <- step(model_base, scope = list(lower = model_base, upper = model_all), direction = "both", trace = 0, steps = 1000, k=log(length(ind)))  # perform step-wise algorithm
    shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
    shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept
    myForm <- as.formula(paste("salaryPercSalaryCap ~ ", paste (shortlistedVars, collapse=" + "), sep=""))
    len = length(shortlistedVars)
    model <- lm(myForm,data = train)
    print(summary(model))
    y.hat <- predict(model,newdata = test)
    
    pred_year = data.frame("Year"=test_Year, "Percentage"=y.hat)
    actualSal_year = data.frame("Year"=test_Year, "Percentage"=test$salaryPercSalaryCap)
    #Transform the percentages to actual Salary using the salary cap from each year
    pred_year=transform(pred_year, Salary=ifelse(Year==2010, Percentage*58044000, 
                                                 ifelse(Year==2011, Percentage*58044000,
                                                        ifelse(Year==2012, Percentage*58044000,
                                                               ifelse(Year==2013, Percentage*58679000,
                                                                      ifelse(Year==2014, Percentage*63065000,
                                                                             ifelse(Year==2015, Percentage*70000000,
                                                                                    ifelse(Year==2016, Percentage*94143000, Percentage*99093000))))))))
    actualSal_year = transform(actualSal_year, Salary=ifelse(Year==2010, Percentage*58044000, 
                                                             ifelse(Year==2011, Percentage*58044000,
                                                                    ifelse(Year==2012, Percentage*58044000,
                                                                           ifelse(Year==2013, Percentage*58679000,
                                                                                  ifelse(Year==2014, Percentage*63065000,
                                                                                         ifelse(Year==2015, Percentage*70000000,
                                                                                                ifelse(Year==2016, Percentage*94143000, Percentage*99093000))))))))
    #print(pred_year)
    #print(actualSal_year)
    #Calculate RMSE and adjusted R-squared
    SE=sum(((actualSal_year$Salary)-(pred_year$Salary))^2)
    RMSE=round(sqrt(SE/nrow(actualSal_year)),3)
    print(RMSE)
    MultipleLinearRMSE <<- c(MultipleLinearRMSE, RMSE)
    
    
    # MSE <- mean((exp(test$Salary)-exp(y.hat))**2)
    # print(sqrt(MSE))
    print(name)

    
    
}


cleanPlayerSalary <- function(player_csv){
    #' @description This function subsets the dataframe by position
    #' and calls other function to perform the predictions
    #'
    #' 
    #' @param player_csv the entire dataset
    #' @return Returns clean player_csv. 
    
    #Hot one-encoding for Team
    #player_csv <- dummy_cols(player_csv, select_columns = 'Tm', remove_first_dummy = TRUE)
    #Drop the text and duplicate columns
    player_csv[,c( "Tm", "Player", "Salary")] <- list(NULL)
    #Subset the dataframe by position
    SG_dataset  <- subset(player_csv, Pos == "SG")
    C_dataset  <- subset(player_csv, Pos == "C")
    PF_dataset  <- subset(player_csv, Pos == "PF")
    SF_dataset  <- subset(player_csv, Pos == "SF")
    PG_dataset  <- subset(player_csv, Pos == "PG")
    # #Perform linear regression
    Perform_Linear_regression(player_csv, "Overall")
    Perform_Linear_regression(SG_dataset, "Shooting Guard")
    Perform_Linear_regression(C_dataset, "Center")
    Perform_Linear_regression(PF_dataset, "Power Forward")
    Perform_Linear_regression(SF_dataset, "Small Forward")
    Perform_Linear_regression(PG_dataset, "Point Guard")
    #Perform Random Forest and Boosting
    Trees(player_csv, "Overall")
    Trees(SG_dataset, "Shooting Guard")
    Trees(C_dataset, "Center")
    Trees(PF_dataset, "Power Forward")
    Trees(SF_dataset, "Small Forward")
    Trees(PG_dataset, "Point Guard")
    
    # print(MultipleLinearRMSE)
    # print(RandomForestRMSE)
    # print(BoostingRMSE)
    
    #Plot the RMSE for each model
    barplot(MultipleLinearRMSE, main = "Multiple Linear Regression RMSE of Predicted Salaries",
            xlab = "Position",
            ylab = "Salary", 
            names.arg=c("Overall", "SG", "C", "PF", "SF", "PG"), col = "darkred")
    barplot(RandomForestRMSE, main = "Random Forest RMSE of Predicted Salaries",
            xlab = "Position",
            ylab = "Salary", 
            names.arg=c("Overall", "SG", "C", "PF", "SF", "PG"), col = "darkred")
    barplot(BoostingRMSE, main = "Boosting RMSE of Predicted Salaries",
            xlab = "Position",
            ylab = "Salary", 
            names.arg=c("Overall", "SG", "C", "PF", "SF", "PG"), col = "darkred")
    #RMSE CALCULATED USING SALARY CAP PERCENTAGES - OUT OF SAMPLE - Boosting Trees
    # 2852859 - "No Grouping"
    # 3257454 - "Shooting Guard"
    # 3885828 - "Center"
    # 2139504 - "Power Forward"
    # 3506058 - "Small Forward"
    # 2909815 - "Point Guard"
    
    #RMSE CALCULATED USING SALARY CAP PERCENTAGES - OUT OF SAMPLE - Random Forest
    # 3080980 - "Overall"
    # 3337990 - "Shooting Guard"
    # 3934175 - "Center"
    # 2230782 - "Power Forward"
    # 3625654 - "Small Forward"
    # 3366156 - "Point Guard"
    
    #RMSE CALCULATED USING SALARY CAP PERCENTAGES - OUT OF SAMPLE - Multi Linear Regression
    # 3060917 - "No Grouping"
    # 3582381 - "Shooting Guard"
    # 4042490 - "Center"
    # 2285300 - "Power Forward"
    # 3576691 - "Small Forward"
    # 3621779 - "Point Guard"
    
    return (player_csv)
}

