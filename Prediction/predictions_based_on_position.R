library(kknn) ## knn library
library('fastDummies')
library(leaps)
library(docstring)
library(nnet)
library(randomForest)
library(gbm)

player_2017_salary_metrics<-read.csv("2017_player_salary_and_metrics.csv", stringsAsFactors = FALSE)


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
    #Print losses
    # print(cbind(parmrf,ilrf, olrf))
    
    #Boosting trees
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
    thebpred = predict(theb,newdata=test,n.trees=parmb[iib,2]) #Get the prediction for the validation set
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
    #print(pred_year)
    #print(actualSal_year)
    SE=sum(((actualSal_year$Salary)-(pred_year$Salary))^2)
    RMSE=round(sqrt(SE/nrow(actualSal_year)),3)
    print(RMSE)
    #Print losses
    #print(cbind(parmb,olb,ilb))

    
    #Results no grouping
    # tdepth ntree   lam     olb        ilb
    # 1      4  1000 0.001 5275090 5085007.13
    # 2     10  1000 0.001 5130949 4911225.99
    # 3      4  5000 0.001 4178061 3840245.47
    # 4     10  5000 0.001 4031305 3539281.22
    # 5      4  1000 0.200 4932238 1482678.51
    # 6     10  1000 0.200 4389668  169447.29
    # 7      4  5000 0.200 4693875   35362.97
    # 8     10  5000 0.200 4428822       1.31
    print(name)
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
    SE=sum(((actualSal_year$Salary)-(pred_year$Salary))^2)
    RMSE=round(sqrt(SE/nrow(actualSal_year)),3)
    print(RMSE)
    
    # MSE <- mean((exp(test$Salary)-exp(y.hat))**2)
    # print(sqrt(MSE))
    # print(name)

    
    
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
    player_csv[,c("X", "player_id", "COLID", "Tm", "firstName", "lastName", "player_Year", "league", "season", "season_end", "season_start", "Player", "team", "Salary")] <- list(NULL)
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
    # Trees(player_csv, "Overall")
    # Trees(SG_dataset, "Shooting Guard")
    # Trees(C_dataset, "Center")
    # Trees(PF_dataset, "Power Forward")
    # Trees(SF_dataset, "Small Forward")
    # Trees(PG_dataset, "Point Guard")
    
    #RMSE CALCULATED USING SALARY CAP PERCENTAGES - OUT OF SAMPLE - Boosting Trees
    # 2852859 - "No Grouping"
    # 3257454 - "Shooting Guard"
    # 3885828 - "Center"
    # 2139504 - "Power Forward"
    # 3506058 - "Small Forward"
    # 2909815 - "Point Guard"
    
    #RMSE CALCULATED USING SALARY CAP PERCENTAGES - OUT OF SAMPLE - Multi Linear Regression
    # 3060917 - "No Grouping"
    # 3582381 - "Shooting Guard"
    # 4042490 - "Center"
    # 2285300 - "Power Forward"
    # 3576691 - "Small Forward"
    # 2621779 - "Point Guard"
    
    
    
    
    #RMSE CALCULATED USING SALARY COLUMN
    #RSME for different models
    #Trees Result (Point Guard Boosting)
    #tdepth ntree   lam     olb         ilb
    # 1      4  1000 0.001 5506735 4829594.530
    # 2     10  1000 0.001 5303531 4554384.146
    # 3      4  5000 0.001 4245805 3353807.654
    # 4     10  5000 0.001 3938456 2554879.939
    # 5      4  1000 0.200 4020126   26254.172
    # 6     10  1000 0.200 3713437    1881.572
    # 7      4  5000 0.200 4117787       0.000
    # 8     10  5000 0.200 4171727       0.000
    #Random Forest(Point Guard)
    # 1   47   100 4073006 4405797
    # 2    7   100 4266135 4596289
    # 3   47   500 4067542 4385697
    # 4    7   500 4263556 4621702
    
    
    #Trees Result (Small Forward Boosting)
    # tdepth ntree   lam     olb         ilb
    # 1      4  1000 0.001 5862465 4994789.269
    # 2     10  1000 0.001 5705319 4705288.361
    # 3      4  5000 0.001 4509738 3347267.917
    # 4     10  5000 0.001 4379000 2530689.765
    # 5      4  1000 0.200 4192193   21057.625
    # 6     10  1000 0.200 4688942    1331.720
    # 7      4  5000 0.200 4931656       0.001
    # 8     10  5000 0.200 4460381       0.000
    #Random Forest(Small Forward)
    # mtry ntree    ilrf    olrf
    # 1   47   100 4269703 4879056
    # 2    7   100 4332129 4908045
    # 3   47   500 4236599 4798365
    # 4    7   500 4317717 4925752
    
    
    #Trees Result (Power Forward Boosting)
    # tdepth ntree   lam     olb         ilb
    # 1      4  1000 0.001 3729822 4959034.839
    # 2     10  1000 0.001 3609930 4678861.142
    # 3      4  5000 0.001 3011802 3282727.411
    # 4     10  5000 0.001 3017894 2532943.570
    # 5      4  1000 0.200 3867109   35008.983
    # 6     10  1000 0.200 3773858     897.580
    # 7      4  5000 0.200 3698884       0.004
    # 8     10  5000 0.200 3968908       0.000
    #Random Forest(Power Forward)
    # mtry ntree    ilrf    olrf
    # 1   47   100 4146855 3203545
    # 2    7   100 4238444 3317751
    # 3   47   500 4121224 3198080
    # 4    7   500 4252930 3281175
    
    
    #Trees Result (Center Boosting)
    # tdepth ntree   lam     olb         ilb
    # 1      4  1000 0.001 6018251 5116536.092
    # 2     10  1000 0.001 5913308 4820797.738
    # 3      4  5000 0.001 5171775 3515316.427
    # 4     10  5000 0.001 5111544 2696009.660
    # 5      4  1000 0.200 5257364   43840.728
    # 6     10  1000 0.200 5259646     655.658
    # 7      4  5000 0.200 4952887       0.003
    # 8     10  5000 0.200 5115411       0.000
    #Random Forest(Center)
    # mtry ntree    ilrf    olrf
    # 1   47   100 4489011 5328404
    # 2    7   100 4660426 5470666
    # 3   47   500 4429075 5287558
    # 4    7   500 4663364 5473414
    
    
    #Trees Result (Shooting Guard Boosting)
    # tdepth ntree   lam     olb         ilb
    # 1      4  1000 0.001 5169419 4841689.688
    # 2     10  1000 0.001 5039949 4582721.297
    # 3      4  5000 0.001 4151157 3396990.997
    # 4     10  5000 0.001 4038905 2632023.530
    # 5      4  1000 0.200 4291002   41129.921
    # 6     10  1000 0.200 3976290     315.819
    # 7      4  5000 0.200 4277914       0.001
    # 8     10  5000 0.200 3958650       0.000
    #Random Forest(Shooting Guard)
    # mtry ntree    ilrf    olrf
    # 1   47   100 4291789 4442072
    # 2    7   100 4378622 4542887
    # 3   47   500 4255561 4377670
    # 4    7   500 4339349 4500589

    return (player_csv)
}

