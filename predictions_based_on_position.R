rm(list=ls())
library(kknn) ## knn library
library('fastDummies')
library(leaps)
player_2017_salary_metrics<-read.csv("2017_player_salary_and_metrics.csv", stringsAsFactors = FALSE)

Perform_Linear_regression <- function(pos_dataset){
    #' @description This function performs a linear regression using
    #' the most relevant variables found by stepwise algorithm
    #' on the different datasets provided 
    #'
    #' 
    #' @param pos_dataset dataframe. 
    pos_dataset[,c("Pos")] <- list(NULL)
    set.seed(123)
    n=dim(pos_dataset)[1]
    ind <- sample(1:n,size=n*0.80,replace = FALSE)
    train = pos_dataset[ind,]
    test = pos_dataset[-ind,]
    print(dim(train))
    model_base <- lm(salary ~ 1 , data= train)  # base intercept only model
    model_all <- lm(salary ~ . , data= train) # full model with all predictors
    stepMod <- step(model_base, scope = list(lower = model_base, upper = model_all), direction = "both", trace = 0, steps = 1000)  # perform step-wise algorithm
    shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
    shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept
    myForm <- as.formula(paste("salary ~ ", paste (shortlistedVars, collapse=" + "), sep=""))
    len = length(shortlistedVars)
    #Conditional here because PF dataset has more predictors than observations
    if(len <40){
        model <- lm(myForm,data = train)
        print(summary(model))
    }
    else{
        #For PF_dataset using the best predictors using entire datset
        model <- lm(salary ~ PTS+Age+Tm_MIN+GS+DRB_perc+PF+Tm_POR+Tm_MIA+Tm_UTA+Tm_DEN+AST+TOV+TS_perc,data = train)
        print(summary(model))
    }
    
    
}

Perform_predictions <- function(pos_dataset, kcv, plot_name){
    #' @description This function performs a knn regression using
    #' kcv on the different datasets (by position)
    #'
    #' 
    #' @param pos_dataset dataframe
    #' @param kcv Do cross-fold validation.
    #' @param plot_name Used to give plot appropriate name
    
    #Get rid of salary and pos columns 
    pos_dataset[,c("Pos")] <- list(NULL)
    print(summary(pos_dataset$salary))
    Salary <- log(pos_dataset$salary)
    pos_dataset[,c("salary")] <- list(NULL)
    print(summary(Salary))
    #Normalize data
    normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
    normalized_dataset <- as.data.frame(lapply(pos_dataset[, 1:75], normalize))
    normalized_dataset=normalized_dataset[colSums(!is.na(normalized_dataset)) > 0]
    n=dim(pos_dataset)[1]
    #Set seed to guarantee same results
    set.seed(123)
    train = data.frame(Salary,normalized_dataset)
    test = data.frame(Salary,normalized_dataset)
    n=dim(normalized_dataset)[1]

    n0 = round(n/kcv,0) #Size of each fold
    
    #Different values of neighbors
    kk <- 1:40
    
    #MSE matrix
    out_MSE = matrix(0,
                     nrow = kcv, #number of rows
                     ncol = length(kk)) #number of columns
    #Vector of indices that have already been used inside the for
    used = NULL
    
    #The set of indices not used (will be updated removing the used)
    set = 1:n
    
    for(j in 1:kcv){
        
        if(n0<length(set)){ #If the set of 'not used' is > than the size of the fold
            val = sample(set, size = n0) #then sample indices from the set
        }
        
        if(n0>=length(set)){ #If the set of 'not used' is <= than the size of the fold
            val=set #then use all of the remaining indices as the sample
        }
        
        #Create the train and test matrices
        train_i = train[-val,] #Every observation except the ones whose indices were sampled
        test_i = test[val,] #The observations whose indices sampled
        
        for(i in kk){
            
            #The current model
            near = kknn(Salary~., #The formula
                        train = train_i, #The train matrix/df
                        test = test_i, #The test matrix/df
                        k=i, #Number of neighbors
                        kernel = "rectangular") #Type of kernel (see help for more)
            
            #Calculating the MSE of current model
            aux = mean((test_i[,1]-near$fitted)^2)
            
            #Store the current MSE
            out_MSE[j,i] = aux
        }
        
        #The union of the indices used currently and previously
        used = union(used,val)
        
        #The set of indices not used is updated
        set = (1:n)[-used]
        
        #Printing on the console the information that you want
        #Useful to keep track of the progress of your loop
        cat(j,"folds out of",kcv,'\n')
    }
    
    
    #Calculate the mean of MSE for each k
    mMSE = apply(out_MSE, #Receive a matrix
                 2, #Takes its columns (it would take its rows if this argument was 1)
                 mean) #And for each column, calculate the mean
    
    par(mfrow=c(1,1)) #Redimension plot window to 1 row, 1 column

    #Complexity x RMSE graph
    plot(log(1/kk),sqrt(mMSE), #the values
         xlab="Complexity (log(1/k))",
         ylab="out-of-sample RMSE",
         main = plot_name,
         col=4, #Color of line
         lwd=2, #Line width
         type="l", #Type of graph = line
         cex.lab=1.2) #Size of labs

    #Find the index of the minimum value of mMSE
    best = which.min(mMSE)
    print(min(sqrt(mMSE[best])))

    #Inclusing text at specific coordinates of the graph
    text(log(1/kk[best]),sqrt(mMSE[best])+0.01, #Coordinates
         paste("k=",kk[best]),#The actual text
         col=2, #Color of the text
         cex=1.2) #Size of the text
    text(log(1/40)+.2,sqrt(mMSE[40]),"k=40")
    text(log(1/1)-0.5,sqrt(mMSE[1])+0.001,"k=1")

    ind <- sample(1:n,size=n*0.85,replace = FALSE)
    n=dim(normalized_dataset)[1]
    train = data.frame(Salary,normalized_dataset)
    test = data.frame(Salary,normalized_dataset)
    train = train[ind,]
    test = test[-ind,]
    near = kknn(Salary~., #The formula
                train = train, #The train matrix/df
                test = test, #The test matrix/df
                k=best, #The number of neighbors
                kernel = "rectangular") #Type of kernel (see options in the help section)

    #Calculating the MSE for the current model
    aux = mean((test[,1]-near$fitted)^2)
    
}
cleanPlayerSalary <- function(player_csv){
    #' @description This function subsets the dataframe by position
    #' and calls other function to perform the predictions
    #'
    #' 
    #' @param player_csv the entire dataset
    #' @return Returns clean player_csv. 
    
    #Hot one-encoding for Team
    player_csv <- dummy_cols(player_csv, select_columns = 'Tm', remove_first_dummy = TRUE)
    #Drop the text and duplicate columns
    player_csv[,c("X", "player_id", "COLID", "Tm", "firstName", "lastName", "player_Year", "league", "season", "season_end", "season_start", "Year", "Player", "team")] <- list(NULL)
    #Subset the dataframe by position
    SG_dataset  <- subset(player_csv, Pos == "SG")
    C_dataset  <- subset(player_csv, Pos == "C")
    PF_dataset  <- subset(player_csv, Pos == "PF")
    SF_dataset  <- subset(player_csv, Pos == "SF")
    PG_dataset  <- subset(player_csv, Pos == "PG")
    #Perform knn regression
    Perform_predictions(SG_dataset, 5, "Shooting Guard knn")
    Perform_predictions(C_dataset, 5, "Center knn")
    Perform_predictions(PF_dataset, 5, "Power Forward knn")
    Perform_predictions(SF_dataset, 5, "Small Forward knn")
    Perform_predictions(PG_dataset, 5, "Point Guard knn")
    #Perform linear regression
    Perform_Linear_regression(player_csv)
    Perform_Linear_regression(SG_dataset)
    Perform_Linear_regression(C_dataset)
    Perform_Linear_regression(PF_dataset)
    Perform_Linear_regression(SF_dataset)
    Perform_Linear_regression(PG_dataset)
    

    return (player_csv)
}
player_csv <- cleanPlayerSalary(player_2017_salary_metrics)