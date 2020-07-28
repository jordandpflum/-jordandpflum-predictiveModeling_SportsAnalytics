library(glmnet)

Perform_penaltyreg <- function(player_csv,a = 1){
  #' @description This function performs penalized regression 
  #' and returns the best lambda/MSE
  #'
  #' 
  #' @param player_csv the entire dataset
  #' @param alpha Defaults to 1 (LASSO). For Ridge use 0
  player_csv[,c("Pos","Year")] <- list(NULL)
  set.seed(123)
  n=dim(player_csv)[1]
  ind <- sample(1:n,size=n*0.80,replace = FALSE)
  scaled <- scale(player_csv[,-c(length(colnames(player_csv)))])
  scaled <- cbind(scaled,player_csv$salaryPercSalaryCap)
  colnames(scaled)[length(colnames(scaled))] <- "salaryPercSalaryCap"
  tr.s <- as.matrix(scaled[ind,])
  t.s <- as.matrix(scaled[-ind,])
  penalty.model <- cv.glmnet(tr.s[,-c(length(colnames(tr.s)))],tr.s[,c(length(colnames(tr.s)))],alpha = a)
  MSE <- sqrt(penalty.model$cvm[penalty.model$lambda == penalty.model$lambda.1se])
  print(MSE)
  plot(log(penalty.model$lambda),sqrt(penalty.model$cvm),
       main="CV (k=10)",xlab="log(lambda)",
       ylab = "RMSE",col=4,type="b",cex.lab=1.2)
  abline(v=log(penalty.model$lambda.1se),lty=2,col=2,lwd=2)
  coefs <- predict(penalty.model, type = "coefficients", s = penalty.model$lambda.1se)
  print(coefs)
}

