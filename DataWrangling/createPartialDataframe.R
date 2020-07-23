



createParitalDataframe <- function(df, colNames){
  df_new <- df %>% 
    select(.dots = all_of(colNames))
  
  return(df_new)
}

