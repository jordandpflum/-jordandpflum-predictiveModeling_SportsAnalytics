library(dplyr)





createAgeGroupingDataframe <- function(df, ageGroup){
  df_new <- df %>% mutate(ageGroups = case_when(Age <= 22 ~ '18 - 22',
                                            Age > 23 & Age <= 26 ~ '23 - 26',
                                            Age > 26 & Age <= 30 ~ '27 - 30',
                                            Age > 30 & Age <= 35 ~ '31 - 35',
                                            Age > 35 ~ 'Over 35'
                                            )
                      ) %>% filter(ageGroups == ageGroup)
  return(df_new)
}


