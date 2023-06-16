fix_names <- function(dataframe) {
  names(dataframe) <- tolower(names(dataframe)) %>% make.names()
  
  dataframe <- dataframe %>%
    mutate(across(where(is.character), tolower))
  
  return(dataframe)
}
