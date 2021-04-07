write_demographics <- function(data) {
  temp <- data %>%
    group_by(female) %>%
    summarise(count = n()) %>%
    mutate(pct = round(count / dim(data)[1], 3)*100)
  
  
  paste0("A total of ",  dim(data)[1],
         " participants (", temp$pct[which(temp$female == 1)], 
         "% female; age: M = ", round(mean(data$age), 2), 
         " years, SD = ", round(sd(data$age), 2), ")...")
}

# test