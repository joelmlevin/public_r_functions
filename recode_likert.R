agree_disagree_responses1 <- c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")
agree_disagree_responses2 <- c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree")
confidence_responses <- c("Extremely confident", "Very confident", "Somewhat confident", "Slightly confident", "Not confident")
yesno_responses <- c("Definitely yes", "Probably yes", "Not sure", "Probably not", "Definitely not")
wellness_responses <- c("Extremely well", "Very well", "Somewhat well", "Slightly well", "Not well at all")


recode_likert <- function(qualresponse) {
  if("TRUE" %in% (qualresponse %in% agree_disagree_responses1 | qualresponse %in% agree_disagree_responses2)) { #if the response pattern matches 'agree-disagree', recode
    temp <- qualresponse %>%
      recode("Strongly agree" = 5, #recode automatically converts strings to numeric
             "Somewhat agree" = 4,
             "Agree" = 4,
             "Neither agree nor disagree" = 3,
             "Somewhat disagree" = 2,
             "Disagree" = 2,
             "Strongly disagree" = 1
      )
    #print("Recoding agree...")
  } else { 
  }
  if("TRUE" %in% (qualresponse %in% confidence_responses)) { 
    temp <- qualresponse %>%
      recode("Extremely confident" = 5, 
             "Very confident" = 4,
             "Somewhat confident" = 3,
             "Slightly confident" = 2,
             "Not confident" = 1
      )
  } else { 
  }
  if("TRUE" %in% (qualresponse %in% yesno_responses)) { 
    temp <- qualresponse %>%
      recode("Definitely yes" = 5, 
             "Probably yes" = 4,
             "Not sure" = 3,
             "Probably not" = 2,
             "Definitely not" = 1
      )
    #print("Recoding yes/no...")
  } else { 
  }
  if("TRUE" %in% (qualresponse %in% wellness_responses)) { 
    temp <- qualresponse %>%
      recode("Extremely well" = 5, 
             "Very well" = 4,
             "Somewhat well" = 3,
             "Slightly well" = 2,
             "Not well at all" = 1
      )
    #print("Recoding yes/no...")
  } else { 
  }
  return(temp)
}

reverser.5 <- function(x) {
  temp <- as.integer(-1 * x + 6)
  #print("Reversing item")
  return(temp)
}
