# note that this uses tidyverse functions and should be used inside scripts that already load tidyverse.
# a thorough implementation will check for this

# also note that this is set up to convert section names for Joel Levin MGT162 SP24... 

canvas_toqualtrics <- function(canvas_export_path) {
    temp <- read_csv(canvas_export_path) 
    
    temp2 <- temp %>%
        select(name = Student,
               sis = `SIS Login ID`,
               section = Section) %>%
        filter(name != "Points Possible",
               name != "Student, Test",
               !is.na(name),
        ) %>%
        separate(name, into = c("LastName", "FirstName"), sep = ", ") %>%
        mutate(Email = paste0(sis, "@ucsd.edu"),
               section = case_when(str_detect(section, "C00") ~ "morning",
                                   str_detect(section, "D00") ~ "afternoon",)) %>%
        select(LastName, FirstName, Email, section) 
    
    return(temp2)
    
}

# testing 

# canvas_toqualtrics("Documents/Teaching/Negotiation/Levin 2024/00 Administrative/rosters/roster_canvas_050424.csv")
