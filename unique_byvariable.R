# a function to take a df and a character vector of variables and return a list of unique values and counts for each variable

unique_byvariable <- function(data, # a dataframe
                              variables) { # a character vector of variable names
    map(variables,
        function(x) {data %>% group_by(value = .[[x]]) %>% count()})
}
