best <- function(state, outcome)
{
    ## define valid outcomes to check
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    ## read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## check that state and outcome are valid
    if (is.na(match(state, unique(data$State))))
    {
        ## not a valid state name, as it does not exist in the dataset
        stop("invalid state")
    }
    
    if (is.na(match(outcome, valid_outcomes)))
    {
        stop("invalid outcome")
    }
    
    ## return hospital name in the state with the lowest
    ## 30-day death rate
    
    ## get only the data for the state in question
    state_data <- data[data$State == state,]
    
    ## check which outcome in the valid outcomes list to use
    if (outcome == valid_outcomes[1])
    {
        getMinHospital(state_data, 11)
    }
    else if (outcome == valid_outcomes[2])
    {
        getMinHospital(state_data, 17)
    }
    else if (outcome == valid_outcomes[3])
    {
        getMinHospital(state_data, 23)
    }
    
    
}

## gets the name of the hospital that has the lowest value in a 
## specified measurement of the supplied data
##
## "data" is a data frame that has the hospital names and the
## measurements that will be used to evaluate the minimum
## "col" is the column number containing the measurement of which 
## the minimum will be evaluated
getMinHospital <- function(data, col)
{
    ## first get the data as numeric data
    data[, col] <- as.numeric(data[, col])
    ## then evaluate the minimum, dropping any NAs introduced by coercion
    min_val <- min(data[, col], na.rm = TRUE)
    
    ## get the rows where measurement is equal to the minimum
    min_outcome <- data[data[, col] == min_val, ]
    
    ## get the list of hospital names
    hospitals <- min_outcome[complete.cases(min_outcome),]$Hospital.Name
    
    ## sort the list alphabetically
    sort(hospitals)
    
    ## return only the first value in the list
    return(hospitals[1])
}