rankhospital <- function(state, outcome, num = "best")
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
        ## not a valid outcome
        stop("invalid outcome")
    }
    
    ## return hospital name in the state with the lowest
    ## 30-day death rate
    
    ## get only the data for the state in question
    state_data <- data[data$State == state,]
    
    ## get the ranked hospital name for the specified outcome
    if (outcome == valid_outcomes[1])
    {
        ## get ranking according to heart attack rate (column 11)
        getRankedHospitals(state_data, 11, num)
    }
    else if (outcome == valid_outcomes[2])
    {
        ## get ranking according to heart failure rate (column 17)
        getRankedHospitals(state_data, 17, num)
    }
    else if (outcome == valid_outcomes[3])
    {
        ## get ranking according to pneumonia rate (column 23)
        getRankedHospitals(state_data, 23, num)
    }
}

## ranks hospitals in the supplied dataframe according to the
## specified column, as well as the hospital name (2nd level).
## the hospital name at the specified index is returned
##
## "data" is a data frame that has the hospital names and the
## measurements that will be used to evaluate the ranking
## "col" is the column number containing the measurement according
## to which the hospitals will be ranked
## "index" is the position in the ranking for which the name is
## returned. The default of 1 is equivalent to "best"
getRankedHospitals <- function(data, col, index)
{
    ## first get the data as numeric data
    data[, col] <- as.numeric(data[, col])
    
    if (index == "worst"){
        ## looking for the worst ranking, so order descending
        data <- data[order(-data[,col], data$Hospital.Name),]
        ## return the hospital in the first position
        data$Hospital.Name[1]
    }else if (index == "best"){
        ## looking for the best hospital, so order ascending
        data <- data[order(data[,col], data$Hospital.Name),]
        ## return the hospital in the first position
        data$Hospital.Name[1]
    }
    else{
        ## looking for the best hospital, so order ascending
        data <- data[order(data[,col], data$Hospital.Name),]
        ## return the hospital in the specified position
        data$Hospital.Name[index]
    }
}