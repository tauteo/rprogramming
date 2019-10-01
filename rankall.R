rankall <- function(outcome, num = "best")
{
    ## define valid outcomes to check
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    ## read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    df <- data.frame(matrix(ncol = 2, nrow = 0))
    
    ## check that the outcome is valid
    if (is.na(match(outcome, valid_outcomes)))
    {
        ## not a valid outcome
        stop("invalid outcome")
    }
    
    ## return hospital name in the state with the lowest
    ## 30-day death rate
    states <- sort(unique(data$State))
    for (state in states){
        ## get only the data for the state in question
        state_data <- data[data$State == state,]
        
        ## get the ranked hospital name for the specified outcome
        if (outcome == valid_outcomes[1])
        {
            hospital <- data.frame(getRankedHospitals(state_data, 11, num), state)
            rownames(hospital) <- state
            ## get ranking according to heart attack rate (column 11)
            df <- rbind(df, hospital)
            
        }
        else if (outcome == valid_outcomes[2])
        {
            hospital <- data.frame(getRankedHospitals(state_data, 17, num), state)
            rownames(hospital) <- state
            ## get ranking according to heart failure rate (column 17)
            df <- rbind(df, hospital)
        }
        else if (outcome == valid_outcomes[3])
        {
            hospital <- data.frame(getRankedHospitals(state_data, 23, num), state)
            rownames(hospital) <- state
            ## get ranking according to pneumonia rate (column 23)
            df <- rbind(df, hospital)
        }
    }
    colnames(df) <- c("hospital", "state")
    df
}

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

