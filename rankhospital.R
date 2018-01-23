rankhospital <- function(state, disease, num = "best")
{
    ## Read outcome data
    
    ## Check that state and outcome are valid
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
    ## Check that state and outcome are valid
    disease_list <- c("heart attack", "heart failure", "pneumonia")
    if(state  %in% outcome[,7] == FALSE)
    {
        stop("invalid state")
    }
    if(disease %in% disease_list == FALSE)
    {
        stop("invalid outcome")
    }
    ## Return hospital name in that state with lowest 30-day death
    deathrate_col <- 0
    if(disease == "heart attack")
    {
        deathrate_col <- 11
    }
    if(disease =="heart failure")
    {
        deathrate_col <- 17
    }
    if(disease == "pneumonia")
    {
        deathrate_col <- 23
    }
    # part_outcome <- outcome[,c(2,7,deathrate_col)]
    state_list <- outcome$State
    outcome_list <- split(outcome, state_list)
    state <- as.character(state)
    spec_outcome <- outcome_list[state]
    dataframe_spec <- as.data.frame(spec_outcome)
    dataframe_spec[, deathrate_col] <- as.numeric(dataframe_spec[,deathrate_col])
    dataframe_spec <- dataframe_spec[order(dataframe_spec[,deathrate_col],dataframe_spec[,2],decreasing=FALSE),]
    part <- dataframe_spec[,c(2,deathrate_col)]
    names(part) <- c("Hospital.Name", "Rate")
    ans <- character()
    if(num == "best")
    {
        ans <- part[1,1]
    }
    else if(num == "worst")
    {
        count <- nrow(part)
        for(i in nrow(part):1)
        {
            if(!is.na(part[i,2]))
            {
                count <- i
                break
            }
        }
        ans <- part[count, 1]
    }
    else if(num > nrow(part))
    {
        ans <- NA
    }
    else
    {
        ans <- part[num, 1]
    }
    # index <- which.min(dataframe_spec[,deathrate_col])
    # dataframe_spec[index, 2]
    ans
    
}