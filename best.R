best <- function(state, disease)
{
    ## Read out come data
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
    # spec_outcome[,11] <- as.numeric(spec_outcome[,11])
    index <- which.min(dataframe_spec[,deathrate_col])
    dataframe_spec[index, 2]
    ## rate
}

