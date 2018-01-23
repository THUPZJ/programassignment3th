source("rankhospital.R")
rankall <- function(disease, num = "best")
{
    ## Read outcome data
    
    ## Check that state and outcome are valid
    
    ## For each state, find the hospital of the given rank
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
    ## Check that state and outcome are valid
    # disease_list <- c("heart attack", "heart failure", "pneumonia")
    
    state_list <- outcome$State
    outcome_list <- split(outcome, state_list)
    state_names <- names(outcome_list)
    ans <- data.frame(state_names, hospital = rep(NA, length(state_names)), state = state_names)
    for(i in 1:length(state_names))
    {
        ans[i, 2] <- rankhospital(state_names[i], disease, num)
    }
    ans
}