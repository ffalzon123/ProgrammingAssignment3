best <- function(state, outcome) {
    ## Read outcome data
    outcomeData <-
        read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    
    vectorOfStates <- unique(outcomeData[, 7])
    vectorOfOutcomes <- c("heart attack","heart failure","pneumonia")
    validState <- state %in% vectorOfStates
    validOutcome <- outcome %in% vectorOfOutcomes

    if (validState) {
        if (validOutcome) {
            print("do stuff")
        } else {
            print("invalid outcome")
        }
        
    } else {
        print("invalid state")
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
}