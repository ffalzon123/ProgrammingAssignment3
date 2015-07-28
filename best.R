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
            
            ## filter by state
            StateData <- outcomeData[outcomeData$State == state,]
            ## convert rates to numeric
            suppressWarnings(StateData[, 11]<- as.numeric(StateData[, 11]))
            suppressWarnings(StateData[, 17]<- as.numeric(StateData[, 17]))
            suppressWarnings(StateData[, 23]<- as.numeric(StateData[, 23]))
            
            if(outcome == "heart attack") {
                rates <- StateData[, 11] ## vector of rate
                minRate <- min(rates, na.rm = TRUE) ## min rate for outcome
                bestHospital <- StateData[c(2,11)]
            }
            if(outcome == "heart failure") {
                rates <- StateData[, 17] ## vector of rate
                minRate <- min(rates, na.rm = TRUE) ## min rate for outcome
                bestHospital <- StateData[c(2,17)]
            }
            if(outcome == "pneumonia") {
                rates <- StateData[, 23] ## vector of rate
                minRate <- min(rates, na.rm = TRUE) ## min rate for outcome
                bestHospital <- StateData[c(2,23)]
            }

            bestHospital <- bestHospital[complete.cases(bestHospital),]
            names(bestHospital) <- c("Hospital.Name","Rate")
            
            ## select Hospital(s) with the min rate
            bestHospital <- bestHospital[bestHospital$Rate == minRate,]
            
            ## order by hospital name
            bestHospital <- bestHospital[with(bestHospital, order(Hospital.Name)),]
            
            ## Return hospital name in that state with lowest 30-day death
            ## rate
            return(bestHospital[1,1])
            
        } else {
            stop("invalid outcome")
        }
        
    } else {
        stop("invalid state")
    }
    

    
}