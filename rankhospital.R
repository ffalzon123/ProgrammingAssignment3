rankhospital <- function(state, outcome, num = "best") {
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
        ##minRate <- min(rates, na.rm = TRUE) ## min rate for outcome
        hospitalRank <- StateData[c(2,11)]
      }
      if(outcome == "heart failure") {
        rates <- StateData[, 17] ## vector of rate
        ##minRate <- min(rates, na.rm = TRUE) ## min rate for outcome
        hospitalRank <- StateData[c(2,17)]
      }
      if(outcome == "pneumonia") {
        rates <- StateData[, 23] ## vector of rate
        ##minRate <- min(rates, na.rm = TRUE) ## min rate for outcome
        hospitalRank <- StateData[c(2,23)]
      }

      ## remove NAs from dataframe
      hospitalRank <- hospitalRank[complete.cases(hospitalRank),]
      
      ## order by hospital rank
      names(hospitalRank) <- c("Hospital.Name","Rate")
      hospitalRank <- hospitalRank[with(hospitalRank, order(Rate,Hospital.Name)),]
      
      ## assign rank number to dataframe
      hospitalRank <- cbind(hospitalRank, 1:length(hospitalRank$Hospital.Name))
      names(hospitalRank) <- c("Hospital.Name","Rate","Rank")
      
      ## select Hospital(s) for given rank
      if(num == "best"){
        num <- 1
      }
      if(num == "worst"){
        num <- length(hospitalRank$Hospital.Name)
      }
      hospitalRank <- hospitalRank[hospitalRank$Rank == num,]
      
      ## Return hospital name in that state with the given rank
      ## 30-day death rate
      return(hospitalRank[1,1])
      
    } else {
      stop("invalid outcome")
    }
    
  } else {
    stop("invalid state")
  }
  

  
}