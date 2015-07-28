rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomeData <-
    read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  
  vectorOfStates <- unique(outcomeData[, 7])
  vectorOfOutcomes <- c("heart attack","heart failure","pneumonia")
  validOutcome <- outcome %in% vectorOfOutcomes
  
  
  if (validOutcome) {
    ## convert rates to numeric
    suppressWarnings(outcomeData[, 11] <-
                       as.numeric(outcomeData[, 11]))
    suppressWarnings(outcomeData[, 17] <-
                       as.numeric(outcomeData[, 17]))
    suppressWarnings(outcomeData[, 23] <-
                       as.numeric(outcomeData[, 23]))
    
    if (outcome == "heart attack") {
      rates <- outcomeData[, 11] ## vector of rate
      hospitalRank <- outcomeData[c(2,7,11)]
    }
    if (outcome == "heart failure") {
      rates <- outcomeData[, 17] ## vector of rate
      hospitalRank <- outcomeData[c(2,7,17)]
    }
    if (outcome == "pneumonia") {
      rates <- outcomeData[, 23] ## vector of rate
      hospitalRank <- outcomeData[c(2,7,23)]
    }
    
    ## remove NAs from dataframe
    hospitalRank <- hospitalRank[complete.cases(hospitalRank),]
    
    
    ## order by state and hospital rank
    names(hospitalRank) <- c("Hospital.Name","State","Rate")

    ## For each state, find the hospital of the given rank
    hospitalRank <- hospitalRank[with(hospitalRank, order(State,Rate,Hospital.Name)),]
    hospitalRank$Rank <- 0 ## added rank column
    ## loop through records to assign ranks
    stateRank <- 0
    lastState <- hospitalRank[1,2]
    for (i in 1:length(hospitalRank$Hospital.Name)){
      currentState <- hospitalRank[i,2]
      if (currentState == lastState){
        stateRank <- stateRank + 1
        ## assign rank number to dataframe
        hospitalRank[i,4] <- stateRank
      } else {
        ## reset rank counter on change of state
        stateRank <- 1
        ## assign rank number to dataframe
        hospitalRank[i,4] <- stateRank
        lastState <- currentState
      }
    }
    
    ## filter by rank
    if(num != "worst"){
      hospitalRank <- hospitalRank[hospitalRank$Rank == num,]
    } else {
      worstRank <- aggregate(Rank ~ State, hospitalRank, max)
      hospitalRank <- merge(x = worstRank, y = hospitalRank, by = c("State","Rank"), all.x = TRUE)
      hospitalRank <- hospitalRank[,c(3,1)]
    }
    
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    dfOfStates <- data.frame(vectorOfStates)

    names(dfOfStates) <- c("State")
    results <- merge(x = dfOfStates, y = hospitalRank, by = "State", all.x = TRUE)
    results <- results[,c(2,1)]
    names(results) <- c("hospital","state")
    return(results) 

    
  } else {
    stop("invalid outcome")
  }
  
  
  
  
  
}