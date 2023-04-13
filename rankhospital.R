rankhospital <- function(state, outcome, num = "best") {
  hospData<-read.csv("outcome-of-care-measures.csv", head=TRUE)
  presentStates<-unique(hospData$State) ##vectore with state abbr from data frame
  allowedOutcomes<-c("heart attack", "heart failure", "pneumonia") #valid outcomes
  ## Check that state and outcome are valid
  if(is.element(state, presentStates)==FALSE){
    stop("invalid state")
  }
  if(is.element(outcome, allowedOutcomes)==FALSE){
    stop("invalid outcome")
  }
  presentStates<-unique(hospData$State)
  
  ##seacrh data for observations for input state
  
  ##subset columns, hard code values now, automate later
  if(outcome=="heart attack")
    outcomeCol<- 11
  else{
    if(outcome== "heart failure"){
      outcomeCol<-  17
    }
    else{
      if (outcome == "pneumonia")
        outcomeCol<- 23
    }
  }
  
  hospNamesForOutcome<-hospData[c(2,7,outcomeCol)]
  
  ## Return hospital name in that state with the given rank
  
  
  
  
  ## 30-day death rate
}