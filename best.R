best <- function(state, outcome) {
  ## Read outcome data
    hospData<-read.csv("outcome-of-care-measures.csv", head=TRUE)
    presentStates<-unique(hospData$State) ##vectore with state abbr from data frame
    allowedOutcomes<-c("heart attack", "heart_failure", "Pneumonia") #valid outcomes
    ## Check that state and outcome are valid
    if(is.element(state, presentStates)==FALSE){
      stop("invalid state")
    }
    if(is.element(outcome, allowedOutcomes)==FALSE){
      stop("invalid outcome")
    }
     presentStates<-unique(hospData$State)
    
 
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}