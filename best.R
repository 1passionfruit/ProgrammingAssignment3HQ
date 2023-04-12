best <- function(state, outcome) {
  ## Read outcome data
    hospData<-read.csv("outcome-of-care-measures.csv", head=TRUE)
    presentStates<-unique(hospData$State) ##vectore with state abbr from data frame
    allowedOutcomes<-c("heart attack", "heart failure", "Pneumonia") #valid outcomes
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
         outcomeCol<- 23
         }
     }
     
     hospNamesForOutcome<-hospData[c(hospData$Hospital.Name,hospData$State,outcomeCol)]
    hospWithOutcome<-c() ##empty vector

         ##process subset by extracting hospitals in the input state that has outcome data
     for(index in nrow(hospData)){
       if(hospNamesForOutcome[index,2]==state && 
          hospNamesForOutcome[index,3]!="Not Available"){
         rbind(hospWithOutcome,c(hospNamesForOutcome[index,1],hospNamesForOutcome[index,3]))
       }
       else{
         next()
     }
     }
     
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}