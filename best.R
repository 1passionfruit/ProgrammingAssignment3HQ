best <- function(state, outcome) {
  ## Read outcome data
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
   
     hName<-c()
     hMR<-c()
     hospWithOutcome<-data.frame(hsname=hName,mrate=hMR) ##empty data frame

         ##process subset by extracting hospitals in the input state that has outcome data
   
     for(index in 1:nrow(hospData)){
       if((hospNamesForOutcome[index,2]==state) && 
          (hospNamesForOutcome[index,3]!='Not Available')){
           
         hospWithOutcome<- rbind(hospWithOutcome, data.frame(hsname=hospNamesForOutcome[index,1]
                                  ,mrate=as.numeric(hospNamesForOutcome[index,3])))
         
       }
      }
     
  ## Return hospital name in that state with lowest 30-day death
    ##sort first by mortality rate, then by hospital name
    hospWithOutcome<-hospWithOutcome[order(hospWithOutcome$mrate, 
                                          hospWithOutcome$hsname),]
    hospWithOutcome[1,]
  ## rate
}