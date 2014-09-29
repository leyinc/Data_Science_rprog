 ## Finding the best hospital in a state
best<- function(state,outcome){
        
         ## Function for getting the hospital name
        check<- function(data, state, outcome)
        {
               stateSubset<- data[data$State==state, ]
               outcomeSubset<- stateSubset[ ,outcome]
                ## Find the  min death rate and ignore NAs
               minDeath<- min(outcomeSubset, na.rm=T)
               minIndex<- which(outcomeSubset==minDeath)
               
               hospital_name<- stateSubset[minIndex,2]
               
               return(hospital_name)
               
        }
        
        
        
         ## Read outcome data
        setwd("~/Desktop/ProgAssignment3-data/") 
        data<- read.csv("outcome-of-care-measures.csv", colClasses="character")
        
        data[ ,11]<- as.numeric(data[ ,11]) ## death rate from heart attack
        data[ ,17]<- as.numeric(data[, 17]) ## death rate from heart failure
        data[ ,23]<- as.numeric(data[ ,23]) ## death rate from pneumonia
        
         ## Check that state and outcome are valid
        outcomes<- c("heart attack", "heart failure", "pneumonia")
         ## Check whether state argument equals State values
        if(!state %in% unique(data$State))
        {
               stop("invalid state")        
        }
        
         ## Check whether outcome arugment equals outcomes values
        else if(!outcome %in% outcomes)
        {
               stop("invalid outcome")
        }
        
         ## Both valid input for state and outcome, then
         ## return hospital name in that state with lowest 30-day 
         ## death rate
        else
        {
              if(outcome=="heart attack")
              {
                      hosp_name<- check(data, state, 11)
              }
              else if(outcome=="heart failure")
              {
                      hosp_name<- check(data, state, 17)
                      
              }
              else
              {
                      hosp_name<- check(data, state, 23)
                      
              }
              
            return(hosp_name)
              
        }  
        
}