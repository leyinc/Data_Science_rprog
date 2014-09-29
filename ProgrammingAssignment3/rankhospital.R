 ## Ranking hospitals by outcome in a state
rankhospital<- function(state, outcome, num="best"){
        
         ## Function for returning hosp name 
        check<- function(data, state, outcome, num)
        {
                stateSubset<- data[data$State==state, ]
                outcomeSubset<- stateSubset[ ,outcome]
                 ## Return the num of rows for satisfiable state & outcome, ignore NAs
                len <- dim(stateSubset[!is.na(outcomeSubset), ])[1]
                
                if(num=="worst")
                {
                        name <- rank(stateSubset, outcomeSubset, len)
                } 
                 ## Input rank longer than critical value
                else if (num > len) 
                {
                        name <- NA
                } 
                else 
                {
                        name <- rank(stateSubset, outcomeSubset, num)
                }
                
                
             return(name)
        }
        
        
        rank<- function(stateSubset, outcomeSubset, num)
        {
                 ## Order returns the sequence of locations of the lowest value
                hospital_name<- stateSubset[ ,2] [ order(outcomeSubset, stateSubset[ ,2]) [num] ]
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
        
         ## Both valid input for state and outcome
        else
        {
                if(outcome=="heart attack")
                {
                        hosp_name<- check(data, state, 11, num)
                }
                else if(outcome=="heart failure")
                {
                        hosp_name<- check(data, state, 17, num)
                }
                else
                {
                        hosp_name<- check(data, state, 23, num)
                }
                
         }
        
        return(hosp_name)
              
}