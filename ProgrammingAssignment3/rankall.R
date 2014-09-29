## Ranking hospitals in all states
rankall<- function(outcome, num){
        
        check<- function(stateSubset, colIndex, num)
        {
               
                outcomes<- as.numeric(stateSubset[ ,colIndex])
                len<- dim( stateSubset[!is.na(outcomes), ] ) [1]
                if(num=="best")
                {
                        rank<- rank(stateSubset, outcomes, 1)
                }
                else if(num=="worst")
                {
                        rank<- rank(stateSubset, outcomes, len)
                }
                else if(num>len)
                {
                        rank<- NA
                }
                else
                {
                        rank<- rank(stateSubset, outcomes, num)
                }
                              
                return(rank)
                
        }
        
        rank<- function(stateSubset, outcomes, num)
        {
                result<- stateSubset[ ,2][order(outcomes, stateSubset[ ,2]) [num]]
                return(result)
        }
        
              
        ## Read outcome data
        setwd("~/Desktop/ProgAssignment3-data/") 
        data<- read.csv("outcome-of-care-measures.csv", colClasses="character")
        
        outcomes<- c("heart attack", "heart failure", "pneumonia")
        states<- sort(unique(data$State)) ## sort function sorts state vector
        state_len<- length(states)
        hospital<- rep("", state_len) ## repeat "" for state_len times
        
        if(!outcome %in% outcomes)
        {
                stop("invalid outcome")
        }
        else
        {
                 ## Loop for each state
                for(i in 1:state_len)
                {
                    stateSubset<- data[data[ ,7]==states[i], ]    
                    
                    if(outcome=="heart attack")    
                    {
                            hospital[i]<- check(stateSubset, 11, num)
                    }
                    else if(outcome=="heart failure")
                    {
                            hospital[i]<- check(stateSubset, 17, num)          
                    }
                    else
                    {
                            hospital[i]<- check(stateSubset, 23, num)
                    }
                        
                }
                
        }
        
        ## Create the data frame to return
        df<- data.frame(hospital=hospital, state=states)
        return(df)
    
}