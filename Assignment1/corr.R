corr <- function(directory, threshold = 0) {
        
         ## dir="~/Desktop/specdata"
        dir<- paste("~/Desktop/",directory,sep="")
        setwd(dir)
        
         ## Return all the complete cases in the dataset
        completeCases <- complete(directory, 1:332)
         ## Return complete cases which satisfied the threshold
         ## threshold is for the number of complete cases
        completeCases <- subset(completeCases, nobs > threshold )
        
         ## Initialize correlation vector
        correlations <- vector()
        
         ## Loop over the passed id's and compute the correlation
        for(i in completeCases$id ) 
        {            
                 ## filename="001.csv"
                filename <- sprintf("%03d.csv", i)              
                data <- read.csv(filename)
                
                 ## Calculate and store the count of complete cases
                cases <- data[complete.cases(data),]
                counts <- nrow(cases)
                
                 ## Calculate and store the count of complete cases
                 ## if threshhold is reached
                if( counts >= threshold )
                {
                  correlations <- c(correlations, cor(cases$nitrate, cases$sulfate, 
                                                   use="pairwise.complete.obs") )
                }
        }
         ## Return a numeric vector of correlations
        correlations
}