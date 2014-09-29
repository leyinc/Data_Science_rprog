complete <- function(directory, id = 1:332) {
        
        ## dir="~/Desktop/specdata"
        dir<- paste("~/Desktop/",directory,sep="")
        setwd(dir)
        
        ## filename list="001.csv" "002.csv" "003.csv"
        filenames <- list.files(path=dir, pattern="*.csv")
        
        ## Initialize vectors
        ids <-vector()
        counts = vector()
        
        for(i in id) {
                ## filename="001.csv"
                filename <- sprintf("%03d.csv", i)
                data <- read.csv(filename)
                
                ## Store the id
                ids <- c(ids, i)
                
                ## Calculate and store the count of complete cases
                cases <- data[complete.cases(data),]
                counts <- c(counts, nrow(cases))
        }
        ## Return the data frame
        data.frame(id=ids, nobs=counts)
}