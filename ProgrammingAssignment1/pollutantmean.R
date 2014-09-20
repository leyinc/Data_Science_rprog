pollutantmean<- function(directory, pollutant, id=1:332){
              
         ## dir="~/Desktop/specdata"
        dir<- paste("~/Desktop/",directory,sep="")
        
        setwd(dir)
        
         ## Return a list of names in the current directory
        namelists<- dir()
        dataframe<- lapply(namelists,read.csv)
        
         ## Specify the returned file based on id in dataframe
        dataset<- do.call("rbind",dataframe[id])
        
         ## Calculate the mean of the specified col in final dataset
        sprintf("%.3f", mean(dataset[[pollutant]],na.rm=TRUE))
        
}

