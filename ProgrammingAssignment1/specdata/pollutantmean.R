pollutantmean<- function(directory, pollutant, id=1:332){
              
        ## Users Desktop
        dir<- paste("~/Desktop/",directory,sep="")
        
        ## /Users/emilychen/specdata
        setwd(dir)
        
        ## return a list of names in the current directory
        namelists<- dir()
        dataframe<- lapply(namelists,read.csv)
        
        ## specify the returned file based on id in dataframe
        dataset<- do.call("rbind",dataframe[id])
        
        ## calculate the mean of the specified col in final dataset
        colname<- pollutant
        sprintf("%.3f", mean(dataset[[colname]],na.rm=TRUE))
}

