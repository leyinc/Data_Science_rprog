 ## Create a matrix that cashes its inverse
makeCasheMatrix<- function( x=matrix() ){
        
         ## Initialize the inverse matrix
        i<- NULL
        
         ## Set matirx 
        set<- function(matrix) {
                m<<- matrix
                i<<- NULL
        }
        
         ## Get matrix 
        get<- function() {
                m
        }
        
         ## Set the inverse matrix
        setInverse<- function(inverse) { 
                i<<- inverse 
        }
        
         ## Get the inverse matrix
        getInverse<- function() {
                i
        }
        
        ## Return the listed functions
        list(set=set, get=get, 
             setInverse=setInverse, getInverse=getInverse)    
}



## The following function calculates the inverse matrix 
## created with the above function.

cacheSolve<- function(x, ...){
        
         ## Return an inverse matrix of x
        i<- x$getInverse()
        
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        
        data<- x$get()
        
         ## Invoke matrix multiplication
        i<- solve(data) %*% data
        
        x$setInverse(i)
        
         ## Return this inverse matrix
        i
}
