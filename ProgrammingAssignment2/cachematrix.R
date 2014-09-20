## Create a matrix that cashes its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}



## The following function calculates the inverse matrix
## created with the above function.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached inverse matrix:")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        message("first run for the inverse matrix:")
        inv
}


## Test Case:
## --------------------------------------
## > source("cachematrix.R")
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## > cacheSolve(m)
## first run for the inverse matrix:
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## > cacheSolve(m)
## getting cached inverse matrix:
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
