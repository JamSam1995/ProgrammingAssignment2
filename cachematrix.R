## These two functions will together, take a matrix input, compute the
## inverse if it has not already been calculated, or return it from cached
## data, with a message, if it has. This saves repeated computation.

## The makeCacheMatrix function takes the matrix input 'x' and transforms
## it into a list of functions. The first element in the list, set, will set 
## the data in 'x' to its input 'y'. The second element, get, will return the 
## the data 'x'. The third, setinverse, will set the object made to store the
## inverse of 'x', 'i', to its input 'inverse' and the fourth will return 'i'. 

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function takes the output of makeCacheMatrix as input,
## checks if the inverse has already been calculated, using the getinverse
## function in 'x', and outputs it if it has, with the accompanying message 
## "getting cached data". If it has not, the function gets the data from 'x',
## using the get function, computes its inverse, stores the result in 'i' 
## with the setinverse function and then outputs 'i'.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
