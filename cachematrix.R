## A pair of functions allowing a matrix inverse to be cached, 
## previnting repeated calculations

## Create n special "matrix" object that can cache the inverse 

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(y)
    {
        inv <<- NULL
        x <<- y
    }
    get <- function() x
    setinverse <- function(i) inv <<- i
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, 
         getinverse=getinverse)
}


## Returns a matrix that is the inverse of x

cacheSolve <- function(x, ...) 
{
    inv <- x$getinverse()
    if(!is.null(inv))
    {
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}