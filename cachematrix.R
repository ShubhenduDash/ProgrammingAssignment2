## The two functions that cache the inverse of a given matrix.

## The below makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y){
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inver <<- solveMatrix
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The below cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getInverse()
        if(!is.null(inver)){
                message("Getting Cached Data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setInverse(inver)
        inver      
}