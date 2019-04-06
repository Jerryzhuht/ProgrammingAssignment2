## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { # make cache of matrix and inverse matrix
    inverse.matrix <- NULL ## initial matrix set to be null
    
    set <- function(matrix){ ## set x be the matrix
        x <<- matrix
        inverse.matrix <<- NULL
    }
    
    get <- function() x ## get the matrix
    set.inverse <- function(inverse.matrix) inverse.matrix <<- inverse.matrix # set inverse matrix
    get.inverse <- function() inverse.matrix # get the inverse of the matrix
    
    # return the list of functions
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
    
    inverse.matrix <- x$get.inverse() # get the inverse matrix from cache
    
    if (!is.null(inverse.matrix)){ # if not null, return inverse matrix without calculation
        message("getting cached data")
        return(inverse.matrix)
    }
    
    matrix <- x$get() # get matrix from cache
    inverse.matrix <- solve(matrix) # solve the inverse of matrix 
    x$set(inverse.matrix) # save inverse matrix into cache
    inverse.matrix # return inverse matrix
    
}
