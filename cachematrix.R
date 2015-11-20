## Functions to create a matrix and calculate the inverse of this matrix, 
## returning the calculated matrix or a cache matrix if exist already.

## Function to store a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    #Initializing variables
    matrix_inv <- NULL
    
    #Store the initial matrix
    set <- function(y){
        x <<- y
        matrix_inv <<- NULL        
    }    
    
    #Return the de original matrix
    get <- function() x
    
    #Store the value of the inverse matrix
    setInverse <- function(mat) matrix_inv<<-mat
    
    #Return the result of the inverse matrix
    getInverse <- function() matrix_inv
    
    #Return the getters and setters
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Function to solve inverse matrix if its needed otherwise return the cache matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #Getting the value of the inverse matrix
        matrix_inv <- x$getInverse()
        
        #Check if exist the inverse matrix in cache
        if(!is.null(matrix_inv)){
            message("Getting from cache")
            #Return the cache value
            return(matrix_inv)
        }
        
        #Calculate the inverse matrix
        matrix_inv <- solve(x$get())
        
        #Setting the value of the inverse matrix
        x$setInverse(matrix_inv)
        
        #Return the value of the inverse matrix for display
        return(matrix_inv)
}