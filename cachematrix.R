## The idea is to create a function that caches the inverse matrix
## in order to avoid calculating it each time. Input parameter: matrix to be inverted
makeCacheMatrix <- function(x = matrix()) {
    
    ## This is the object to keep (cache) the inverse matrix. We initialize it to NULL
    s <- NULL
    
    ## Function to set a new "special" matrix. We initialize the inverse matrix object and keep the 
    ## new matrix to be inverted
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    ## This function returns the value of the matrix to be inverted   
    get <- function() x
    
    ## This function receives as parameter the inverse matrix and caches it on the object s
    setsolve <- function(solveM) s <<- solveM
    
    ##This function returns the inverse matrix 
    getsolve <- function() s
    
    ## Object returned. A list with all functions defined previously
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## this function calculates inverse matrix if it's not calculated yet. if it's calculated, the function
## makeCaheMatrix is caching this value and this object is returned instead of calculating it again
## Input parameter: especial matrix created by the function makeCacheMatrix
cacheSolve <- function(x) {
    
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    
    ## If it's not calucated yet, the inverse matrix is NULL, if not we return the inverse matrix cached
    if(!is.null(s)) {
        message("Getting cached Inverse Matrix - Not computing again 'solve' operation")
        return(s)
    }
    
    ## We reach this code if s = NULL. So firstly, we get the matirx to be inverted
    data <- x$get()
    
    ## seconly, we perform the operation to invert this matrix.Function solve.
    s <- solve(data)
    
    ## Finally, we cache the inversed matrix using the function setsolve
    x$setsolve(s)
    
    ## this function returns the inversed matrix. Only the first time we are calculating it
    s
}
