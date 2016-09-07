## These functions will calcuate and cache the inverse of a matrix.

## Function Name : makeCacheMatrix
##
## Description :  This function takes an invertable matrix and calculates and caches
##                the inverse.  It returns a list of functions for setting and 
##                accessing the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Input:
        ##       x - an invertable matrix
        ## Returns:
        ##       A list of functions for setting and getting 
        ##       the cached value of the matrix inverse
        
        inverseMatrix <- NULL
        
        ## set function - initalizes the caches variables
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        
        ## get function - returns the matrix
        get <- function() x
        
        ## setInverse function - caches the inverse
        setInverse <- function(inverse) inverseMatrix <<-inverse
        
        ## getInverse function - returns the caches value of inverseValue
        getInverse <- function() inverseMatrix
        
        ## Create a list of named functions
        list( set = set, get = get, setInverse = setInverse,
              getInverse = getInverse)
}


## Function Name : cacheSolve
##
## Description :  This function takes a list of funtions and uses them to access and set the
##                cached inverse of a given matrix.

cacheSolve <- function(x, ...) {
        
        ## Input:
        ##       x - a list of functions to access and set the inverse of a matrix
        ## Returns:
        ##       The inverse of a matrix.  If the inverse is already cached then the cached value
        ##       is returned.  If the cached inverse isn't found then it is calcuated, cached and 
        ##       returned to the caller.
        
        inverseMatrix <- x$getInverse()
        
        ##-- if the data is cached then return that value
        if (!is.null(inverseMatrix)) {
                message ("Retrieving the cached data")
                return(inverseMatrix)
        }
        
        ##-- if the data isn't cached then calcuate, cache and return the value to the caller
        data <- x$get()
        inverseMatrix <- solve(data)
        x$setInverse(inverseMatrix)
        inverseMatrix
}
