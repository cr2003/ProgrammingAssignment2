## These functions create a cache matrix and then returns its inverse
## Example usage:
## > mat <- matrix(rnorm(25), nrow = 5)                 # Create a square matrix 
## > cmat <- makeCacheMatrix(mat)                       # Create our cache matrix
## > cacheSolve(cmat)                                   # First call, calculates and returns de inverse matrix
## > cacheSolve(cmat)                                   # Second and following calls return the inverse matrix
                                                        # from the chache without need to calculate it


## makeCacheMatrix: creates a list of 4 funtions called from cacheSolve function
## 1. Set the value of the square matrix
## 2. Get the value of the square matrix
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        ## invMatrix stores the cached inverse matrix
        invMatrix <- NULL
        
        ## Setter for the square matrix
        setMatrix <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        
        ## Getter for the square matrix
        getMatrix <- function() x
        
        ## Setter for the inverse matrix
        setInverse <- function(inverse) invMatrix <<- inverse
        
        ## Getter for the inverse matrix
        getInverse <- function() invMatrix
        
        ## Return the list of the defined functions
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)       
}


## cacheSolve: In the first call calculates the inverse of the square matrix, following calls returns the cached
## inverse matrix

cacheSolve <- function(x, ...) {
        invMatrix <- x$getInverse()
        ## Return a matrix that is the inverse of 'x'
        
        ## If the inverse is calculated and cached, return it
        if (!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        
        ## The inverse matrix isn't calculated yet, so it is calculated now
        data <- x$getMatrix()
        invMatrix <- solve(data, ...)
        
        ## cache the inverse matrix for future calls
        x$setInverse(invMatrix)
        
        ## return the inverse matrix
        invMatrix
}
