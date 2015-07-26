# Programming Assignment #2
# inv takes the inverse of the matrix
# 
makeCacheMatrix <- function(x = matrix()) {
        # inv will store the cached inverse matrix
        inv <- NULL
        
        # Set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Get the matrix
        get <- function() x
        
        # Set the inverse
        setinv <- function(inverse) inv <<- inverse
        # Get the inverse
        getinv <- function() inv
        
        # set the matrix
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# Function cacheSolve - check to see if the inverse exists and if not,  
# calculate and set the inverse
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        
        # check to see if inv has a value, if so, return it
        if (!is.null(inv)) {
                #message("value was already calc'd - returning that value")
                return(inv)
        }
        
        # value not yet calculated - calculate it
        data <- x$get()
        inv <- solve(data, ...)
        
        # move the newly calc'd inverse value to inv
        x$setinv(inv)
        
        # Return it
        inv
}