## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#### 1. makeCacheMatrix ####
## makeCacheMatrix is a function that creates a special "matrix" object that can cache
## its inverse.
## makeCacheMatrix returns a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        # inv will store the cached inverse matrix
        inv <- NULL ## Initializes to NULL to know if cacheSolve has been run previously
        
        # Setter for the matrix
        set <- function(y) { ## Creates "set" function to store the matrix passed as "x"
                x <<- y ## Puts the passed matrix into "x" in the cache
                inv <<- NULL ## Initializes "inv" in the cache to NULL to know if cacheSolve 
                # has been run previously
        }
        
        # Getter for the matrix
        get <- function() x ## Creates "get" function to get the matrix passed
        
        # Setter for the inverse
        setinv <- function(inverse) inv <<- inverse ## Creates function "setinv" to set the 
        # value of "inv" in the cache 
        
        # Getter for the inverse
        getinv <- function() inv ## Creates "getinv" function to retrieve the value of "inv" 
        #from the cache check it for NULL
        
        # Returns the matrix with our newly defined functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

#### 2. cacheSolve ####
## cacheSolve is a function that computes the inverse of the matrix returned 
## by makeCacheMatrix (above). If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve retrieves the inverse from 
## the cache.
## Computing the inverse of a square matrix can be done with the solve function in 
## R. If X is a square invertible matrix, then solve(X) returns its inverse.
## We will assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) { ## Receives makeCacheMatrix 
        inv <- x$getinv() ## Gets the value for "inv" from the cache
        
        # If the inverse is already calculated, return it
        if (!is.null(inv)) { ## Checks if "inv" is not NULL
                message("getting cached data") ## If "inv" is not NULL then prints "getting 
                #cached data"
                return(inv) ## If "inv" is not NULL then returns the value of "inv"
        }
        
        # Else means that the inverse is not yet calculated, so we must calculate it
        data <- x$get() ## Obtains the matrix to be inverted and assigns it to "data"
        inv <- solve(data, ...) ## Uses R's "solve" function to compute the inverse of the 
        # invertible square matrix supplied, and asssigns the result 
        # to "inv"
        
        # Cache the inverse
        x$setinv(inv) ## Calls function "x$setinv()" in the makeCacheMatrix function to set 
        #"inv" to the obtained result
        
        # Return it
        inv ## Prints invers matrix 
}

# To test:
x <- # Put here a squared invertible matrix that will be assigned to "x"
        x # Prints matrix "x"
mcm <- makeCacheMatrix(x) # Creates the matrix to be inverted and puts it into "mcm"
mcm$get() # Returns the matrix
cacheSolve(mcm) # Returns the inverse of the matrix "mcm"
cacheSolve(mcm) # Calls again the inverse, so it returns the prior cached inverse matrix
