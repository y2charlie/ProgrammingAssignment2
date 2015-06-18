## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: return a list of functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## cached inverse will be stored in variable inverse
        inverse <- NULL
        
        ## set function to initialize the matrix content
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        ## get function to obtain the matrix content
        get <- function() x
        
        ## setting the inverse
        setinverse <- function(solve) inverse <<- solve
        
        ## getting the inverse
        getinverse <- function() inverse
        
        ## return the list of function created above
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: Calculate the inverse of the matrix. If the inverse is already
## calculated before, it returns the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        
        ## If inverse has been calculated, return it.
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        ## If not, get the matrix
        data <- x$get()
        
        ## Calculate the inverse
        inverse <- solve(data, ...)
        
        ## Cache the inverse
        x$setinverse(inverse)
        
        ## Return the inverse
        inverse
}
