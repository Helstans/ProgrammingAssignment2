## Below are two functions that are used to create a special object 
## that stores a matrix and cache's its inverse.

## This function creates a special matrix, which is really a list 
## containing a function to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
In <- NULL
        set <- function(y) {
        	## use <<- to assign value to an object in an environment ## different from the current environment.
                x <<- y
                In <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) In <<- inverse
        getinverse <- function() In
        list(set = set, get = get, setinverse = setinverse, 
        getinverse = getinverse)
}


## This function computes the inverse of the special matrix returned
## by makeCacheMatrix above. If the inverse has already been        
## calculated and the matrix has not changed, then the cachesolve   
## should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## If the inverse has already been calculated
        if(!is.null(In)) {
        	## get the inverse from the cache instead of computing
                message("getting cached data")
                return(In)
        }
        ## If not already computed, calculate the inverse
        data <- x$get()
        In <- solve(data, ...)
        ## set the value of the inverse in the cache
        x$setinverse(In)
        In
}
