## Cache and retrieve the inverse of matrix 'x'


## makeCacheMatrix creates a list of functions to:
## set and get the value of a matrix and
## set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(x) inv <<- solve(x)
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}



## Given the list vector from makeCacheMatrix, 
## cacheSolve checks whether the inverse has been calculated and stored. 
## If so, it returns the inverse from the cache. 
## If not, calculates and returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
  
}
