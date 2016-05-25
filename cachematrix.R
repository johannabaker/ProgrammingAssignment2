## Cache and retrieve the inverse of matrix 'x'
# To use these functions,
  # 1. create mycache <- makeCacheMatrix(mymatrix);
        # returns a list variable which contains functions to 
        # set and get mymatrix and to
        # set and get the inverse of mymatrix
  # 2. cacheSolve(mycache) returns the inverse of mymatrix;
        # if the inverse has been saved in the cache, it returns the saved inverse.
        # if not, it finds the inverse and then saves it in the cache



## makeCacheMatrix creates a list of functions to:
    # set and get the value of a matrix and
    # set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL

    #create functions which set and get the matrix
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    
    #create functions which set and get the inverse of the matrix
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    
    #create a list variable for the set and get functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}




## Given the list vector from makeCacheMatrix, 
  # cacheSolve checks whether the inverse has been calculated and stored (cached). 
  # If so, it returns the inverse from the cache. 
  # If not, calculates and returns a matrix that is the inverse of mymatrix

cacheSolve <- function(mycache, ...) {
        ## Returns a matrix that is the inverse of the matrix in mycache

  #check whether the inverse has already been calculated and stored
  #if it has, returns the inverse from the cache
    inv <- mycache$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
  #if not, calculates the inverse and stores it in mycache
    data <- mycache$get()
    inv <- solve(data)
    mycache$setinverse(inv)
    inv
}
