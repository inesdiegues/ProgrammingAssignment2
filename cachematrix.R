
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It will be a list containing the functions to set and get the original matrix as weel as set and get the inverse of it

makeCacheMatrix <- function(x = matrix()) {

  M <- NULL
  
  # Setting the value of the matrix 
  
  set <- function(y){
    x <<-y
    M <<- NULL
  }
  
  # Getting the values in the matrix 
  
  get <- function() x
  
  # Setting the inverse of the matrix
  
  setinverse <- function(inverse) M <<- inverse
  
  # Getting the inverse solution of the matrix
  
  getinverse <- function() M
  
  # Create the list containing all the functions associated to this special "matrix" object
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # First of all let's see if there is something already stored in our special "matrix" object related with the inverse calculation
  
  M <- x$getinverse()
  
  # If there is already an inverse matrix in the cache then we get that value
  
  if(!is.null(M)){
    message("Getting the inverse solution from cache data")
    return(M)
  }
  
  # Otherwise we are going to calculate the inverse from the values of the given matrix
   
    data <- x$get()
    M <- solve(data,...)
    x$setinverse(M) # Now we need to store this computation for the future
    M
}
