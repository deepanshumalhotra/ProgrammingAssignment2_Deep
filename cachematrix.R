## makeCacheMatrix <- Takes a squared invertible matrix as an input and Returns a list
## of functions which can be used for changing the matrix to be inversed (set) 
## retreiving the matrix (get) setting the inverse of the matrix to be cached in future (setInverse) 
## and retreiving the stored matrix inverse(getInverse)

## CacheSolve <- Takes the output list from makeCacheMatrix as input and returns
## the inverse matrix. This function cache the inverse matrix if its already computed using getInverse
## else compute the Inverse of the matrix and store the inverse in the output 
## list of makeCacheMatrix function.


makeCacheMatrix <- function(x = matrix()) {
  inverse  <- NULL
  
  # To set the input matrix x
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  #To retrive the input matrix x
  get <- function() x
  
  #To set the computed inverse of the input matrix  x
  setinverse <- function(inv) inverse <<- inv
  #To retreive  the computed stored inverse of the input matrix x
  getinverse <- function() inverse
  
  #Output Special List
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  
  ## Check if the matrix inverse already cached. If yes then Return the inverse matrix
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## Computing and then setting the inverse of the matrix.
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
