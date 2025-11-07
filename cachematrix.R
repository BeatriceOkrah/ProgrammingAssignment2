## Programming Assignment 2: Lexical Scoping
## -----------------------------------------

## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL               # Initialize the inverse as NULL
  
  # Function to set a new matrix
  set <- function(y) {
    x <<- y               # Assign new matrix to x in parent environment
    inv <<- NULL          # Reset inverse cache
  }
  
  # Function to get the current matrix
  get <- function() x
  
  # Function to set (cache) the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse
  getInverse <- function() inv
  
  # Return a list of all four functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()     # Retrieve cached inverse (if available)
  
  # If inverse is already cached, return it directly
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  mat <- x$get()            # Retrieve the matrix
  inv <- solve(mat, ...)    # Compute the matrix inverse
  x$setInverse(inv)         # Cache the result
  inv                       # Return the inverse
}
