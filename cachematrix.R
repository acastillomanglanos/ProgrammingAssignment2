## makeCacheMatrix creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  # stores the inverted matrix cached value
  # initialize the inverse matrix to NULL
  inver <- NULL
  
  # create the matrix in the working environment
  set <- function(y) 
  {
    x <<- y
    inver <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  # invert the matrix and store in cache
  setMatrix <- function(inverse) inver <<- inverse
  # get the inverted matrix from cache
  getInverse <- function() inver
  
  # return the created functions to the working environment
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}


## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it creates it in the working environment and inverts the matrix
## and after is stored in the cache

cacheSolve <- function(x, ...) {
  ## Looking for the inverse of the matrix stored in cache
  inver <- x$getInverse()
  
  # if there is an inverted matrix in the cache, returns it
  # else creates the  matrix in the environment
  if (!is.null(inver)) {
    message("Getting inverse from cache")
    
    # display matrix in console
    return(inver)
  }
  
  # create the new matrix since it does not exist
  matrix <- x$get()
  # set and return inverse of the matrix
  inver <- solve(matrix, ...)
  # save the inverted matrix in cache
  x$setMatrix(inver)
  
  # display the inverted matrix
  return (inver)
}
