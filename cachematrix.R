## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  xInv <- NULL
  
  set <- function( y )
  {
    if( !is.matrix(y) )
      return(NULL)
    else
      x <<- y
    xInv <<- NULL
  }  
  
  get <- function() x
  
  setInverse <- function(inv) xInv <<- inv
  getInverse <- function() xInv
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}


##   The cacheSolve() function returns the inverse of 
##   of a given invertible matrix. If it has not already
##   been computed, it is computed and cached. If it has
##   already been computed and cached, the cached value 
##   is returned without repeating the computation.

cacheSolve <- function(x, ...) {
  
  i <- x$getInverse() 
  
  # If it's already stored, return cached data
  if( !is.null(i) )
  {
    return(i)
  }  
  
  mat <- x$get()
  i <- solve( mat , ... )
  x$setInverse( i )
  i
}
