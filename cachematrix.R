## These are 2 functions that cache the inverse of a matrix.
##Mzee_Said_Kiti

## This function 1 creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  k <- NULL
  set <- function(y){
    x <<- y
    k <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) k <<- inverse
  getInverse <- function() k 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


## This function 2 computes the inverse of the special matrix returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  
  k <- x$getInverse()
  if(!is.null(k)){
    message("getting cached data")
    return(k)
  }
  mat <- x$get()
  k <- solve(mat,...)
  x$setInverse(k)
  k
}
