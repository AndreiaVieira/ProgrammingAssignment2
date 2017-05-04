##  makeCacheMatrix and cacheSolve 
makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(y){
    x <<- y 
    inverse <<- NULL  
  }
  
  get <- function() x
  setInverse <- function(Inverse) inverse <<- Inverse
  getInverse <- function() inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)    
}


##  Solves the inverse of the cached matrix created by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("Getting the cached inverse matrix")
    return(inverse)
  }
  
  aux_matrix <- x$get()
  inverse <- solve(aux_matrix, ...)
  x$setInverse(inverse)
  inverse
  
}


