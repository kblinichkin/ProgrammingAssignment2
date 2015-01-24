## ------------------------------------------------------------
## The script was created by Kirill Blinichkin at 24.01.2015. |
##                                                            |
## This script contains two helpful functions that allows     |
## calculate inversion of matrices little faster than usual   |
## solve() function.                                          |
## ------------------------------------------------------------


## This function make from matrix a special object that 
## can store inversion calculation.
## x - a invertiyble matrix.
## Return value is a special object.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInversion <- function(inversion) inv <<- inversion
  
  getInversion <- function() inv
  
  list(set = set, get = get,
       setInversion = setInversion,
       getInversion = getInversion)
}


## This function do the same as solve() function, however, 
## it caches the result of calculation.
## x - a spatial object that is returned by makeCacheMatrix.
## Return value is inversion of matrix that is contained in x.

cacheSolve <- function(x, ...) {
  inv <- x$getInversion()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInversion(inv)
  
  inv
}
