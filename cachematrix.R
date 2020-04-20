## This program contains two functions namely - makeCacheatrix and cacheSolve which cache the inverse of a matrix

## This function creates a special matrix object that chaches its inverse


makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) s <<- inverse
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
        
## This function will compute the inverse of the matrix that is returned by the makeCacheMatrix function  
}

cacheSolve <- function(x, ...) {
  s <- x$getinv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  matrix <- x$get()
  s <- solve(matrix, ...)
  x$setinv(s)
  s
}

