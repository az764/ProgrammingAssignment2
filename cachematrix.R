## The below two functions can be used to cache the inverse of a matrix


## The makeCacheMatrix function below can be used to create a special 'matrix' object which can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function() inv <<- solve(x)
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The below cacheSolve function computes the inverse of the matrix returned by the makeCacheMatrix. If the inverse has
## already been calculated then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getinv()
  inv <- solve(data, ...)
  x$setinv(x)
  inv
}
