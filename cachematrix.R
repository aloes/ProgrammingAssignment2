## Put comments here that give an overall description of what your
## functions do
# Should check, whether a matrix has been inversed, cache the inversed one
# and use it, if values are eqaul.

## Write a short comment describing this function
# It doesn't work properly.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  y <- NULL

  set <- function(y)
    x <<- y
    s <<- NULL

  get <- function(x) x
  
  setsolved <- function(solve) s <<- solve
  getsolved <- function() s
  
  list(set = set, get = get,
       setsolved = setsolved,
       getsolved = getsolved)
}


## Write a short comment describing this function
# This function should compare the new matrix to the old matrix and
# do nothing in case they are equal. I other case the inverted matrix
# should be returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolved()

  if(!is.null(s)) {
    if(x$set(data) == x$get(x)) {
        message("getting cached data")
        return(s)
      }
    data <- x$get()
    x$set(data)
    s <- solve(data, ...)
    x$setsolved(s)
  }
  s
}
