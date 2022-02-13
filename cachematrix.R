## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL     ##considering inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x  ##assign function to get matrix x
  setinv <- function(inverse)inv <<- inverse
  getinv <- function(){
    inver <- ginv(x)
    inver%*%x   ##function assigned to get inverse of matrix x
  }
  list(set = set, get = get,
       setinv = setinv,
       getinve = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) ##to get cache data
  {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data!")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
}
        ## Return a matrix that is the inverse of 'x'

