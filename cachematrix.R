##These functions will cache the inverse of a matrix so repeated computations aren't required.
## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
         i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function computes the inverse of the special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

 D <- matrix (c(1,2,3,4), 2, 2)
> D1 <- makeCacheMatrix(D)
> cacheSolve(D1)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(D1)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
