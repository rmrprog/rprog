## Pair of function which 
## - construct a cache matrix object, 
## - computes the inverse of the matrix, and  retain it in the cache matrix for fast retrieval


## Constructs a cache matrix. A cache matrix is a list of functions
## - get : return the matrix
## - set : set the matrix
## - getinverse : return the inverse of the matrix
## - setinverse : set the inverse of the matrix
## Apart from the function get, these methods are implementation details and should not be used directly
makeCacheMatrix <- function(x = matrix()) {
  # variable for cached inverses of m
  m <- NULL
  list(set = function(y) { x <<- y; m <<- NULL; }, 
       get = function() x, 
       setinverse = function(inverse) m <<- inverse,
       getinverse = function() m)
}


## Takes an instance of the cache matrix created using makeCacheMatrix and returns the inverse
## If the cache matrix does not already hold the inverse, then it is computed and set on the cache matrix
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}


# > # create 1000x1000 matrix
# > m = makeCacheMatrix(matrix(rnorm(1e6), nrow=1e3, ncol=1e3))
# > 
# > ## first time slow, no cache
# > system.time(cacheSolve(m))
# user  system elapsed 
# 2.52    0.03    2.55 
# > 
# > ## cache now populated much faster
# > system.time(cacheSolve(m))
# getting cached inverse
# user  system elapsed 
# 0       0       0 


