## Program for Caching the Inverse of a Matrix

## We assume that the matrix supplied is always invertible and it is squeare matrix.
## We are using R function  solve(X) which return inverse Matrix.


##  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {    ## set matrix value 
    x <<- y
    m <<- NULL
  }
  get <- function() x    ## get matrix value
  set_inv_matrx <- function(solve) m <<-solve  ## set inverse matrix
  get_inv_matrx <- function() m                ## get inverse matrix
  list(set = set, get = get,
       set_inv_matrx = set_inv_matrx,
       get_inv_matrx = get_inv_matrx)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$get_inv_matrx()
  if(!is.null(m)) {         ## check if data exists
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inv_matrx(m)
  m
}
