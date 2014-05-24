## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly


makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse.
  ## makeCacheMatrix(x) : special "matrix" object (a list of functions)
  ## x                  : a regular matrix.
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  ## makeCacheMatrix returns a list of functions:
  
  ## set(): set the value of the matrix
  ## get(): get the value of the matrix
  ## setsolve(): set the value of inverse of the matrix
  ## getsolve(): get the value of inverse of the matrix
  
  ## set/get of matrix, set/get of solution
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  ## cacheSolve(x): inverse of x, 
  ## x:             object returned by makeCacheMatrix()
  ## 
  ## This function assumes that the matrix is invertible
  ##
  ## First it checks if the inverse matrix has been computed already.
  ## If so, it retrieves the result from the previous computation, otherwise if computes it
  ## and stores the result in the cache for later use.
  ##
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
