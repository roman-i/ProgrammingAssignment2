## this module has 2 function for cache solve(matrix)

## create a wrapper around matrix with cache inside.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(slv) s <<- slv
  getsolve <- function() s
  list(set = set,
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## if cache is not set - solves the matrix and caches the result, if result is in the cache - just return 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if (!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setsolve(s)
  s
}

## some testing code

test <- function() {
  mat = matrix(c(3, 3.5, 3.2, 3.6), nrow=2, ncol=2)
  m1 <- makeCacheMatrix(x = mat)
  m1s <- cacheSolve(m1)
  m1s2 <- cacheSolve(m1)
}

# t <- test()
