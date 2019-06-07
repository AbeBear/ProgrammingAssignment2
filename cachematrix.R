## makeCacheMatrix - creates matrix-like object with cached inverse
## cacheSolve - inverts matrix-like object created with makeCacheMatrix

## Takes matrix and returns list with functions to
## (set up matrix, get matrix values, 
## set up inverse matrix, get inverse matrix values)

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  setMatrix <- function(y){
    x <<- y
    cachedInverse <<- NULL
  }
  getMatrix <- function() x
  setCache <- function(inverse) cachedInverse <<- inverse
  getCache <- function() cachedInverse
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setCache = setCache,
       getCache = getCache)
}


## Takes matrix from above function
## checks if Inverse has already been computed
## if not, computes inverse

cacheSolve <- function(x, ...) {
  cachedInverse <- x$getCache()
  if(!is.null(cachedInverse)){
    message("getting cached data")
    return(cachedInverse)
  }
  matrix <- x$getMatrix()
  cachedInverse <- solve(matrix)
  x$setCache(cachedInverse)
  cachedInverse
}
