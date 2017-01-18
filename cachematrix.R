## Author : Elliot Francis
## Date Created : 2017-1-18
## Date Modified : 2017-1-18
## Description : Assignment 2 - Demonstrate Lexical Scoping via a Cached Matrix Custom Object
##               Object will manage access to matrix data, returning a cached object on unchanged matrix.

## Set storage object for the matrix and a caching mechanism
## Inputs : matrix (matrix) - Matrix Variable to be Inverted and Cached
## Outputs : List - Contains
##    set(m) - m (matrix) to update to nulling the cache in the process
##    get() - Returns Current Matrix
##    getInverse() - Returns matrixInvers Value (including null)
##    setInverse(inverse) - sets stored matrix inverse

makeCacheMatrix <- function(mtrx = matrix()) {
  matrixInverse <- NULL
  set <- function (m) {
    mtrx <<- m
    matrixInverse <<- NULL
  }
  get <- function () { mtrx }
  setInverse <- function (inverse) { matrixInverse <<- inverse }
  getInverse <- function() { matrixInverse }
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

## Solve a matrix, obtaining it's inverse. Returns a Cached Value if the inverse was already calculated
## Inputs : x (makeCacheMatrix Object) - matrix to be inverted
##    ... - Additional Unspecified Arguments
## Outputs : inverse (matrix) - inverse of the specified matrix (cached or calculated)
## Notes : Function Contains Print Outputs -- Triggered on returning cached input


cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
      print("Returning Cached Inverse")
      return(inverse)
    }
    mtrx <- x$get();
    inverse <- solve(mtrx);
    x$setInverse(inverse)
    inverse
}
