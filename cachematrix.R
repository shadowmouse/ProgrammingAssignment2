## Author : Elliot Francis
## Date Created : 2017-1-18
## Date Modified : 2017-1-18
## Description : Assignment 2 - Demonstrate Lexical Scoping via a Cached Matrix Custom Object
##               Object will manage access to matrix data, returning a cached object on unchanged matrix.

## Set storage object for the matrix and a caching mechanism

makeCacheMatrix <- function(matrix = matrix()) {
  matrixInverse <- NULL
  set <- function (m) {
    matrix <<- m
    matrixInverse <<- NULL
  }
  get <- function () { matrix }
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

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
      print("Returning Cached Inverse")
      return(inverse)
    }
    matrix <- x$get();
    inverse <- solve(matrix);
    x$setInverse(inverse)
    inverse
}
