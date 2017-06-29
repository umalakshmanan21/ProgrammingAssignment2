## Put comments here that give an overall description of what your
## functions do

## To compute a matrix, inverse of the matrix and store it in a variable to be accessed by 
## another function

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invs <<- inverse
  getInverse <- function() invs
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## To get the inverse of a matrix from cached memory

cacheSolve <- function(x, ...) {
  ## Return an inverse of matrix x
  invs <- x$getInverse()
  if (!is.null(invs)) {
    return(invs)
  }
  mat <- x$get()
  invs <- solve(mat, ...)
  x$setInverse(invs)
  invs
}

## Testing
my_matrix <- makeCacheMatrix(matrix(1:6, 2, 2))
my_matrix$get()
cacheSolve(my_matrix)
my_matrix$getInverse()
