## two functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## @x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
inv =null

set <- function(y) {
  ##the <<- operator which can be used to assign a value to an object 
  ##in an environment that is different from the current environment.
  x <<- y
  inv <<- NULL
}

get <- function() x

setinv <- function(inverse) inv <<- inverse

getinv <- function() inv

list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}


## Below function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()

    if(!is.null(inv)) {
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.)
  x$setinv(inv)
  inv
}
