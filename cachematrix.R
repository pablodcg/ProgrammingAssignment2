## This function creates a vector of four functions 
## get () returns a matrix of which we want to calculate the inverse
## set () sets the value of the matrix of which we want to calculate the inverse
## setinv() sets the inverse of the matrix, caches its value
## getinv() gets the cached value of inversve of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
   <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This functions receives a vector created by the funcion  makeCacheMatrix
## and returns the inverse of a matrix given in the vector
## If the inverse value has been already calculated, it returns its cached value, 
## if not it calls solve to calculate it ans then it caches the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}