## This file contains two functions: 
## makeCacheMatrix: The optional input is a matrix. The output is a list of 4 functions (2 accesors and 2 mutators).
##                  The 4 functions access/mutate a matrix and its inverse 
## cacheSolve: Access matrix stored in makeCacheMatrix object passed as input and set and retrun the inverse of the matrix.  

## makeCacheMatrix internally caches a matrix and its inverse. Theses matriciese can be accessed using the 
## respective accessor functions and be set using respective mutator functions.
## input x is optional. Default value is the empty matrix. input x is assumed to be an invertable matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <- y
    i <- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes a makeCacheMatrix object as input(assumption). It then retreives and returns the inverse matrix
## stored in the makeCacheMatrix object. If the inverse does not exist the inverse is calculated and stored
## in the makeCacheMatrix object and the inverse matrix is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
