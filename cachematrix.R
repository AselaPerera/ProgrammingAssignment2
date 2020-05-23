
##The function makeCacheMatrix create an R object that stores a matrix and its inverse
##cacheSolve function requires argument that is returned by makeCachematrix order to retrieve the inverse from the cached inverse matrix that is stored in the makeCacheMatrix() object's environment


##makeCacheMatrix() is that it builds a set of function and returns the functions within a list to the parent enviroment,
##The object create when giving argument(matrix) to the makeCacheMatrix() contains four functions:set(),get(),setsolve() and getsolve().And also include two data objects x and m.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##cacheSolve() is required to retrieve the inverse matrix
##from an object type makeCacheMatrix()


cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
