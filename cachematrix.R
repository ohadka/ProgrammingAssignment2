## Assignment 2 - Lexical Scoping
## This assignment includes 2 functions that provide the ability to inverse
## a Matrix and cache the result in order to save repeated computations

## makeCacheMatrix function creates a list of functions that:
## 1. Set the Matrix values
## 2. Get the Matirx values
## 3. Set the inverse Matrix values
## 4. Get the inverse Matrix values

makeCacheMatrix <- function(x= matrix()) {
  inv<-NULL
  set<-function(y)
  {
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv <- function(invs) inv <<- invs
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function first checks if the inverse Matrix values are Cached.
## In case they are, it returns the cached inverse Matrix values,
## If they are not cached, it will calculate the inverse Matrix values
## and cache them for future reference using the functions under makeCahceMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}