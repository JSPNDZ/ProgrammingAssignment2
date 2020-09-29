## The following function lets store a matrix through the setting 
## and getting data.
## Thats functions-results will be called later to inverse the matrix.

## "set" is an object wich stores the matrix in two plus objects: "x" and "inv"
## (previously equalited to "NULL").
## "get" object stores the matrix element to inverse ("x")
## "setinverse" force the asignation to empty object "inv" like a free space to
## calculate the inverse.
## "getinverse" get the inverse value of the matrix.
## Finally the objects are stored like a list type data, to keep in cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)

}


## Write a short comment describing this function
## This function returns the inverse of matrix previosusly collected.
## "inv" takes one of the elements from "x" to evaluate your inverse trough
## base function solve.
## If the matrix is not invertible, we will get a warning (line 40)
## The result is stored in "inv" object

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  
  inv
}

## x = rbind(c(1, -1/4), c(-1/4, 1))
## x

##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## m = makeCacheMatrix(x)
## m$get()
## cacheSolve(m)

##      [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

