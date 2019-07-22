## This is a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <-function() x
  setinverse <-function(inverse) s <<- inverse
  getinverse <-function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has allready been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <-solve(data,...)
        x$setinverse(s)
        s
}

## Below are matrices for testing the R functions makeCacheMatrix and cacheSolve
## The solve function computes the inverse of a square matrix
## The R functions work correctly since the results of cacheSolve = solve

A <-matrix(c(4,7,2,6),nrow=2,ncol=2)
Matrix_A <- makeCacheMatrix(A)
cacheSolve(Matrix_A)
solve(A)

## inverse of A is matrix(c(0.6,-0.2,-0.7,0.4))

B <-matrix(c(3,5,4,6),nrow=2,ncol=2)
Matrix_B <- makeCacheMatrix(B)
cacheSolve(Matrix_B)
solve(B)

## inverse of B is matrix(c(-3.0,2.0,2.5,-1.5))
