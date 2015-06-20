

## Assignment 2
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## set_inverse function.

## This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$set_inverse(inv)
    inv
}

## Sample run:
## > x = rbind(c(1, -1/5), c(-1/5, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##        [,1] [,2]
##  [1,]  1.0 -0.2
##  [2,] -0.2  1.0

## No cache in the first run
## > cacheSolve(m)
##        [,1]      [,2]
##   [1,] 1.0416667 0.2083333
##   [2,] 0.2083333 1.0416667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##        [,1]      [,2]
##   [1,] 1.0416667 0.2083333
##   [2,] 0.2083333 1.0416667
## > 


