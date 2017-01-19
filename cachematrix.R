## This function creates a special "matrix" object that can cache its inverse.
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inversion
## 4.  get the value of the inversion

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y){
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


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## minverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setinverse`
## function.Computing the inverse of a square matrix can be done with the `solve`
## function in R.

## Return a matrix that is the inverse of 'x'
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
