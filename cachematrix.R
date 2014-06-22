## Following functions are used to demonstrate the capability of caching
## your data to reduce memory usage and improve speed.

## makeCacheMatrix will create a special "matrix" object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
     s <- NULL
     set <- function(y){
          x <<-y
          s <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) s <<- solve
     getsolve <- function() s
     list (set=set, get=get,
           setsolve=setsolve,
           getsolve=getsolve)

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## assume that the matrix supplied is always invertible
     s <- s$getsolve()
     if(!is.null(s)){
          message("retrieving cached data")
          return(s)
     }
     data<-x$get()
     m <- solve(data,...)
     x$getsolve(s)
     s
}
