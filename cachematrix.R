## Put comments here that give an overall description of what your
## functions do

## There are two functions makeCacheMatrix, cacheSolve
## makeCacheMatrix cosist of set, get, setinv, getinv
makeCacheMatrix <- function(x = matrix()) {       # This function creates a special "matrix" object that can cache its inverse
     inv <- NULL                                  # initialize inv as NULL
     set <- function(y){                          # define the set function to assign new
          x <<- y
          m <<- NULL
     }
     get <- function()x                           # function to get matrix x
     setinv <- function(inverse) inv <<- inverse  # assigns value of inv in parent environment
     getinv <- function(){
          inver <- ginv(x)
          inver%*%x                               # function to obtain inverse of the matrix
     }
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}

## This is used to get cache data
cacheSolve <- function(x, ...) {             
     inv <- x$getinv()                       # return a matrix that is the inverse of 'x'
     if(!is.null(inv)) {                     # checking whether inverse has already been calculated
          message("getting cache data!")
          return(inv)                        # get it from the cache and skips the computation then returns inverse value
     }
     data <- x$get()
     inv <- solve(data,...)                  # otherwise, calculates inverse value
     x$setinv(inv)
     inv                                     # return a matrix that is the inverse of 'x'
} 
