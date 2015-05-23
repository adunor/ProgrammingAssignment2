## Coursera R Programming Assignment 2
## Two functions used to Cache the inverse of a matrix

## The first function, makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This second function computes the inverse of the matrix returned from 
## the makeCacheMatrix() function. It returns the cached inverse if the inverse had previously
## been calculated and the matrix has not changed otherwise it calculates and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
       message("Getting cached matric inverse")
       return(m)
   }
   data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
