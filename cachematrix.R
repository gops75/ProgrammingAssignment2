## This file features two main functions -- makeCacheMatrix and
## cacheSolve. The makeCacheMatrix function creates a special 
## matrix object that can cache its inverse. The cacheSolve function
## computes the inverse of the special matrix returned by makeCacheMatrix
## outputs the inverse. If the inverse has been already calculated
## (and the matrix hasn't changed), then cacheSolve should retrieve
## the inverse from the cache.


## makeCacheMatrix function takes the input from the user; the input
## needs to be a square matrix, and will display a message if anything
## else is input by the user.

## The makeCachematrix contains four functions set(newmat), get(), 
## setinv(iMat) and getinv() which are used to set (or initialize)
## a matrix, get the matrix, set or initialize the inverse matrix
## and get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  if(is.matrix(x) & nrow(x) == ncol(x)) {
    s <- nrow(x)
    invMat <- matrix(list(), nrow = s, ncol = s)
    set <- function(newmat) {
      if(is.matrix(newmat) & nrow(newmat) == ncol(newmat)) {
        x <<- newmat
        s <<- nrow(newmat)
        invMat <<- matrix(list(), nrow = s, ncol = s)
      } else print("Please ensure the input to be a square matrix")
    }
    get <- function() x
    setinv <- function(iMat) {
      if(is.matrix(iMat) & nrow(iMat) == ncol(iMat)) {
        invMat <<- iMat
      } else print("Please ensure the matrix entered is square")
      invMat
    }
    getinv <- function() invMat
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
  } else print("Please supply a square matrix")
  
}

## cacheSolve function feeds on the stored values of list of four
## functions inside makeCacheMatrix i.e. set, get, setinv and 
## getinv; It checks invMat from makeCacheMatrix, and if it is 
## null, it gets the matrix from the get() and solves for the 
## inverse; if invMat turns out to be a non-null matrix, the
## cacheSolve outputs that invMat as the inverse (also prints
## a user-friendly message saying "getting cached Inverse")

cacheSolve <- function(x, ...) {
  invMat <- x$getinv()
  if(!is.null(unlist(invMat))) {
    message("getting cached Inverse")
    return(invMat)
  }
  data <- x$get()
  invMat <- solve(data)
  x$setinv(invMat)
  invMat
}