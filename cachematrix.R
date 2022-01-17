#rm(list = ls())
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function: 
#This function takes in a matrix 'x', initializes the inverse to
#null to wipe out any inverse matrix from previous computations. Within this function, we have 4 nested functions,
#set() helps to transfer any input matrix we wish to find its inverse say (y) to the parent environment, in a 
#sense mutating it to x.while maintaining the null value of the inverse.
#get() retrieves the 'new' x matrix.
#setinverse() stores the newly solved inverse into i in the parent environment
#getinverse() returns the cached inverse
#A list is returned which contains these four functions

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y = matrix()){
        x<<-y
        i<<-NULL
    }
    get<- function() x
    setinverse<-function(inverse) i<<-inverse #stores the newly solved inverse into i
    getinverse<- function() i #returns the cached inverse
    L <- list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    return(L)
}

k <- matrix((1:4),nrow = 2, ncol = 2)
myMatrix <- makeCacheMatrix(k)

## Write a short comment describing this function
# cacheSolve takes an input argument returned from makecacheMatrix (in this case, x == myMatrix) and first we try
#to retrieve the cached inverse if this calculation has previously been done. But if an inverse for this data does
#not exist in the cache, then it computes afresh, first gets the data using x$get(), then find the inverse using
#solve, then we transfer this inverse to the parent environment using x$setinverse() and we return this inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse()
    if (!is.null(i)){
        message("getting cached inverse")
        return (i)
    }
    data <- x$get()#set
    i <- solve(data)
    x$setinverse(i)
    i
    
}
# t=cacheSolve(myMatrix)
# t%*%k
