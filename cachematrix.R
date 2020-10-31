## Put comments here that give an overall description of what your
## functions do

## There are two functions makeCacheMatrix,makeCacheMatrix
##makeCacheMatrix consists of set,get, setinv, getinv
##library(MASS) is used to calculate inverse for non squared as well as squared matrices
library(MASS)
makeCacheMatrix <- function(x=matrix()){
  inv <- NULL           #initializing inverse as NULL 
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x           #function to get matrix x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() {
                           inver<-ginv(x)              #function  to obtain the inverse of the matrix
                           inver%*%x
                           }
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##write a short comment describing this function
##This is used to cache data

cacheSolve <- function(x, ...)   ##gets cache data
  {
  inv <- x$getinverse()
  if(!is.null(inv)){                      #checking whether inverse is NULL
    message("getting cached data!")
    return(inv)                           #returns inverse value 
  }
  data <-x$get()
  inv <- solve(data, ...)                  #calculates inverse value
  x$setinverse(inv)
  inv           ## Return a matrix that is the inverse of 'x'
}