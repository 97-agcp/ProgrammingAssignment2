## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL 
}
    get<-function()x
    setinverse<-function(inverse)m<<-inverse
    getinverse<-function()m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
    }
    

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  j<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mat<-x$get()
  m<-solve(mat,...)
  x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
  m
}
