## Calculates the inverse of an invertible matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-matrix()
  set<-function(size=NA,nrows=1,ncols=1)
  {
    x<<-matrix(data=size,nrow=nrows,ncol=ncols)
    m<<-matrix()
    
  }
  get <- function()
  {  
    x
  }
  setinverse <- function(inverse) 
  {
    
    m <<- inverse
  }
  getinverse <- function()
  {
    m
  }
  list(set=set,get = get,setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(length(m)==1 & is.na(m[1,1])==TRUE)
  {
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    return(m)
  }
  
  message("getting cached data")
  m
       
}
