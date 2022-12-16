## makeCacheMatrix 
## This function creates a special "matrix" object that can cache its inverse.

makeCachematrix <- function(x= matrix()){
  invers <- NULL
  set <- function(y){
    invers <<- NULL
    x<<- y
  }
  get <- function(x) x
  setinvers <- function(solve) invers<<- solve
  getinvers <- function() invers
  list(set=set, get= get, setinvers=setinvers,getinvers=getinvers)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by
#makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## function to Caching the Inverse of a Matrix
cachSolve <- function(x=...){
  invers<- x$getinvers()
  if(!is.null(invers)){
    message("getting cached data")
    return(invers)
    
  }
  data <- x$get()
  invers <- solve(data)
  x$setinvers(invers)
  invers
}