## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a list that contains function that
# Set and get the value of the matrix
# Set and get the value of the inverse of the matrx

makeCacheMatrix <- function(x = matrix()) 
{
  inverse <-  NULL
  set <- function(y)
  {
    x <<- y
  }
  
  get <- function() return(x)
  setinverse <- function(inv)inverse <<- inv
  getinverse <- function() return(inverse)
  
  return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))

}


## Write a short comment describing this function
#This function computes the inverse of the matrix returned
#by makeCacheMatrix. If the inverse has already been 
#compute then it will skip and return the cache result. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinverse()
      if(!is.null(inverse))
      {
        message("Getting cached data...")
        return (inverse)
      }
      
      data <- x$get()
      inverse <- solve(data)
      x$setinverse(inverse)
      return(inverse)
}

