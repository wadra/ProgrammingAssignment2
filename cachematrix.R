# Script's puspose is to store a user provided martix and cache value 
# of the inverse of the matrix. After an inverse has been solved, on invoking
# same inverse through CacheSolve() values are pulled out from Cache rather
# than being computed again

#### Testing Instructions
# 1- First set matrix first by using makeCacheMatrix and assign to a variable.
# 2- use the variable to get / set new matrix
# 3- use cacheSolve with the variable defined and assigned to get innverse.
# 4- Test cacheSolve to see if data is cached from memory
# 5- use getinverse with assigned variabel for inverse results also

# makeCacheMatrix function starts here
# argument x is of type matrix() so pass only matrix type argument

makeCacheMatrix <- function(x = matrix()) 
#makeCacheMatrix function returns a function to new_x as most recent declared value of x
{
  # declares and holds the cached value, initially set to NULL
  cache <- NULL
  
  # to store a matrix values declared through cbind into x variable
  # CacheMatrix retuned value goes into argument of SetMatrix function
  setMatrix <- function(new_x) 
  {
    # x variable is available in the child environment of makeCacheMatrix
    # Lexical Scoping
    x <<- new_x
    cache <<- NULL
  }
  
  # returns the stored matrix
  getMatrix <- function() 
  {
    x
  }
  
  # cache the given argument 
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # get the cached value
  getInverse <- function() 
  {
    cache
  }
  
  # return a list. Each named element of the list is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# The following function calculates the inverse of a 2*2 matrix created with 
# makeCacheMatrix
cacheSolve <- function(y, ...) {
  # get the cached value
  inverse <- y$getInverse()
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("Values coming from the Cache")
    return(inverse)
  }
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  #call/return inverse matrix
  inverse 
}

# Disclaimer: Taking the course again for verified certificate. Deleted old repo, re made my previous code and committed after a new fork from scratch.