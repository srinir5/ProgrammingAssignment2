## R Programming - Assignment-2
## Function to cache the inverse of a matrix
## The special matrix stores inverse and has functions to get and set

## This function creates cache matrix

makeCacheMatrix <- function(x = matrix()) {
  # Initialize inverse matrix to null
  m_inverse <- NULL
  
  # set matrix. so reset inverse to null
  set <- function (y) {
    x <<- y
    m_inverse <<- NULL
  }
  
  # return orignial matrix
  get <- function () x
  
  # set inverse
  setInverse <- function (mi) {
    m_inverse <<- mi
  }
  
  # return inverse
  getInverse <- function () m_inverse
  
  # list of function calls for this special matrix    
  list (set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## Function to compute inverse of a matrix. 
## Function call get/set in makeCacheMatrix to get inverse from cache
cacheSolve <- function(x, ...) {
  
  # First get inverse from cache
  m_Inverse = x$getInverse()
  
  # If inverse from cache is null then compute inverse
  if (is.null(m_Inverse)) {
    
    # Compute inverse
    print ("Computing inverse..") 
    
    # Check if matrix is invertible
    if (determinant(x$get(), logarithm = FALSE)$modulus == 0) {
      print ("Singular matrix cannot be inverted!")      
    }
    else {
      # Matrix is invertible..so call solve()
      m_Inverse = solve (x$get())
      
      # set inverse in special matrix
      x$setInverse (m_Inverse)
    }
  }
  else
  {
    # inverse from cache is not null. so return cached matrix
    print ("Returning from cache..")
  }
  
  m_Inverse        
}
