## makeCacheMatrix function sets matrix for future process

## get matrix when needed

## set the inverse matrix

## fetch the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

  m_inv <- NULL
  
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(matrix_inverse) m_inv <<- matrix_inverse
  
  getinv <- function() m_inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## Check existence of inverse

## If inverse exist then return inverse

## If inverse does not exist the set inverse and return calculated inverse

cacheSolve <- function(x, ...) {
  
  m_inv <- x$getinv()
  
  if (!is.null(m_inv)) {
    print("returning cached inverse")
    return(m_inv)
  }
  
  print("returning fresh inverse")
  data <- x$get()
  
  m_inv <- solve(data, ...)
  
  x$setinv(m_inv) 
  m_inv
  
}
