## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
#################################################################################################################################################

## This function takes in  a square matrix supposedly invertable as argument and returns a list object (cacheable matrix)
## with 4 self contained functions to set values to and get values he from our square matrix, and so for the Inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  Inv<- NULL ## Initial value of  the inverse.
  set <- function(y) { ## Setting new data of type matrix with initial value for inverse as null.
    x <<- y 
    Inv<<- NULL
  }
  get <- function() x  ## Reads data contents 
  setInverse <- function(Inverse) Inv<<- Inverse ## Setting the inverse first time when computed in cacheSolve function.
  getInverse <- function() Inv                   ## Reading the cached inverse without having to compute again
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)                 ## Returns the special cached matrix which is taken as argument in cacheSolve().
}
}


## This function takes the "special/cached matrix object of type list" as argument and returns the inverse
## of the original matrix

cacheSolve <- function(x, ...) {
        
  Inv<- x$getInverse()  ## Getting Inverse from cache
  data <- x$get()       ## Reads the matrix data
  if(!is.null(Inv)) {   ## Looks if inverse is cached already
    message("getting cached data")
    return(Inv)
  }
  else if (det(data)==0) message("Not invertable") ## Invertable matrix always have a non-zero determinant
  else{
  Inv<- solve(data)
  x$setInverse(Inv)
  }
  Inv 
}
