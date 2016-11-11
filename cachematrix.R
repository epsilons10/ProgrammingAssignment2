## Below are two functions... 
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## It returns a list of 4 functions
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

## makeCacheMatrix will accept a matrix object as input and defines 4 functions in it
## it returns 4 functions set, get, setinv and getinv to be called in another function defined below

makeCacheMatrix <- function(x = matrix()) {
     m_inv <- NULL
     set <- function(y) {
          x <<- y
          m_inv <<- NULL
     }
     get <- function() x
     setinv <- function(mat_inv) m_inv <<- mat_inv
     getinv <- function() m_inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
     
}


## cacheSolve will accept the list object with 4 functions in it
## it will check if the parent environment has the inverse value already present then it will display the cached value
## otherwise it will calculatre the inverse of the matrix and store the value in the parent variable by calling setinv function
## set function can be called from the console to set the new matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m_inv <- x$getinv()
     if(!is.null(m_inv)) {
          message("getting cached data")
          return(m_inv)
     }
     data <- x$get()
     m_inv <- solve(data)
     x$setinv(m_inv)
     m_inv
}
