## Invert Matrix functions.
## The two functions here are used to compute the inverse of a matrix.
## First use "MyMatrix <- makeCacheMatrix(your_square_matrix)" to create a function MyMatrix
## function named MyMatrix initialized with your square matrix.
## Then call "cacheSolve(MyMatrix) to find the inverse on your_square_matrix

## makeCacheMatrix is used to initilize the variables and functions that 
## will be used by cacheSolve
makeCacheMatrix <- function(x = matrix()) {
      ## Initialize m with NULL
      m <- NULL
      
      ## The set function is only used for testing and will change the initial matrix with
      ## a new square matrix specified in y.  
      set <- function(y) {
            ## store the new square matrix in x in the parent function
            x <<- y
            ## initialize m with NULL in the parent function
            m <<- NULL
      }
      
      ## Returns the initial Matrix when called by cacheSolve
      get <- function() x
      
      ## sets m with the solved matrix inverse
      setsolved <- function(solved) m <<- solved
      
      ## returns the matrix inverse if cached or returns NULL
      getsolve <- function() m
      
      ## needed to allow set, get, setsolve and getsolve to be callable by cacheSolve
      list(set = set, get = get,
           setsolved = setsolved,
           getsolve = getsolve)

}

## cacheSolve is used to compute the inverse of a matrix.  If this is the first call to cacheSolve (i.e. m = NULL)
## then compute inverse and store it in parent function environment for possible future calls.
cacheSolve <- function(x, ...) {
      ## returns either the already solved inverse matrix or NULL
      m <- x$getsolve()
      
      ## if m is not NULL then the matrix inverse has already been computed so we simply return
      ## the previously cached inverse data and exit the function
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      
      ## otherwise we need to caculate the inverse and cache the results in the parent function
      ## for possible later use.
      
      ## get the initial square matrix
      data <- x$get()
      
      ## find the matrix inverse using the built-in 'solve' function and store it in to m
      m <- solve(data, ...)
      
      ## save the solved matrix inverse in the parent function
      x$setsolved(m)
      
      ## return the solved inverse matrix (for printing?) and exit
      m
}
