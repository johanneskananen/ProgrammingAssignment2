## The function makeCacheMatrix does the following things
## 1) Set the value of the matrix
## 2) Get the value of the matrix
## 3) Set the value of the inverse
## 4) Get the value of the inverse

## The function creates a special "matrix" that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
## setting the value of the matrix
        set <- function(y) {
              x <<- y
              m <<- NULL
    }
## getting the value of the matrix
        get <- function() x
## setting the value of the inverse matrix
        setinverse <- function(inversef) m <<- inversef
## setting the value of the inverse matrix
        getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function "cacheSolve" takes x as an argument, tests whether its inverse
## has been calculated and returns the cached value if it exists.
## Otherwise it will inverse the x with "solve" function and return
## the inversed function
cacheSolve <- function(x, ...) {
## Returns cached matrix if it has been saved to m    
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
## Solve the inverse of the given matrix 
    m <- solve(data, ...)
## Setting the inverse that was solved 
    x$setinverse(m)
## Return a matrix that is the inverse of 'x'
    m
}
