## Programming assignment 2 for R programming course. 
## This assignment demonstrates how to cache the results of 
## time consuming computations using scoping rules of the R
## language. 

## Create a special 'matrix' which is really a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function()
        x
    setinverse <- function(inverse)
        i <<- inverse
    getinverse <- function()
        i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## Return a matrix that is the inverse of x, using the caching 
## methods defined in the function just above, makeCacheMatrix. 

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
