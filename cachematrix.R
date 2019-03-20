## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. 
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 


## Write a short comment describing this function
#The following functions are used to create a special object that stores a matrix and caches its inverse. 
#The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
# 1.set the value of the matrix 2. get the value of the matrix 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    ## Return a matrix that is the inverse of 'x'
}
