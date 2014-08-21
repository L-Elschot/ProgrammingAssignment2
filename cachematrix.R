## You dont want to repeat the same calculations for the same data
## over and over. Therfore the functions makeCacheMatrix and cacheSolve can store 
## values in the cache and retrieve them upon requess. This can improve the speed of your analysis,
## especially usefull for large datasets. 


## makeCacheMatrix will store the matrix that has to be solved. 
## If it was previously stored it will print the inversed matrix upon request cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve will call makeCacheMatrix to see if there is a solved matrix in the cache, which will then be returned. 
## If this is not the case, the cacheSolve function will solve and print the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
