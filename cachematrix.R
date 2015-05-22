## The first function creat an object "matrix" which contains a list
## of function. The second function caculate its inverse matrix but
## looking for the cache first. If there has been an result of the 
## input matrix, process of caculating will be skipped

## creat object "matrix" with a list of function

makeCacheMatrix <- function(x = matrix()) {
        inv <- matrix()
        set <- function(y){
                x <<- y
                inv <<- matrix()
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## caculate the inverse of the matrix by looking for cache first

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.na(inv)){
                message("getting cached result")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
