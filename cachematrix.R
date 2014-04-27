## ProgrammingAssignment2 : Coursera R-Programming Course
## File : cachematrix.R
## Functions for caching matrix inverse value

## makeCacheMatrix  
### Creates a special matrix-wrapper that can cahce the matrix-inverse
### get : returns the matrix
### set : sets matrix and clears cache
### getinverse : returns matrix inverse from cache
### setinverse : sets the matrix inverse in the cache
makeCacheMatrix <- function(x = matrix()) {
    if ( !is.matrix(x))
    {
       warning("Invalid input to makeCacheMatrix.")
       return;
    }
    m_inv <- NULL
    set <- function(y) {
        x <<- y
        m_inv <<- NULL;
    }
    get <- function() x
    setinverse <- function(inv) m_inv <<- inv
    getinverse <- function() m_inv
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse )
}

## chacheSolve  : 
### Takes input a matrix-wrapper object and gets matrix-inverse  
###       if already present in the cahce. 
###       else computes the inverse, saves in cahce and return the inverse values.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m_inv <- x$getinverse()
    if ( !is.null(m_inv)) {
        ## lucky you found in the cache
        message("getting matrix inverse from cache..")
        return (m_inv)
    }
    
    ## matrix inverse is not found in cache
    data <- x$get()
    ## compute the inverse
    m_inv <- solve(data)
    ## save it in cache for subsequent use
    x$setinverse(m_inv)
    m_inv
}
