## Computing matrix inversion costs lots of time. If we can store the result in cache,
## next time it don't need to compute again. It can save your time. 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL       # initialize the inverse of a matrix and it is NULL
    set <- function(y) {
        x <<- y     # if user have new matrix, use set() function to store
        i <<- NULL  # set NULL to inverse of a matrix 
    }
    get <- function() x     # if user want to get matrix, use get() to return it
    setinv <- function(inv) i <<- inv   # if user have the inverse of a matrix, user can set it directly
    getinv <- function() i  # if user want to get inverse of a matrix, use getinv() to return it
    list(set = set, get = get,  # add all four function as list  
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(x, ...) {
    i <- x$getinv()     # return a matrix that is the inverse of 'x'
    if(!is.null(i)) {   # if we already have inverse of 'x', just return to it
        message("getting cached data")
        return(i)
    }
    data <- x$get()     # if we didn't have the inverse of 'x', get the matrix to 'data'
    i <- solve(data, ...)   # compute the inverse of 'x' and store it to 'i'
    x$setinv(i)         # set new 'i' to cache
    i                   # print the result
}

