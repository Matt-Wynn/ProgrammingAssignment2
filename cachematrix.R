## This cachematrix.R file contains two functions.
## The first, makeCacheMatrix sets up a special matrix to be used to
## cache a matrix and store the information in working memory.
## The second function, CacheSolve checks if the matrix inverse is already cached,
## and either returns the solved cached data or calculates the matrix inverse
## and stores it in memory.

## This function cashes the matrix, using I (for Inverse) as the variable,
## and names components with the function solve to create a special matrix

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) I <<- solve
    getsolve <- function() I
    list (set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}

## Write a short comment describing this function
## The following checks to see if the inverse (solve) has already been calculated.
## If yes, it gets the inverse from the cache and skips a new calculation.
## If no, it calculates the inverse and sets this in the cache.

cacheSolve <- function(x, ...) {
    I <- x$getsolve()
    if(!is.null(I)) {
      message("getting cached data")
      return(I)
    }
    data <- x$get()
    I <- solve(data)
    x$setsolve(I)
    I
}
