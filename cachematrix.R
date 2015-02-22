## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


##"solve" is the command used to find an inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    s = NULL; #sets value to null as default
    set = function(y) {
        x<<-y; ##this creates a cache for the matrix input to the function
        s<<-NULL; ## sets to null
    }
    get = function() x; #defines the get function
    setsolve = function(solve) s<<- solve; #defining the setsolve function
    getsolve = function() s; #defining the getsolve function
    list(set = set, get = get, 
         setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x=matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    s = x$getsolve(); #gets the inverse matrix if there is one
    if(!is.null(s)){ #this will run if cacheSolve has not been called
        message("getting the cached data")
        return(s);
    }
    matrix = x$get(); ##gets the input
    s = solve(matrix,...); #computes the inverse of the input
    x$setsolve(s) ##this runs to cache the inverse
    s ##returns the inverse to the screen
}
