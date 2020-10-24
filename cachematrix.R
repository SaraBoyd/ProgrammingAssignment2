## There are two functions in this file:  makeCacheMatrix and cacheSolve.
## These functions take a matrix and return its inverse.  If the 
## matrix has already been inverted, the function returns the matrix that has
## already been calculated from the cache.
## It is assumed that the matrix supplied is always invertible.


## The makeCacheMatrix function creates an R object that stores a vector and  
## its inverse matrix.  The object contains four functions set(), get(),  
## setinversem(),and getinversem().  It also contains the two data objects 
## x and im.  This function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
## x is initialized as a function argument.
## Sets the value of im to NULL.
    im <- NULL

## Sets the value of the vector.     
    set <- function(y) {
        
## The form of the assignment operator <<- below assigns the input argument 
## to the x object in the parent environment.
    x <<- y
        
##  Below the value of NULL is assigned to the im object in the parent
##  environment.  This line of code clears any value of im that has been
##  cached by a prior execution of cacheSolve().
    im <<- NULL
    }
    
## Gets the value of the vector
    get <- function() x
    
## Sets and gets the inverse matrix.
    setinversem <- function(inversem) im <<- inversem
    getinversem <- function() im
    
## Below code assigns each of these functions as an element within a list(), 
## and returns it to the parent environment. 
    list(set = set, get = get,
         setinversem = setinversem,
         getinversem = getinversem)
}


## The cacheSolve function requires an argument that is returned by 
## makeCacheMatrix() in order to retrieve the inverse matrix from the cached  
## value that is stored in the makeCacheMatrix() object’s environment.
## In other words this function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.  If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getinversem()
    if(!is.null(im)) {ls
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinversem(im)
    im
}
