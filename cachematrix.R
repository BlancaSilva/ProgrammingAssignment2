## Using these two functions will allow you to calculate
## the inverse of a matrix and save the value in cache



## This function will create a list with the four functions
## that will be needed in cachesolve to calculate the value
## of the inverse of the matrix x. The inverse matrix
## is called i. The set() is used to modify the value of 
## the arguments (if needed to recalculate inverse with different
## set of values). get() imports the values introduced in the
## function (x), setinv() gives the variable i the value of
## the inverse of the matrix x. getinv imports the value
## of i in cache.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL 
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## Cachesolve will use the previous list of functions. It will
## first check if the value of the inverse (getinv of 
## the list) already exists, i.e. the value is not null,
## (in case of not null it prints "getting cached data" 
## and returns the value in cache). If i is null, it
## gets the values of the matrix (x$get()), calls them "data"
## and calculates the inverse. Finally, it sets the value of
## the inverse in cache (setinv) and prints the inverse (i)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv() #this will be the inverse matrix
        
        # If the inverse is already cache (i not null)
        # we won't calculate it
        
        if(!is.null(i)) {
                message("getting cached data") 
                return(i) 
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
