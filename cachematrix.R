## This function creates a list that contains 4 member function:
## set, get, setInv and getInv.
## It uses <<- assignment operator so that these internal variables
## are not exposed to the outside environment.

makeCacheMatrix <- function(x = matrix()) {
        xInv <- NULL  #This is where the result of inversion is stored
        set <- function(y) {
                x <<- y
                xInv <<- NULL
        }
        get <- function() x  #Return the input matrix
        setInv <- function(Inv) xInv <<- Inv  #Set the inversed matrix
        getInv <- function() xInv  #Return the inversed matrix
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


cacheSolve <- function(x, ...) {
        m <- x$getInv()  #Get the inversed matrix from object x
        if(!is.null(m)) {
                message("Getting Cached Data!")
                return(m)
        }  #Return the calculated inversion if the result exists
        data <- x$get()  #If not, do x$get to get the matrix object
        m <- solve(data)
        x$setInv(m)
        m
}
