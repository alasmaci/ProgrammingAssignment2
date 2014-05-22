
# The following two functions are used to create a special object 
# that stores a matrix object and cache's its inverse. The latter 
# function checks to see if the cache contains the  calculated inverse.
# If not the inverted matrix is calculated from the data.


## `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.



makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {        # This function sets the value
        x <<- y                 # of the matrix to be solved
        m <<- NULL
    }
    get <- function() x         #  This function gets the value of
                                #  the inverse of the matrix
    setSolve <- function(solve) m <<- solve  #  The matrix is solved
    getSolve <- function() m    # This gets the value of the solved matrix
    list(set = set, get = get,  # 
         setSolve = setSolve,   # A list of the above functions is created
         getSolve = getSolve)   #
}

####################################################################
# This part checks to verify makeCacheMatrix (if desired)
# Not required for the assignment

x <- matrix(rnorm(900,500,20),nrow=30,ncol=30) # Creates a square matrix
xx <- makeCacheMatrix(x)                       # makes the matrix inverse

xx$get()                                       # The original matrix
xx$setSolve(solve(x))
xx$getSolve()                                  # The solved matrix

#####################################################################

## This function computes the inverse of the special "matrix" 
## returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), 
## then`cacheSolve` retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
       
    m <- x$getSolve()   # The solved original matrix (if it exists)
    if(!is.null(m)) {   # Checks to see if the inverse is cached
        message("getting cached data")
        return(m)       # Returns the solved matrix (if it exists)
    }
    data <- x$get()        # If inverse is not cached
    m <- Solve(data, ...)  # solves the data for the inverse
    x$setSolve(m)          # solvesthe matrix
    m                      # Returns the solved matrix   
}

#################################################################
# This part checks to verify makeCacheMatrix (if desired)
# Not required for the assignment

cacheSolve(xx)             #  Returns the solved matrix from the cache
                           #  or solves the matrix if no cached solution

#################################################################


