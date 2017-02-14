## Creates a list that get&set the values of a matrix and its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        ## Initializing objects (x as function argument and m)
        m <- NULL
        
        ## Defines setter for matrix x
        set <- function(y) {
                ## Assigning values to objects in the parent environment
                x <<- y
                m <<- NULL
        }
        
        ## Defines getter for matrix x
        get <- function() x
        
        ## Defines setter for the inverse of matrix x, called m
        setsolve <- function(solve) m <<- solve
        
        ## Defines getter for matrix m
        getsolve <- function() m
        
        ## Create a new object (list) to be used later
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
        ## Return the inverse matrix result cached for the input matrix (if it already exists)
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## or solve the input matrix (to obtain the inverse matrix) and return it
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
