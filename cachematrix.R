## The 'makeCacheMatrix' function sets and gets the values for the input matrix, 
## then it sets and gets the values of the resulting inverse matrix to be cached 
## if the 'cacheSolve' function has been run previously. 
## 
## 'makeCacheMatrix' has 4 nested functions.
makeCacheMatrix <- function(x = matrix()) {
    #Create empty variable 'm'
    m <- NULL
    #This function allows for setting the matrix 'x' directly regardless of 
    #input argument and set 'm' to NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #Assign input argument to 'x'
    get <- function() x
    #Return 'm', the value of 'inverse' is assigned to 'm' after 'cacheSolve' 
    #is run; otherwise is NULL
    setInverse <- function(inverse) m <<- inverse
    #Return the value of 'm'
    getInverse <- function() m
    #The function will return these values
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## The 'cacheSolve' function returns the inverse for matrix x, which is 
## the argument for the 'makeCacheMatrix' function. The argument for this
## function should contain the results from running 'makeCacheMatrix'.
cacheSolve <- function(x, ...) {
    #Assign value of getInverse to 'm'
    m <- x$getInverse()
    #Check if matrix inverse already exists (not NULL)
    # if it exists, display message; if it doesn't, continue to execute solve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #Assign value of original 'x' matrix to 'data' 
    data <- x$get()
    #Execute matrix inversion, solve()
    m <- solve(data, ...)
    #Pass the value of 'm' to setInverse
    x$setInverse(m)
    #Return inverse matrix
    m
}
