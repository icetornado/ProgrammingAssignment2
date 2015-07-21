## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inversedM <- NULL
    
    set <- function(d, size) {
        x <<- matrix(data = d, nrow = size, ncol = size)
        print("x setter")
        print(x)
        inversedM <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setInversed <- function(inMatrix) {
        inversedM <<- inMatrix
    }
    
    getInversed <- function() {
        inversedM
    }
    
    list(set = set, get = get, setInversed = setInversed, getInversed = getInversed)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMatrix <- magicMatrix$getInversed()
    print("inv M")
    print(invMatrix)
    
    if(!is.null(invMatrix)) {
        message("getting cached inversed matrix")
        return(invMatrix)
    }
    else {
        orgMatrix <- magicMatrix$get()
        print("org M")
        print(orgMatrix)
        
        invMatrix <- solve(orgMatrix)
        magicMatrix$setInversed(invMatrix)
        invMatrix
    }
}
