## function makeCacheMatrix consists two pairs of getter/setter for 
## 1. x - a variable to store a numeric matrix in a different enviroment
## 2. inversedM - a local variable to store inversed matrix of x
makeCacheMatrix <- function(x = matrix()) {
	inversedM <- NULL
	 
    set <- function(inMatrix) {
        x <<- inMatrix
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


## cacheSolve function takes a makeCacheMatrix "object" as an input argument
## output is a inversed matrix of the matrix which is created by calling 
## set method of the makeCacheMatrix "object"

cacheSolve <- function(magicMatrix, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMatrix <- magicMatrix$getInversed()
    
    if(!is.null(invMatrix)) {
        message("getting cached inversed matrix")
        return(invMatrix)
    }
    else {
        orgMatrix <- magicMatrix$get()
        message("making a new inversed matrix")
        
        invMatrix <- solve(orgMatrix)
        magicMatrix$setInversed(invMatrix)
        return(invMatrix)
    }
}

## usage:
## 1/
## myMatrix <- matrix(1:4, 2, 2)
## myMagicMatrix <- makeCacheMatrix()
## myMagicMatrix$set(myMatrix)
## invMatrix <- cacheSolve(myMagicMatrix)
## print(invMatrix)     ## expect get an inversed matrix

## 2/
## myMatrix <- matrix(5:8, 2, 2)
## invMatrix <- cacheSolve(myMagicMatrix)
## print(invMatrix)     ## expect to get from cache
## myMagicMatrix$set(myMatrix)
## invMatrix <- cacheSolve(myMagicMatrix)
## print(invMatrix)     ## expect to get a new inversed matrix

