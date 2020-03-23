# makeCacheMatrix creates a list containing a function to
# - set the value of the matrix
# - get the value of the matrix
# - set the value of inverse of the matrix
# - get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
#Check to see that the number of rows and columns in the matrix are equal
if (ncol(x)==nrow(x)){
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}else{
    return(message("The matrix is not a square matrix."))
}
}

# Function to compute the inverse of the matrix returned by makeCacheMatrix.
# Checks cache to see if inverse was previously calculated.
# Returns cache if inverse previously calculated. 
# Calculates inverse and sets cache if not previously calculated.
# tryCatch block returns error if the matrix is not invertible. 
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data.")
        return(i)
    }
    data <- x$get()
    tryCatch(
        expr = { i <- solve(data) },
        error = function(e){ 
            message("Error encountered during matrix inversion.")
            print(e)
        }
    )
    x$setinverse(i)
    i
}
