## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #create an empty variable
        #this function accept a matrix y and pass it to x
        set_matrix <- function(y) {
                x <<- y #x is now assigned by the current matrix value
                inv <<- NULL #no calculation on the inv so it's still empty
        }
        get_matrix <- function() x #single statement function, no curly brackets needed
        #set and get inverse
        set_inverse <- function(x){
                inv <<- solve(x)
        }
        get_inverse <- function() inv
        
        list(set_matrix = set_matrix, 
             get_matrix = get_matrix, 
             get_inverse = get_inverse, 
             set_inverse = set_inverse)
}

# Now make a test
mdat <- matrix(c(1,2,3,11),nrow = 2, ncol = 2)
aMatrix <- makeCacheMatrix(mdat)

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inverse()
        if(!is.null(inv)){
                print("Getting cached data.")
                return(inv)
        }else{
                data <- x$get_matrix()
                inv <- solve(data)
                inv
        }
}

# Test for the cacheSolve function
cacheSolve(aMatrix)

