# The makeCacheMatrix is a function that possesses
#two variables: the x variable, that is the input
#matrix and the m variable. The output is a list
#with 4 elements, corresponding to the 4 child-functions
#created.
#The m variable is set as NULL when makeCacheMatrix
#runs for the first time.
#When the functions set, get, setsolve and getsolve are
#called (by the cacheSolve function), 
#the setsolve function gets the input value
#of x, calculates the inverse of the matrix with the
#solve function, and then output this result to the
#getsolve element of the list 

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


#When the cacheSolve function runs for the first time,
#it will call the setsolve function and will provide the 
#value of the inverse of the matrix. It will also
#create the getsolve elements of the list previously 
#mentioned.
#If this is not the first time that cacheSolve is called
#for a specific input matrix, this will get the inverse of this 
#matrix previously calculated and cached in getsolve element
#of the list defined in makeCacheMatrix.    

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m

}
