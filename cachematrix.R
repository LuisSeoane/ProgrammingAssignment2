##There are two functions makeCacheMatrix, cacheSolve

## library MASS is used to calculate  inverse of nonsquared as well as square matrices
library(MASS)

##makeCacheMatrix --< This function creates a special "matrix" object
##that can cache its inverse. Composed of set, get, setinv and getinv
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #initializing inverse as null
        set <- function (y) {
                                x <<- y 
                                inv <<- NULL
                            }
        get<- function()x #funtion to get matrix x
        setinv <- function(inverse)inv<<-inverse
        getinv <- function(){
                            inver<-ginv(x) #funtion to get inverse of the matrix
                             }
        list(set = set, get = get, 
             setinv = setinv, getinv = getinv)
}

## cacheSolve is used to get the cache data

cacheSolve <- function(x, ...) { ##gets cache data
        
        inv<- x$getinv()
        if(!is.null(inv)){ ## checking wether inverse is null
                         message("getting cached data!")
                         return(inv) ##return inverse value
        }
        data <-x$get()
        inv<-solve(data,...) #calculate inverse value 
        x$setinv(inv)
        inv  ##return matrix that is the inverse of x
}
