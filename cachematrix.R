## Put comments here that give an overall description of what your
## functions do

# This script creates two functions, makeCacheMatrix() and cacheSolve(). The first
# function (makeCacheMatrix()) creates an R object that stores a matrix and its 
# inverse. The other function (cacheSolve()) needs an argument that is returned
# by the first function in order to obtain the inverse from the cached value that
# is stored in the object's environment of the first function. 


## Cleaning the global environment and setting the working directory

# rm(list=ls())
# setwd("~/Documents/Documents/R-Statistics/Coursera/git/ProgrammingAssignment2")


## Write a short comment describing this function

# This function builds a set of functions and returns those within a list to the
# parent environments. Those are the set() and get() function as known from
# project oriented programming - both set and get the values, and the setsolve() 
# and getsolve() function, which set and get the inverse. Further, two data
# objects are created, x (the matrix) and i(the inverse function).

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

# This function is needed to poulate or obtain the inverse from an object
# of type of the first function. If no data is cached, the matrix from the input 
# object is used to calculate an inverse, using the getsolve() function on the 
# input object to set the inverse in the input object. The values are returned to
# the parent environment by printing the inverse object. If there is a valid
# inverse in the cache, this is returned instead.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getsolve()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}


## Calling the functions and testing the results

mymatrix <- makeCacheMatrix(x=matrix(1:4,2,2)) #pass on a simple 2:2 matrix
ls(mymatrix) #see environment

cacheSolve(mymatrix)  # calculate from argument
cacheSolve(mymatrix)  # call a second time to see if results are returned from
                      # cache.

solve(matrix(1:4,2,2)) # compare results with the same matrix in solve.



# Footnote: description based on this content:
# https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md
# (status 17.07.2017)
# Some wording has been adapted to fit the needs of the assignment.

