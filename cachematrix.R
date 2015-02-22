#The makeCacheMatrix makes a list of functions that:
#1.  set the matrix
#2.  get the matrix
#3.  set the invers of the matrix, when the invers has been found
#4.  get the invers of the matrix, if the invers has been found

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL  # Makes a variabel and sets its to NULL, this variabel will be set to be the invers matrix by the next function.
        set <- function(y) {
                x <<- y       # Sets the matrix in the makeCacheMatrix function to be the matrix given in the set function.
               im <<- NULL    # Sets the m variabel to be NULL
        }
        get <- function() x   # Gets the matrix in the makeCacheMatrix
        set_invers <- function(solve) im <<- solve  #Sets the m variabel to be the variabel given to the set_invers function.
        get_invers <- function() im   #Gets the variabel m
        list(set = set, get = get,
             set_invers = set_invers,
             get_invers = get_invers)   #Makes the list that has the functions made in the makeCacheMatrix, which is the cache.

}


# Finds the invers of the matrix, by checking if it has already been calculated. If sit has, it gets the invers of the matrix from the cache.
#Otherwise, it calculates the invers of the matrix and stores the invers of the matrix in the cache via the set_invers function

cacheSolve <- function(x, ...) {
        im <- x$get_invers()   #Makes a variabel and sets it to the vaule of the get_invers from the previous function, this will be the invers of the matrix 
                                #if this has been calulated, and NULL if it hasn't.
        
        if(!is.null(im)) {     #Checks if the invers of the matrix has been calulated by checking if im is different from NULL
                message("getting cached data")   #If it has, the massage 'getting cached data' in the console
                return(im)             #and ends the function and returns the invers of the matrix
        }
        data <- x$get()      # If the invers hasn't been found a variabel 'data' is made and set to the matrix of the get from the previous function
        im <- solve(data, ...)  #Calulates the invers of the matrix called data and sets the variabel im to be this invers.
        x$set_invers(im)    #Sets the set_invers of the previouse function to be this invers.
        im                  #returns the invers og the matrix.
}
