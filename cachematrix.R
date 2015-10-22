
makeCacheMatrix <- function(x = matrix()) {
        
        # define m as empty object
        m <- NULL
        
        # set func is used to assign parameter input valure(y) into cache
        # then reset the m as empty object
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # get func is used to get x matrix which is the origianl 
        # matrix you input
        get <- function() {
                x
        }
        
        # setInv func is used to set Invert Matrix into cache
        setInv <- function(inv) {
                m <<- inv
        }
        
        # getInv func is sued to get Invert Matrix from cache if existed
        getInv <- function() {
                m
        }
        
        # return List of funcions
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Using getInv func to search(get) Invert Matrix in cache
        m <- x$getInv()
        
        # If Invert Matrix was found, then showing message and return the Invert Matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # If Invert Matrix was NOT found
        # Using get func to get orignal input Matrix as data object
        data <- x$get()
        
        # Calculate Invert Matrix by using funcion solve ans assing to m object
        m <- solve(data, ...)
        
        # Set m(Invert Matrix) object into cache by using setInv func
        x$setInv(m)
        
        # Then return Invert Matrix which calculted in first time 
        m
}
