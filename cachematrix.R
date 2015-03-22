makeCacheMatrix <- function(x = matrix()) {
        i <- matrix(nrow=nrow(x), ncol=ncol(x))			#creating matrix filled with 'NA'
        set <- function(y) {						#defining function set()
                x <<- y							#making x in this function environment equal to input of set()
                i <<- matrix(nrow=nrow(x), ncol=ncol(x))	#filling i matrix in this function envoronment with 'NA'
        }
        get <- function() x
        setinv <- function(inv) i <<- inv				#make i from makeCacheMatrix() equal to inverse matrix when calculated
        getinv <- function() i
        list(set = set, get = get,					#this list is makeCacheMatrix() output
             setinv = setinv,
             getinv = getinv)
}

cacheSolve <- function(x, ...) {

	i <- x$getinv()							#getting inverse matrix from x environment
	if(!identical(i,matrix(nrow=nrow(i), ncol=ncol(i)))) {
		message("getting cached data")			#if it's not filled with 'NA', return i matrix
		return(i)
	}
	data <- x$get()							#otherwise, get initial matrix
	i <- solve(data, ...)						#calclate the inverse matrix
	x$setinv(i)								#and put it into cache
	i
}
