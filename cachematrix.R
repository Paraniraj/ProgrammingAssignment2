## Programming Assignment : 2
## Programming Assignment : Cache the inverse of matrix which would benefit to avoid repetative matrix inverse computation(same data)

## Function Name          : makeCacheMatrix
## Function Arguments     : Input : Matrix; Output : List with four functions
## Function Description   : Function creates a list contaiting set/get the value and inverse of matrix

makeCacheMatrix <- function(x = matrix()) 
{
	m <- NULL 
	set <- function(y) 
		{
			x <<- y
			m <<- NULL
		}
	
	get <- function()x
		
	setInverse <- function(inverse) m <<- inverse
	getInverse <- function() m
	
	list(set=set,get=get,setInverse = setInverse,getInverse = getInverse)


}

## Function Name          :  cacheSolve
## Function Arguments     :  Input : makeCacheMatrix output; Output : Inverse of Matrix
## Function Description   :  Compute inverse of the matrix created by makeCacheMatrix. If the inverse has already been calculated, then use the inverse from cache. 

cacheSolve <- function(x, ...) 
{
    m<-x$getInverse()
	if(!is.null(m))
	{
		message("Getting Cached Data")
		return(m)
	}
	
	data<-x$get()
	m<-solve(data)
	x$setInverse(m)
	m
		
}

## Sample Execution output 

## myMatrix<-matrix(c(1,2,3,4),2,2)
## myCacheMatrix<-makeCacheMatrix(myMatrix)
## myCacheMatrix$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## myCacheMatrix$getInverse()
## NULL
## cacheSolve(myCacheMatrix)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## cacheSolve(myCacheMatrix)
## Getting Cached Data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5