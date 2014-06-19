#.............................................................................
# The following two functions are used to calculate and speed up the inverse 
# calculation of a series of square invertible matrices. This is done by 
# caching of the each matrix and its inverse into a special "cache matrix".

# Subsequent matrices are compared to the cache and if matched the stored 
# inverse is returned. If not found the current matrix is solved(), 
# added to the cache matrix with its inverse, then the inverse is returned.

#*****************************     makecacheMatrix.R     ******************** 

# makeCacheMatrix()returns a list of functions (set,get,getInv,setInv,getCache)
# to the encapsulating function that modify or retrieve data as specified to:
  
  # 1) its single function parameter of type matrix()
  # 2) its single variable that contains a special list acting as a cache 
#------------------------------------------------------------------------------

makeCacheMatrix <- function(myMatrix = matrix()) {

  # Empty list of two lists: (mat = matrices, inv = inverse of matrices)
  
    cacheInv <- list(mat=list(NULL),inv=list(NULL))
  
  #...........................................................................  
    get <- function() myMatrix	    # get current matrix to process
  #........................................................................... 
    set <- function(d)  myMatrix <<- d  # set next matrix to process
  #...........................................................................  
    setInv <- function(theMat,theInv){  # cache current matrix and its inverse
    
    ct <- length(cacheInv$mat)          # Gets number of current matrices
    
    # if null matrices then length = 1 = ct else add new matrix to end = ct +1

    if(!is.null(cacheInv$mat[[ct]])) ct <- ct + 1  
    
    cacheInv$mat[[ct]] <<-theMat        # Adds matrix and inverse to the cache
    cacheInv$inv[[ct]] <<-theInv
  } 
  #.............................................................................  
  getInv <- function(){                           # Retrieves cached Inverse 
    								  # if it exists
   numMatrices <-length(cacheInv$mat)  
  
   if ( is.null(cacheInv$mat[[1]])){              # Stop search - empty cache 
          
      return(FALSE)
    } 
        
    found <- FALSE                                # Look for matching matrix
   
    for(i in 1: numMatrices){

      if(identical(myMatrix,cacheInv$mat[[i]])){  # If a match send message
                                                  
        message("getting cached data")    
        return(cacheInv$inv[[i]])                 # return inverse
      }
             
    }# end for

      return(FALSE)                               # No match return False
 }
#.........................................................................   
  getCache <- function() cacheInv
#......................................................................... 

# Return function list to calling function 'cacheSolve

list(get=get,set=set, setInv = setInv, getInv=getInv, getCache=getCache)
}
#*****************************  end makecacheMatrix.R  ******************* 
#*************************************************************************  
  
#*****************************     cacheSolve.R       ******************** 
# Receives a list of function parameters from makecacheMatrix()that allow 
# matrices to be cached and received. 

# This function takes a list of square invertible matrices, then solve(X) 
# and returns their inverses.

#-        !!!!Warning: Matrices must be square invertible matrices!!!!

#-------------------------------------------------------------------------

cacheSolve <- function (makeCacheMatrix,...){
 
  myInv <- makeCacheMatrix()          # Assign function param to a variable

  for(i in 1:length(mats))            # Input list of matrices to process
  {						  # mat = list( matrices )	
   myInv$set(mats[[i]])               # Set matrix value
   invm <- myInv$getInv()             # Get Inverse if available
  
   if(class(invm) == 'logical'){      # Not available - FALSE returned
    
   	data <- myInv$get()             # Get matrix to process
    
  	dataInv <- solve(data)          # Get Inverse of matrix
   
  	myInv$setInv(data,dataInv)      # Add matrix and inverse to the cache

	print(dataInv)                   
   } 
  	else {print(invm)}              # myInv <- makeCacheMatrix()  
  						  # returned the inverse
  }# end for
  
   # print(myInv$getCache())   # If necessary print out entire cache for analysis
  
}
#*****************************    end cacheSolve.R     ******************** 
