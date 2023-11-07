surveyors <-
  function(poly,usedet=FALSE)
  {
    ##If usedet == T, skip the shortcuts. This is only good for
    ##testing, etc.
    if(!usedet)
      {
        n <- nrow(poly)
        if(n<3)  return(0)
        if(n==3) return(surveyors.3(poly))
        if(n==4) return(surveyors.4(poly))
      }
    
    poly2 <- poly[c(2:nrow(poly),1),]
    polyall <- cbind(poly,poly2)
    deter <- apply(polyall,1,function(x){det(matrix(x,2))})
    abs(sum(deter/2))
  }

## This is probably 20x faster than the general surveyor's formula;
## using the 'shoelace' formula
surveyors.3 <-
  function(poly)
  {
    abs(poly[1,1]*poly[2,2]+ poly[2,1]*poly[3,2] + poly[3,1]*poly[1,2]
      - poly[2,1]*poly[1,2] -poly[3,1]*poly[2,2]-poly[1,1]*poly[3,2])/2
  }


## This is probably 20x faster than the general surveyor's formula;
## using the 'shoelace' formula
surveyors.4 <-
function(poly)
  {
    abs(poly[1,1]*poly[2,2]+ poly[2,1]*poly[3,2] +
     poly[3,1]* poly[4,2] + poly[4,1]*poly[1,2]
     - poly[2,1]*poly[1,2] -poly[3,1]*poly[2,2]-
     poly[4,1]*poly[3,2] - poly[1,1]*poly[4,2])/2
  }

shoelace <- function(poly)
{
    x <- poly[,1]
    y <- poly[,2]
    out <- .C('shoelace_c',as.double(x),
              as.double(y),
              length(x),
              ans=as.double(0))
    return(out$ans)
}


## Profiling test:
#  poly <- rbind(c(1.1,1.2),c(2.1,3.3),c(4.1,1.2))
#system.time(for(i in 1:50000)surveyors(poly,usedet=T))
#system.time(for(i in 1:50000)surveyors(poly))
#system.time(for(i in 1:50000)surveyors.3(poly))
#
#poly2 <- rbind(c(1.1,1.2),c(2.2,1.3),c(4.0,4.25),c(1.3,3.9))
#system.time(for(i in 1:50000)surveyors(poly2,usedet=T))
#system.time(for(i in 1:50000)surveyors(poly2))
#system.time(for(i in 1:50000)surveyors.4(poly2))

##This is heron's formula for area of a triangle based on the length of its sides.  This
##is useful for GPS coordinates, for whom we can more easily calculate sides the coordinates are not
## rectangular.
## Discussion of spherical triangle:
##http://math.stackexchange.com/questions/328114/geometry-of-spherical-triangle
##http://math.rice.edu/~pcmi/sphere/gos5.html
heron  <- function(a,b,c)
{
    p <- a+b+c
    sqrt(p*(p-a)*(p-b)*(p-c))
    
}

##this finds the area of a quadrilateral based on the length of the edges, and the sum of the opposite angles.

# For spherical coordinates:
#http://mathforum.org/library/drmath/view/63767.html
bretschneider <- function(a,b,c,d,theta)
{
}
