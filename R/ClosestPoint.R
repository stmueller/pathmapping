ClosestPoint <-
function(px, py, x1, y1, x2, y2) {

  ##if the two points are the same, just return one of them:
  if(x1==x2 & y1==y2)
    {
      return( c(x1,y1))
    }
  ## px,py is the point to test.
  ## x1,y1,x2,y2 is the line to check distance.
  ##
  ## Returns distance from the line, or if the intersecting point on the line nearest
  ## the point tested is outside the endpoints of the line, the distance to the
  ## nearest endpoint.
  ##
  ## Returns 9999 on 0 denominator conditions.
  ans <- NULL
  ix <-0
  iy <- 0   # intersecting point
  lineMag <- LineMagnitude(x1, y1, x2, y2)
  
  
  if( lineMag < 0.00000001) {
    ##warning("short or zero-length segment; returning first point")
    return(c(x1,y1))
  }
  u <- (((px - x1) * (x2 - x1)) + ((py - y1) * (y2 - y1)))
  u <- u / (lineMag * lineMag)



#  if(u<.001) u <- .001
#  if(u>.999) u <- .999
    
  ##original code for using endpoint.
  if((u < 0.00001) || (u > .99999)) {
    ## closest point does not fall within the line segment, take the shorter distance
    ix <- ix#10000000
    iy <- iy#10000000

    ## to an endpoint
    if(1)
      {
        l1  <- LineMagnitude(px, py, x1, y1)
        l2  <- LineMagnitude(px, py, x2, y2)
        
        if(l1 < l2)
          {
            ix <- x1
            iy <- y1
          }
        
        else
          {
            ix <- x2
            iy <- y2
          }
        
      }
    
  } else {
    ## Intersecting point is on the line, use the formula
    ix <- x1 + u * (x2 - x1)
    iy <- y1 + u * (y2 - y1)
    
  }
  c(ix,iy)
}
