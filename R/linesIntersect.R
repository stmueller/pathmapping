linesIntersect <-
function(A1,A2,B1,B2)
  {
    x1 <- A1[1]
    x2 <- A2[1]
    x3 <- B1[1]
    x4 <- B2[1]
    y1 <- A1[2]
    y2 <- A2[2]
    y3 <- B1[2]
    y4 <- B2[2]
      
    newx <- ((x1*y2-y1*x2)*(x3-x4) - (x1-x2)*(x3*y4-y3*x4))/
      ((x1-x2)*(y3-y4)-(y1-y2)*(x3-x4))

    newy <- ((x1*y2-y1*x2)*(y3-y4)-(y1-y2)*(x3*y4-y3*x4))/((x1-x2)*(y3-y4)-(y1-y2)*(x3-x4))

    ##Is on both line segments?

    newxy <- c(newx,newy)

    prop1 <-  ((A2-A1) %*% (newxy-A1)) / ((A2-A1) %*% (A2-A1))
    prop2 <-  ((B2-B1) %*% (newxy-B1)) / ((B2-B1) %*% (B2-B1))

    if(is.na(prop1+prop2))
      {
        return (c(NA,NA))
      }
    if(prop1>0 & prop1<1 & prop2>0 & prop2<1)
      {
        return( c(newx,newy))
      }else{
        return (c(NA,NA))
      }
    
  }
