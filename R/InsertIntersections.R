
## This function has two purposes, and it works by inserting points along
## the path that are useful for comparing it to another path.  First, it 
## inserts point at any location where a segment of A crosses a segment of B,
## so that there are no crossings.  Second (and optionally), it inserts points
## at the closest point on one path whenever a point is 'opposite' a segment--
## when the shortest line from a point to a segment forms a right angle with
## the segment.  This is useful for computing a mapping that maps monotonically
## from one path to another at all points (and does not 'back up' on any
## individual segment)

InsertIntersections <-
function(path1,path2,insertopposites=T,verbose=F)
{

    ##this will first look at each pair of segments and determine whether they intersect.
    ## If so, it will
    ##add a middle-point at the appropriate locations of each path.
    
    
    path1 <- as.matrix(path1)
    path2 <- as.matrix(path2)

  key1 <- rep(0,nrow(path1))
  key2 <- rep(0,nrow(path2))
  
    addtopath1 <- rbind(c())
    addtopath2 <- rbind(c())
    keep1 <- rbind(c())
    keep2 <- rbind(c())
    
  if(verbose)print("Finding path intersections")
   
  for(i in 2:nrow(path1))
    {
      a1 <- path1[i-1,]
      a2 <- path1[i,]

      for(j in 2:nrow(path2))
        {
          b1 <- path2[j-1,]
          b2 <- path2[j,]

          intr <- linesIntersect(a1,a2,b1,b2)
          if(!is.na(intr[1]&intr[2]))
            {
                addtopath1  <- rbind(addtopath1,c(i-.5,intr))
                addtopath2  <- rbind(addtopath2,c(j-.5,intr))
                keep1 <- rbind(keep1,c(-1,intr))
                keep2 <- rbind(keep2,c(-1,intr))
                
                print(paste("Intersection found at ",intr[1],intr[2]))
            }
      }
  }



    ##now,, do the new paths. Add the original paths to the inserted ones
    ##then re-sort by the faux order.
    path1b <- rbind(cbind(1:nrow(path1),path1),addtopath1)
    path2b <- rbind(cbind(1:nrow(path2),path2),addtopath2)
    sorted1 <- path1b[order(path1b[,1]),]
    sorted2 <- path2b[order(path2b[,1]),]

    keep1b <- rbind(cbind(1:nrow(path1),path1),keep1)
    keep2b <- rbind(cbind(1:nrow(path2),path2),keep2)
    keepsorted1 <- keep1b[order(path1b[,1]),]
    keepsorted2 <- keep2b[order(path2b[,1]),]


    xy1 <- sorted1[,2:3]
    xy2 <- sorted2[,2:3]

    ##create keys for getting back the original points on the path:
    key1 <- cbind(keepsorted1[,1],1:nrow(sorted1))
    key2 <- cbind(keepsorted2[,1],1:nrow(sorted2))

  
    l1 <- nrow(xy1)
    l2 <- nrow(xy2)
    
  l1.b <- 2*l1-1 ##size of bigger cost matrix
  l2.b <- 2*l2-1 ##size of bigger cost matrix


  ##create keys to allow accessing the correct node of the matrix
  ##from the list element:
  l1keya <-c(rep(1:(l1-1),each=2),l1)
  l1keyb <-c(1,rep(2:(l1),each=2))
  
  l2keya <-c(rep(1:(l2-1),each=2),l2)
  l2keyb <-c(1,rep(2:(l2),each=2))

  if(verbose)
    {
      print("Interim paths:")
      print(xy1)
      print(xy2)
    }


  
  if(!insertopposites)
      {

          list(newpath1=xy1,
               newpath2=xy2,
               key1=key1,
               key2=key2)
          
      }else{
          addtopath1.2 <- rbind(c())
          addtopath2.2 <- rbind(c())
          
          ##Now, insert points where anything is 'opposite'
          
          for(i in 1:l1.b)
              {
                  for(j in 1:l2.b)
                      {
                          if(odd(i) & even(j))
                              {
                                  
                                  intprop <- IntersectPoint(unlist(xy2[l2keya[j],]),
                                                            unlist(xy2[l2keyb[j],]),
                                                            unlist(xy1[l1keya[i],]))
                                  
                                  
                                  if(intprop>0 & intprop<1)
                                  {

                                      ##this code results in a warning:
                                      ##     Warning in (xy2[l2keyb[j], ] - xy2[l2keya[j], ]) * intprop :
                                      ##  Recycling array of length 1 in vector-array arithmetic is deprecated.
                                      ##  Use c() or as.vector() instead.
                                          newpoint <- xy2[l2keya[j],] +
                                              (xy2[l2keyb[j],]-xy2[l2keya[j],])*intprop
                                          
                                          
                                          addtopath2.2 <- rbind(addtopath2.2,c(j/2+intprop,newpoint))
                                      }
                              }
                          
                          if(even(i) & odd(j))
                              {
                                  
                                  intprop <- IntersectPoint(unlist(xy1[l1keya[i],]),
                                                            unlist(xy1[l1keyb[i],]),
                                                            unlist(xy2[l2keya[j],]))
                                  
                                  if(intprop>0  & intprop<1)
                                  {

                                      ##this code results in a warning:
                                      ##     Warning in (xy2[l2keyb[j], ] - xy2[l2keya[j], ]) * intprop :
                                      ##  Recycling array of length 1 in vector-array arithmetic is deprecated.
                                      ##  Use c() or as.vector() instead.
                                      
                                          newpoint <- xy1[l1keya[i],] +
                                              (xy1[l1keyb[i],]-xy1[l1keya[i],])*intprop

                                      
                                          addtopath1.2 <- rbind(addtopath1.2,c(i/2+intprop,newpoint))
                                      }
                              }
                      }
              }
          
          
          if(verbose)
              {
                  print("Additional points:")
                  print(addtopath1.2)
                  print(addtopath2.2)

              }
          ##Now, add the implied 'opposite' points to the mix:
          ##now,, do the new paths.
          ##these are the matrices to add based on new inferred points.
          ##add1 is point, point, i.proportion for neworder, -1
          if(length(addtopath1.2)>0)
              {
                  add1 <- cbind(addtopath1.2,rep(-1,nrow(addtopath1.2)))[,c(2,3,4,1)]
                  old1 <- cbind(xy1,key1)
                  path1d <- rbind(cbind(old1),add1)
              } else{
                  path1d <- cbind(xy1,key1)
              }

          
          if(length(addtopath2.2)>0)
              {
                  add2 <- cbind(addtopath2.2,rep(-1,nrow(addtopath2.2)))[,c(2,3,4,1)]
                  old2 <- cbind(xy2,key2)
                  path2d <- rbind(cbind(old2),add2)
              } else{
                  path2d <- cbind(xy2,key2)
              }
          
          ##xy1 is the original points with inferred crossovers.
          ##key is the original key.


          sorted1b <- path1d[order(path1d[,4]),]
          sorted2b <- path2d[order(path2d[,4]),]


          xy1b <- sorted1b[,1:2]
          xy2b <- sorted2b[,1:2]


          list(newpath1=xy1b,
               newpath2=xy2b,
               key1=sorted1b[,3],
               key2=sorted2b[,3])
      }
}
