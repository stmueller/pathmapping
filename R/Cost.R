

## This cost function computes the 'cost'  of the polygon
## typically somehow related to the area.
##

## This is the default cost function.  
## alternateves are Cost.area
## Cost.areasquared,
## Cost.weightedbydist
## Cost.rectangular
## Cost.quadratic
## Only Cost.area and Cost.quadratic are insensitive to segmentation.
##
Cost <- function(xy1,xy2,i,j,pi,pj,opposite,costfn=Cost.area)
{
  costfn(xy1,xy2,i,j,pi,pj,opposite)
}


Cost_Area <- 
function(xy1,xy2,i,j,pi,pj,opposite)
{

    out <- .Call('Cost_Area_c',
                 xy1,
                 xy2,
                 as.integer(i),as.integer(j),
                 as.integer(pi),as.integer(pj),
                 (opposite));

    return(out)
}



Cost.area <-
function(xy1,xy2,i,j,pi,pj,opposite)
{

    #    print("r-code calling Cost.area");
    #print(paste(i,j,pi,pj))
    
  ## The cost of any unconnected transition is infinite.
  if(pi<=0 | pj<=0 | !connected(pi,pj,i,j))#
    {
      return (Inf)
    }
  
  if(odd(i) & odd(j))
    {

      l1key <- (i+1)/2 ## Guesses for what these are; 3==5
      l2key <- (j+1)/2 ##



      ##Let's consider first the mapping from pi=i-1; j=j
      if(pi==(i-1)&pj==j)
        {
          ##opp is really if the x1 segment is opposite the x2 point
          opp <- opposite[i-1,j]
          ## We are doing a triangle, connecting a segment of i to a point on j.
          ## The cost is determined by the relative position (recorded in opp)
          ## of i to segment on j.  

          vecstart <- unlist(xy1[l1key-1,])
          veccurr <-  unlist(xy1[l1key,])
          point <- unlist(xy2[l2key,])  ##Point on j series
          vecdelt <- veccurr-vecstart
          vecstart1 <- vecstart + opp * vecdelt

            #print(c(vecstart1,veccurr,point,opp))
            
          cost <- surveyors.3(rbind(vecstart1,
                                  veccurr,
                                  point)) 
            #print(paste("PATH A",cost))
        }else if(pi==i & pj == (j-1))
          {
            ##ditto above, but with j-(j-1) segment and point i

            opp <- opposite[i,j-1]
            vecstart <- unlist(xy2[l2key-1,])
              veccurr <-  unlist(xy2[l2key,])

              
            point <- unlist(xy1[l1key,])  ##Point on j series


            vecdelt <- veccurr-vecstart
            vecstart1 <- vecstart + opp * vecdelt

            #print(c(vecstart1,veccurr,point,opp))
            cost <- surveyors.3(rbind(vecstart1,
                                    veccurr,
                                    point)) 
            #print(paste("PATH B",cost))

              
          }else if((pi==(i-2)) &(pj ==(j-2)) )
            {

              vec1start <- unlist(xy1[l1key-1,])
              vec1curr <-  unlist(xy1[l1key,])

              vec2start <- unlist(xy2[l2key-1,])
              vec2curr <-  unlist(xy2[l2key,])

              ## Because this is a 4-sider, there might
              ## be a crossover, so see if the different-order
              ## points are larger.  If so, we have a crossover
              ## and the area is half the full quadrilateral.
              ## if cost2 is smaller, then cost is cost1. else cost2/2
              ## this uses the 'shoelace' formula aka surveyors 
              cost1 <- surveyors.4(rbind(vec1start,
                                      vec1curr,
                                      vec2curr,
                                      vec2start)) 

              cost2 <- surveyors.4(rbind(vec1start,
                                       vec1curr,
                                       vec2start,
                                       vec2curr
                                       )) 

              if(cost2>cost1)
                {
                  ##uh-oh.  The polygon is 'crossed'
                  ##cost should be probably cost2, but it depends
                  ##on how they cross.  
                  cost <- cost2

                  
                }else{
                  cost <- cost1
                }
                #print(paste("PATH C",cost))
            }
          else{
            ##otherwise, the cost is infinite--no legal direct path.
              cost <- Inf
              #print(paste("PATH D",cost))
          }
      
    }else if(even(i) & odd(j))
      {

        ##the current mapping is between an edge of i and a point on j.
        ##the area of this segment depends on whether j is opposite i.
        
        ## we could have gotten here from two directions:
        ## directly up (i-1), which is the first point on i;
        ## or  two to the left (j-2), which is the next 'pie' segment


        l1keya <- i/2   ## i is even; grid node i=4 is the connection xy=2-3
        l1keyb <- i/2+1 ## i is even; grid node i=4 is the connection xy=2-3

        l2key <- (j+1)/2 ## 5=3, e.g.

        if(pi == (i-1) & j==pj)
          {
            ##This is finding the cost of moving from a point-point mapping to
            ##a point-segment mapping, along i.  The cost is the area of the triangle,
            ##regardless of the 'opposite' status (hopefully)
            opp <- opposite[i,j]
            vecstart <- unlist(xy1[l1keya,])
            vecdelta <- unlist(xy1[l1keyb,])-vecstart
            newend <- vecstart + vecdelta *opp
      
            #print(c(vecstart,newend,unlist(xy2[l2key,]),opp))
            cost <- surveyors.3(rbind(vecstart,
                                    newend,
                                    unlist(xy2[l2key,])))
              
             # print(paste("PATH E",cost))
              
          }else if(pi == i & (j-2)==(pj))
            {
              ##i is even, so the i path is a segment:
              ##j is odd, so it is just a single point
              ##jump from (i,j-2)
              ##
              vecstart <- unlist(xy1[l1keya,])
              vecdelta <- unlist(xy1[l1keyb,])-vecstart
              point <- unlist(xy2[l2key,])
              lastpoint <- unlist(xy2[l2key-1,])

              opp <- opposite[i,j]
              
              ##in this case, the current tentative mapping should be the orthogonal line.
              ##connected to the point, rather than the whole segment (next point).
              ##Thus, we have a newpoint along l1ab.
              vecend <- vecstart + vecdelta * opp ##the proportional distance.

              
              lastopp <- opposite[i,j-2]

              if(lastopp>0)
                {

                  ##the previous mapping was 'opposite'.  This means
                  ##we need to find the two points in the i vector, and
                  ##the current and previous point on the j vector, and
                  ##compute the area of the polygon.
                  
                  vecprevend <- vecstart + vecdelta * lastopp
                  
                  ##Now, if the current point is not opposite, we need
                  ##to measure the following 3-gon:
                  
                  if(opp)
                  {
                      #print(c(vecprevend,vecend,point,lastpoint,opp))
                      cost <- surveyors.4(rbind(vecprevend,
                                              vecend,
                                              point,
                                              lastpoint))
                      #print(paste("Path F: " , cost))
                      ##this is a quadrilateral, but I don't think we need to
                      ##try both mappings and do the max.  Otherwise, crossovers
                      ##would be a problem.
                      

                                    
                    }else{
                      #print(c(vecprevend,vecend,point,lastpoint,opp))
                      cost <- surveyors.4(rbind(vecprevend,
                                              vecend,
                                              point,
                                              lastpoint))
                       # print(paste("Path G: " , cost))
                      
                    }
                
                }else{
                  ##if that previous point was not 'opposite', we shouldn't use this route;
                  
                  cost <- Inf
              #print(paste("PATH H: ",cost))
                }
            }else{
              cost <- Inf
              #print(paste("PATH I: ",cost))
            }
                                 
      
        
       }else if(odd(i) & even(j))
         {

           ##The cost here is symmetric to the even/odd one above.
           
           ##the current mapping is between an edge of i and a point on j.
           ##the area of this segment depends on whether j is opposite i.
           
           ## we could have gotten here from two directions:
           ## directly up (i-1), which is the first point on i;
           ## or  two to the left (j-2), which is the next 'pie' segment
           

           l2keya <- j/2 ## i is even; grid node i=4 is the connection xy=2-3
           l2keyb <- j/2+1 ## i is even; grid node i=4 is the connection xy=2-3

           l1key <- (i+1)/2 ## 5=3, e.g.


           if(pj == (j-1) & i==pi)
             {
               ##This is finding the cost of moving from a point-point mapping to
               ##a point-segment mapping, along j.  The cost is the area of the triangle,
               ##regardless of the 'opposite' status (hopefully)
               opp <- opposite[i,j]
               vecstart <- unlist(xy2[l2keya,])
               vecdelta <- unlist(xy2[l2keyb,])-vecstart
               newend <- vecstart + vecdelta *opp


               #print(c(vecstart,newend,unlist(xy1[l1key,]),opp))
               cost <- surveyors.3(rbind(vecstart,
                                       newend,
                                       unlist(xy1[l1key,])))

               # print(paste("PATH J",cost))
             }else if((pj == j) & ((i-2)==(pi)))
               {
                 ##j is even, so the j path is a segment:
                 ##i is odd, so it is just a single point
                 ##jump from (i-2,j)
                 ##

                 opp <- opposite[i,j]
                 
                 vecstart <- unlist(xy2[l2keya,])
                 vecdelta <- unlist(xy2[l2keyb,])-vecstart
                 point <- unlist(xy1[l1key,])
                 lastpoint <- unlist(xy1[l1key-1,])
                 
                 ##in this case, the current tentative mapping should be the orthogonal line.
                 ##connected to the point, rather than the whole segment (next point).
                 ##Thus, we have a newpoint along l1ab.
                 vecend <- vecstart + vecdelta * opp ##the proportional distance.
                 
                 lastopp <- opposite[i-2,j]
               
                 if(lastopp>0)
                   {

                     ##the previous mapping was 'opposite'.  This means
                     ##we need to find the two points in the i vector, and
                     ##the current and previous point on the j vector, and
                     ##compute the area of the polygon.
                     
                     vecprevend <- vecstart + vecdelta * lastopp

                  
                     ##Now, if the current point is not opposite, we need
                     ##to measure the following 3-gon:
                     
                     if(opp)
                       {
                      
                         cost <- surveyors.4(rbind(vecprevend,
                                                 vecend,
                                                 point,
                                                 lastpoint))
                         ##this is a quadrilateral, but I don't think we need to
                         ##try both mappings and do the max.  Otherwise, crossovers
                         ##would be a problem.
                        #print(paste("PATH K",cost))

                         
                       }else{


                         #print(c(vecprevend,vecend,point,lastpoint,opp))
                         cost <- surveyors.4(rbind(vecprevend,
                                                 vecend,
                                                 point,
                                                 lastpoint))

                          # print(paste("PATH L",cost))
                       }
                   }else {
                         cost <- Inf
                          #print(paste("PATH MM",cost))
                   }
               }else{
                 ##if that previous point was not 'opposite', we shouldn't use this route;
                 
                 cost <- Inf
                 #print(paste("PATH M",cost))
               }
                       

           

         } else {
             cost <- Inf
            #print(paste("PATH N",cost))
         }

  return(cost)
  
}


## Use cost^2 to create convex loss function.
##
##
Cost.areasquared <-
function(xy1,xy2,i,j,pi,pj,opposite)
  {
    Cost.area(xy1,xy2,i,j,pi,pj,opposite)^2
  }

## This cost function uses the same area-based logic, but
## treats each triangle as 1/2 segment * distance, and each 
## quadrilateral as segment * distance.

Cost.weightedbydist <-
function(xy1,xy2,i,j,pi,pj,opposite)
  {

    ##Compute distance between edges/nodes
    d1 <- LinkCost(xy1,xy2,i,j)
    d2 <- LinkCost(xy1,xy2,pi,pj)
    
    ##compute area
    area <- Cost.area(xy1,xy2,i,j,pi,pj,opposite)

    area * (d1+d2)/2
  }



## This computes the cost of the rectangle having sides
## equal to the average segment distance, and the average distance
## between segments.

Cost.rectangular<-
function(xy1,xy2,i,j,pi,pj,opposite)
{

  ## The cost of any unconnected transition is infinite.
  if(pi<=0 | pj<=0 | !connected(pi,pj,i,j))#
    {
      return (Inf)
    }
  
  if(odd(i) & odd(j))
    {

      l1key <- (i+1)/2 ## Guesses for what these are; 3==5
      l2key <- (j+1)/2 ##



      ##Let's consider first the mapping from pi=i-1; j=j
      if(pi==(i-1)&pj==j)
        {
          ##opp is really if the x1 segment is opposite the x2 point
          opp <- opposite[i-1,j]
          ## We are doing a triangle, connecting a segment of i to a point on j.
          ## The cost is determined by the relative position (recorded in opp)
          ## of i to segment on j.  

          vecstart <- unlist(xy1[l1key-1,])
          veccurr <-  unlist(xy1[l1key,])
          point <- unlist(xy2[l2key,])  ##Point on j series
          vecdelt <- veccurr-vecstart
          vecstart1 <- vecstart + opp * vecdelt

          
#          cost <- surveyors.3(rbind(vecstart1,
#                                  veccurr,
#                                  point)) 
          w <- ldist(vecstart1,veccurr)/2
          h <- (ldist(vecstart1,point)+ldist(veccurr,point))/2
          cost <- w*h
          
        }else if(pi==i & pj == (j-1))
          {
            ##ditto above, but with j-(j-1) segment and point i

            opp <- opposite[i,j-1]
            vecstart <- unlist(xy2[l2key-1,])
            veccurr <-  unlist(xy2[l2key,])
            point <- unlist(xy1[l1key,])  ##Point on j series
            vecdelt <- veccurr-vecstart
            vecstart1 <- vecstart + opp * vecdelt
#            cost <- surveyors.3(rbind(vecstart1,
#                                    veccurr,
#                                    point)) 

            w <- ldist(vecstart1,veccurr)/2
            h <- (ldist(vecstart1,point)+ldist(veccurr,point))/2
            cost <- w*h

            
          }else if((pi==(i-2)) &(pj ==(j-2)) )
            {

              vec1start <- unlist(xy1[l1key-1,])
              vec1curr <-  unlist(xy1[l1key,])

              vec2start <- unlist(xy2[l2key-1,])
              vec2curr <-  unlist(xy2[l2key,])

              ## Because this is a 4-sider, there might
              ## be a crossover, so see if the different-order
              ## points are larger.  If so, we have a crossover
              ## and the area is half the full quadrilateral.
              ## if cost2 is smaller, then cost is cost1. else cost2/2

              w <- (ldist(vec1start,vec1curr) + ldist(vec2start,vec2curr))/2
              h <- (ldist(vec1start,vec2start)+ldist(vec1curr,vec2curr))/2
              cost <- w*h

            }
          else{
            ##otherwise, the cost is infinite--no legal direct path.
            cost <- Inf
          }
      
    }else if(even(i) & odd(j))
      {


        ##the current mapping is between an edge of i and a point on j.
        ##the area of this segment depends on whether j is opposite i.
        
        ## we could have gotten here from two directions:
        ## directly up (i-1), which is the first point on i;
        ## or  two to the left (j-2), which is the next 'pie' segment


        l1keya <- i/2   ## i is even; grid node i=4 is the connection xy=2-3
        l1keyb <- i/2+1 ## i is even; grid node i=4 is the connection xy=2-3

        l2key <- (j+1)/2 ## 5=3, e.g.

        if(pi == (i-1) & j==pj)
          {
            ##This is finding the cost of moving from a point-point mapping to
            ##a point-segment mapping, along i.  The cost is the area of the triangle,
            ##regardless of the 'opposite' status (hopefully)
            opp <- opposite[i,j]
            vecstart <- unlist(xy1[l1keya,])
            vecdelta <- unlist(xy1[l1keyb,])-vecstart
            newend <- vecstart + vecdelta *opp

            point <-  unlist(xy2[l2key,])

            w <-ldist(vecstart,newend)/2
            h <- (ldist(vecstart,point)+ldist(newend,point))/2

            cost <- w*h

                     
          }else if(pi == i & (j-2)==(pj))
            {
              ##i is even, so the i path is a segment:
              ##j is odd, so it is just a single point
              ##jump from (i,j-2)
              ##
              vecstart <- unlist(xy1[l1keya,])
              vecdelta <- unlist(xy1[l1keyb,])-vecstart
              point <- unlist(xy2[l2key,])
              lastpoint <- unlist(xy2[l2key-1,])

              opp <- opposite[i,j]
              
              ##in this case, the current tentative mapping should be the orthogonal line.
              ##connected to the point, rather than the whole segment (next point).
              ##Thus, we have a newpoint along l1ab.
              vecend <- vecstart + vecdelta * opp ##the proportional distance.

              
              lastopp <- opposite[i,j-2]

              if(lastopp>0)
                {

                  ##the previous mapping was 'opposite'.  This means
                  ##we need to find the two points in the i vector, and
                  ##the current and previous point on the j vector, and
                  ##compute the area of the polygon.
                  
                  vecprevend <- vecstart + vecdelta * lastopp
                  
                    
                  w <- (ldist(vecstart,vecprevend) + ldist(point,lastpoint))/2
                  h <- (ldist(vecprevend, lastpoint)+ldist(vecend,point))/2
                  cost <- w*h
                                    
         
                
                }else{
                  ##if that previous point was not 'opposite', we shouldn't use this route;
                  
                  cost <- Inf

                }
            }else{
              cost <- Inf
            }
                                 
      
        
       }else if(odd(i) & even(j))
         {

           ##The cost here is symmetric to the even/odd one above.
           
           ##the current mapping is between an edge of i and a point on j.
           ##the area of this segment depends on whether j is opposite i.
           
           ## we could have gotten here from two directions:
           ## directly up (i-1), which is the first point on i;
           ## or  two to the left (j-2), which is the next 'pie' segment
           

           l2keya <- j/2 ## i is even; grid node i=4 is the connection xy=2-3
           l2keyb <- j/2+1 ## i is even; grid node i=4 is the connection xy=2-3

           l1key <- (i+1)/2 ## 5=3, e.g.


           if(pj == (j-1) & i==pi)
             {
               ##This is finding the cost of moving from a point-point mapping to
               ##a point-segment mapping, along j.  The cost is the area of the triangle,
               ##regardless of the 'opposite' status (hopefully)
               opp <- opposite[i,j]
               vecstart <- unlist(xy2[l2keya,])
               vecdelta <- unlist(xy2[l2keyb,])-vecstart
               newend <- vecstart + vecdelta *opp

               point <- unlist(xy1[l1key,])
               w <- ldist(vecstart,newend)/2
               h <- (ldist(vecstart,point)+ldist(newend,point))/2
               cost <- w*h


             }else if(pj == j & (i-2)==(pi))
               {
                 ##j is even, so the j path is a segment:
                 ##i is odd, so it is just a single point
                 ##jump from (i-2,j)
                 ##

                 opp <- opposite[i,j]
                 
                 vecstart <- unlist(xy2[l2keya,])
                 vecdelta <- unlist(xy2[l2keyb,])-vecstart
                 point <- unlist(xy1[l1key,])
                 lastpoint <- unlist(xy1[l1key-1,])
                 
                 ##in this case, the current tentative mapping should be the orthogonal line.
                 ##connected to the point, rather than the whole segment (next point).
                 ##Thus, we have a newpoint along l1ab.
                 vecend <- vecstart + vecdelta * opp ##the proportional distance.
                 
                 lastopp <- opposite[i-2,j]
               
                 if(lastopp>0)
                   {

                     ##the previous mapping was 'opposite'.  This means
                     ##we need to find the two points in the i vector, and
                     ##the current and previous point on the j vector, and
                     ##compute the area of the polygon.
                     
                     vecprevend <- vecstart + vecdelta * lastopp

                     w <- (ldist(vecstart,vecprevend) + ldist(point,lastpoint))/2
                     h <- (ldist(vecprevend, lastpoint)+ldist(vecend,point))/2
                     cost <- w*h

                     ##Now, if the current point is not opposite, we need
                     ##to measure the following 3-gon:

                   }else {
                         cost <- Inf
                    
                   }
               }else{
                 ##if that previous point was not 'opposite', we shouldn't use this route;
                 
                 cost <- Inf
                 
               }
                       

           

         } else {
           cost <- Inf
         }

  return(cost)
}



## This uses a squared-error loss cost function; the squared distance between two lines,
## centered on the midline, integrated over their lengths.
##
##
Cost.quadratic<- function(xy1,xy2,i,j,pi,pj,opposite)
{

  ## The cost of any unconnected transition is infinite.
  if(pi<=0 | pj<=0 | !connected(pi,pj,i,j))#
    {
      return (Inf)
    }
  
  if(odd(i) & odd(j))
    {

      l1key <- (i+1)/2 ##  ##If they are both odd, use these keys to access values
      l2key <- (j+1)/2 ##



      ##Let's consider first the mapping from pi=i-1; j=j
      if(pi==(i-1)&pj==j)
        {
          ##opp is really if the x1 segment is opposite the x2 point
          opp <- opposite[i-1,j]

          
          ## We are doing a triangle, connecting a segment of i to a point on j.
          ## The cost is determined by the relative position (recorded in opp)
          ## of i to segment on j.  

          vecstart <- unlist(xy1[l1key-1,])
          veccurr <-  unlist(xy1[l1key,])
          point <- unlist(xy2[l2key,])  ##Point on j series
          vecdelt <- veccurr-vecstart
          vecstart1 <- vecstart + opp * vecdelt

          
#          cost <- surveyors.3(rbind(vecstart1,
#                                  veccurr,
#                                  point)) 

#          w <- ldist(vecstart1,veccurr)/2
#          h <- (ldist(vecstart1,point)+ldist(veccurr,point))/2
#          cost <- w*h

          cost <- squarederror(vecstart1,veccurr,point,point)
        }else if(pi==i & pj == (j-1))
          {
            ##ditto above, but with j-(j-1) segment and point i

            opp <- opposite[i,j-1]
            vecstart <- unlist(xy2[l2key-1,])
            veccurr <-  unlist(xy2[l2key,])
            point <- unlist(xy1[l1key,])  ##Point on j series
            vecdelt <- veccurr-vecstart
            vecstart1 <- vecstart + opp * vecdelt

            cost <- squarederror(vecstart1,veccurr,point,point)
            
          }else if((pi==(i-2)) &(pj ==(j-2)) )
            {

              vec1start <- unlist(xy1[l1key-1,])
              vec1curr <-  unlist(xy1[l1key,])

              vec2start <- unlist(xy2[l2key-1,])
              vec2curr <-  unlist(xy2[l2key,])

              cost <- squarederror(vec1start,vec1curr,vec2curr,vec2start)

            }
          else{
            ##otherwise, the cost is infinite--no legal direct path.
            cost <- Inf
          }
      
    }else if(even(i) & odd(j))
      {


        ##the current mapping is between an edge of i and a point on j.
        ##the area of this segment depends on whether j is opposite i.
        
        ## we could have gotten here from two directions:
        ## directly up (i-1), which is the first point on i;
        ## or  two to the left (j-2), which is the next 'pie' segment


        l1keya <- i/2   ## i is even; grid node i=4 is the connection xy=2-3
        l1keyb <- i/2+1 ## i is even; grid node i=4 is the connection xy=2-3

        l2key <- (j+1)/2 ## 5=3, e.g.

        if(pi == (i-1) & j==pj)
          {
            ##This is finding the cost of moving from a point-point mapping to
            ##a point-segment mapping, along i.  The cost is the area of the triangle,
            ##regardless of the 'opposite' status (hopefully)
            opp <- opposite[i,j]
            vecstart <- unlist(xy1[l1keya,])
            vecdelta <- unlist(xy1[l1keyb,])-vecstart
            newend <- vecstart + vecdelta *opp

            point <-  unlist(xy2[l2key,])

            cost <- squarederror(vecstart,newend,point,point)

                     
          }else if(pi == i & (j-2)==(pj))
            {
              ##i is even, so the i path is a segment:
              ##j is odd, so it is just a single point
              ##jump from (i,j-2)
              ##
              vecstart <- unlist(xy1[l1keya,])
              vecdelta <- unlist(xy1[l1keyb,])-vecstart
              point <- unlist(xy2[l2key,])
              lastpoint <- unlist(xy2[l2key-1,])

              opp <- opposite[i,j]
              
              ##in this case, the current tentative mapping should be the orthogonal line.
              ##connected to the point, rather than the whole segment (next point).
              ##Thus, we have a newpoint along l1ab.
              vecend <- vecstart + vecdelta * opp ##the proportional distance.

              
              lastopp <- opposite[i,j-2]

              if(lastopp>0)
                {

                  ##the previous mapping was 'opposite'.  This means
                  ##we need to find the two points in the i vector, and
                  ##the current and previous point on the j vector, and
                  ##compute the area of the polygon.
                  
                  vecprevend <- vecstart + vecdelta * lastopp
                  
                  cost <- squarederror(vecstart,vecprevend,point,lastpoint)
                    
                
                }else{
                  ##if that previous point was not 'opposite', we shouldn't use this route;
                  
                  cost <- Inf

                }
            }else{
              cost <- Inf
            }
                                 
      
        
       }else if(odd(i) & even(j))
         {

           ##The cost here is symmetric to the even/odd one above.
           
           ##the current mapping is between an edge of i and a point on j.
           ##the area of this segment depends on whether j is opposite i.
           
           ## we could have gotten here from two directions:
           ## directly up (i-1), which is the first point on i;
           ## or  two to the left (j-2), which is the next 'pie' segment
           

           l2keya <- j/2 ## i is even; grid node i=4 is the connection xy=2-3
           l2keyb <- j/2+1 ## i is even; grid node i=4 is the connection xy=2-3

           l1key <- (i+1)/2 ## 5=3, e.g.


           if(pj == (j-1) & i==pi)
             {
               ##This is finding the cost of moving from a point-point mapping to
               ##a point-segment mapping, along j.  The cost is the area of the triangle,
               ##regardless of the 'opposite' status (hopefully)
               opp <- opposite[i,j]
               vecstart <- unlist(xy2[l2keya,])
               vecdelta <- unlist(xy2[l2keyb,])-vecstart
               newend <- vecstart + vecdelta *opp

               point <- unlist(xy1[l1key,])

               cost <- squarederror(vecstart,newend,point,point)

             }else if(pj == j & (i-2)==(pi))
               {
                 ##j is even, so the j path is a segment:
                 ##i is odd, so it is just a single point
                 ##jump from (i-2,j)
                 ##

                 opp <- opposite[i,j]
                 
                 vecstart <- unlist(xy2[l2keya,])
                 vecdelta <- unlist(xy2[l2keyb,])-vecstart
                 point <- unlist(xy1[l1key,])
                 lastpoint <- unlist(xy1[l1key-1,])
                 
                 ##in this case, the current tentative mapping should be the orthogonal line
                 ##connected to the point, rather than the whole segment (next point).
                 ##Thus, we have a newpoint (vecend) along l1ab.
                 vecend <- vecstart + vecdelta * opp ##the proportional distance.
                 
                 lastopp <- opposite[i-2,j]
               
                 if(lastopp>0)
                   {

                     ##the previous mapping was 'opposite'.  This means
                     ##we need to find the two points in the i vector, and
                     ##the current and previous point on the j vector, and
                     ##compute the area of the polygon.
                     
                     vecprevend <- vecstart + vecdelta * lastopp

                     cost <- squarederror(vecprevend,vecstart,point,lastpoint)
                     ##Now, if the current point is not opposite, we need
                     ##to measure the following 3-gon:

                   }else {
                         cost <- Inf
                    
                   }
               }else{
                 ##if that previous point was not 'opposite', we shouldn't use this route;
                 
                 cost <- Inf
                 
               }
                       

           

         } else {
           cost <- Inf
         }

  return(cost)
}



## This computes the integrated squared deviation between
## xy1xy2 and xy3xy4, i.e., the polygon xy1234.
## It does so by first transforming the axis so that the midline
## between the two lines is y=0, and midpoint xy1xy4 is (0,0).
## this makes the integration (via line integrals and green's theorem)
## easier and simpler to compute.

squarederror <- function(xy1,xy2,xy3,xy4,plotme=F)
  {
      
    plotme <- T
    a <- (xy1+xy4)/2
    b <- (xy2+xy3)/2
    deter <- (abs(det(cbind(rbind(xy1,xy2,xy3),c(1,1,1)))) +
              abs(det(cbind(rbind(xy2,xy3,xy4),c(1,1,1)))))
    if(deter<.00000001)
      {
        print("zero or neg det")
        print(deter)
        print(xy1)
        print(xy2)
        print(xy3)
        print(xy4)
        plot(rbind(xy1,xy2,xy3,xy4),main="DETERMINANT PROBLEM")

        #return(0)
      }
    ##0-area plots are not uncommon and are
    ##problematic if we let the machinery handle them.
    if(all(xy1==xy2)&all(xy3==xy4))
      {
        print("colinear")
        return(0)
      }
    ##what if the slope is infinite?

    if(b[1]==a[1])
      {
        print("Infinite slope:")
        cat(xy1,"-",xy2,"-",xy3,"-",xy4,"--",a,"-",b,"\n")

        ##infinite slope.  swap x,y and redo:
        return(squarederror(xy1[2:1],xy2[2:1],xy3[2:1],xy4[2:1],plotme=plotme))
      }

    I <- a[2] - (b[2]-a[2])/(b[1]-a[1]) * a[1]
    m <-  (b[2]-a[2])/(b[1]-a[1])


    xs <- c(xy1[1],xy2[1],xy3[1],xy4[1])
    ys <- c(xy1[2],xy2[2],xy3[2],xy4[2])
    
    newys <- (ys - m*xs - I)/ sqrt(m^2+1)

    if(m==0)
      {
        newxs <- xs - a[1]
        
      }else{
        newxs <- sign(m)*(ys + 1/m * xs  - (a[2] + a[1]/m))/sqrt(  (-1/m)^2+1)
      }

        print("Proper polygon")
        print(xy1)
        print(xy2)
        print(xy3)
        print(xy4)

    ##this plots the original and transformed polygons
    if(plotme)
      {
        mat <- cbind(xs,ys)
        xrange <- range(c(mat[,1],newxs))
        yrange <- range(c(mat[,2],newys))

        plot(mat[c(1:4,1),],xlab="x",ylab="y",type="o",pch=16,cex=3, xlim=xrange,ylim=yrange)

        text(mat[,1],mat[,2],1:4,col="white")
        abline(0,0)
        abline(v=0)
        abline(I,m,lwd=2,lty=2)
        points(c(a[1],b[1]),c(a[2],b[2]),cex=3,pch=16)
        text(c(a[1],b[1]),c(a[2],b[2]),c("A","B"),col="white")

        polygon(cbind(newxs,newys),density=5,border="red")
      }


    ##now, xy have been transformed so that m=0 and I=0.  This makes
    ##the formulaes for the line integral much simplerer.



    xys <- cbind(newxs,newys)

    ##transform the four points so they are around 
    
    p1 <- part(xys[1,1],xys[1,2],xys[2,1],xys[2,2])
    p2 <- part(xys[2,1],xys[2,2],xys[3,1],xys[3,2])
    p3 <- part(xys[3,1],xys[3,2],xys[4,1],xys[4,2])
    p4 <- part(xys[4,1],xys[4,2],xys[1,1],xys[1,2]) 

    if(plotme)title(sub=abs(p1+p2+p3+p4))
    print((p1 + p2 + p3 + p4))
    abs(p1 + p2 + p3 + p4)
  }

## squarederror(c(0,-1),c(6,-1),c(6,1),c(0,1),plotme=T)#baseline centered on 0
## squarederror(c(0,4),c(6,4),c(6,6),c(0,6),plotme=T)    ##raise it up by 5
## squarederror(c(5,-1),c(11,-1),c(11,1),c(5,1),plotme=T)  ##move right by 5
## squarederror(c(5,4),c(11,4),c(11,6),c(5,6),plotm=T)    ##up and right


## ##what if we have a trapezoid.  These should be the same, but they are not.
## squarederror(c(0,-1),c(6,-1),c(6,1),c(0,1),plotme=T)  ##baseline centered on 0
## squarederror(c(0,-1),c(6,-1),c(7,1),c(0,1),plotme=T)  ##baseline centered on 0
## squarederror(c(0,-1),c(7,-1),c(6,1),c(0,1),plotme=T)  ##baseline centered on 0
## squarederror(c(0,-1),c(7,-1),c(7,1),c(0,1),plotme=T)  ##baseline centered on 0

## squarederror(c(0,-2),c(4,-1.8),c(5,1.9),c(0,2),plotme=T)  ##baseline centered on 0
 

## squarederror(c(10,10),c(14,7),c(16,11),c(12,12),plotme=T)
## squarederror(c(10,10),c(14,9),c(16,10),c(12,12),plotme=T)
## squarederror(c(10,10),c(14,9),c(16,15),c(12,12),plotme=T) 

## squarederror(c(5,-1),c(11,-2),c(8,5),c(2,9),plotme=T)
## squarederror(c(5,-1),c(11,-2),c(8,5),c(2,1),plotme=T)  

## ##what about negative x values

## squarederror(c(-3,-1),c(11,-2),c(8,5),c(-9,9),plotme=T)
## squarederror(c(-3,-1),c(11,4),c(8,5),c(-9,9),plotme=T)

## ##what if we have an infinite slope?

## squarederror(c(1,-1),c(5,3),c(-5,3),c(-1,-1),plotme=T)


## Here, I is the intercept of the midline
##       m is the slope of the midline.
## part <- function(x1,y1,x2,y2,m,I)
##   {
##     dx <- x2-x1
##     dy <- y2-y1
##     cat(L(x1,y1,m,I), "* ", dx, " +", M(x1,y1,m,I),"*",dy,"\n")
##     (L(x1,y1,m,I))*dx+ (M(x1,y1,m,I))*dy 
##   }

##This gives the M function
## M <-  function(x,y,m,I)
##   {
##     1/(m^2+1) * (m^2*x^3/3 + m*I*x^2 + I^2*x - m*x^2*y/2)
##   }
## L <- function(x,y,m,I)
##   {
##     -1/(m^2+1) *  (y^3/3  -   I*y^2 - m*x*y^2/2)
##   }
  


part <- function(x1,y1,x2,y2,m)
  {
    -(1/6) * ( (x2-x1))  *
      ((y2-y1)^2 * (2*y1) +
       3*y1*(y2-y1)*(y1)+
       y1^2 * (2*y1)+
       + 1/2 *(y2-y1)^3) 
  }


## The standard method will be distorted for GPS coordinates,
## so we need to be able to measure distance and area based on
## the sides defined in GPS space.
## see the followings;
##http://www.movable-type.co.uk/scripts/latlong.html
GPSDistance <- function(lon1,lat1,lon2,lat2)
{
    	
    R = 6371000 # // meters radius of earth
    phi1 = lat1 * pi/ 180
    phi2 = lat2* pi / 180
    
    deltaphi = (lat2-lat1) * pi / 180
    deltalambda = (lon2-lon1)* pi/ 180

    a = sin(deltaphi/2) * sin(deltaphi/2) +
        cos(phi1) * cos(phi2) *
        sin(deltalambda/2) * sin(deltalambda/2);

    c = 2 * atan2(sqrt(a), sqrt(1-a));

    d = R * c;
   return(d)    
}
