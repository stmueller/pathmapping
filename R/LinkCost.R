LinkCost <-
function(xy1,xy2,i,j)
  {

    
    if(odd(i)&odd(j))
      {
        l1key <- (i+1)/2
        l2key <- (j+1)/2
        linkcost <- sqrt(sum( (unlist(xy1[l1key,])-unlist(xy2[l2key,]))^2))

      }else if(odd(i)&even(j))
        {
          
          l1key <- (i+1)/2
          l2keya <- j/2
          l2keyb <-j/2+1

          pt <- unlist(xy1[l1key,])
          linea <-unlist(xy2[l2keya,])
          lineb <-unlist(xy2[l2keyb,])
          
          linkcost <- DistancePointSegment(pt[1],pt[1],linea[1],linea[2],lineb[1],lineb[2])
          
        }else if(even(i)&odd(j))
          {
            
            l1keya <- i/2
            l1keyb <- i/2+1
            l2key <-(j+1)/2

            pt <- unlist(xy2[l2key,])
            linea <-unlist(xy1[l1keya,])
            lineb <-unlist(xy1[l1keyb,])
            
            linkcost <- DistancePointSegment(pt[1],pt[2],linea[1],linea[2],lineb[1],lineb[2])
            
          }else{
            linkcost <- Inf ##this shouldn't really happen; but it
            ##applies to segment-segment mappings.
          }


    return (linkcost)
  }
