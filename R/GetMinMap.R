GetMinMap <-
function(mapping,leftbias = T,verbose=F)
  {

      
    ##First, create the 'implied' points from the mapping:
    l1 <- nrow(mapping$path1)
    l2 <- nrow(mapping$path2)
    
    l1.b <- 2*l1-1 ##size of bigger cost matrix
    l2.b <- 2*l2-1 ##size of bigger cost matrix


    ##Now, create a matrix that records the least-cost chain.
    ## This will incorporate just the
    ## link cost, which we have not minimized.
    leastcostchain <- matrix(0,l1.b,l2.b)
    
    for(i in 1:(l1.b))
      {
        if(verbose)print(paste(i,"of",l1.b))

        for(j in 1:(l2.b))
          {
            curcost <- mapping$linkcost[i,j]
            
            if(i==1 & j==1)
              {
                leastcostchain[i,j]<-mapping$linkcost[i,j]
                
              } else
            if(even(i) & even(j))
              {
                  leastcostchain[i,j] <- Inf
                  
              }else{
                  
                  ##Determine the previous nodes.
                  ##there are up to 3, and if there are only 2, use 0,0 as the placeholder.
                  if(odd(i) & odd(j))
                      {
                          
                          prevL <- c(i,j-1)  ##left
                          prevD <- c(i-2,j-2)  #diagonal back
                          prevU <- c(i-1,j)  ##up 

                          
                      } else
                          if(even(i) & odd(j))
                              {
                                  prevL <- c(i,j-2) #left
                                  prevD <- c(0,0)  #diagonal
                                  prevU <- c(i-1,j) #up         

                                  
                              }else
                                  if(odd(i)&even(j))
                                      {
                                          prevL <- c(i,j-1) #left
                                          prevD <- c(0,0)    ##to the cloud!
                                          prevU <- c(i-2,j)  #up

                                      }
                  
                  
                  ##look at the different paths, moving backward.
                  ##we need to determine which are legal paths.  
                  if(all(prevL>0))
                      {
                          ##diff meeasures the change in the area-based minimization grid
                          diff <- (mapping$leastcost[i,j] - mapping$leastcost[prevL[1],prevL[2]])
                          
                          ##delta measures the actual cost.  A path is legal if the actual cost between
                          ##the nodes is the same as the least-cost path.
                          delta <- Cost(mapping$path1,mapping$path2,i,j,prevL[1],prevL[2],mapping$opposite)
                          
                          
                          ##this might go wrong if you are working with
                          ##values really small, but since diff and delta are
                          ##often zero OR Inf, you can't just scale by the size;
                          pathLlegal <- abs((diff-delta))<.000001
                          
                      } else {
                          pathLlegal <- F
                      }
                  
                  
                  if(all(prevU>0))
                      {
                          diff <- (mapping$leastcost[i,j] - mapping$leastcost[prevU[1],prevU[2]])
                          delta <- Cost(mapping$path1,mapping$path2,i,j,prevU[1],prevU[2],mapping$opposite)
                          pathUlegal <- abs((diff-delta))<.000001
                          
                      } else {
                          
                          pathUlegal <- F
                      }
                  
                  
                  if(all(prevD>0))
                      {
                          diff <- (mapping$leastcost[i,j] - mapping$leastcost[prevD[1],prevD[2]])
                          delta <- Cost(mapping$path1,mapping$path2,i,j,prevD[1],prevD[2],mapping$opposite)
                          pathDlegal <- (abs(diff-delta)<.000001)
                          
                      } else {
                          
                          pathDlegal <- F
                      }
                  
                  
                  ##Now, we know whether either path1 or path2 are legal.
                  ##if 'legal' this means that they are feasible paths to the minimum.
                  ##Use only the legal ones to compute leastcost. Here, we will still
                  ##prefer the leftmost path (where row changes before column changes),
                  ##and we can reverse the order to see if it matters.
                  if(leftbias)
                      {
                          picks <- c(pathLlegal,pathDlegal,pathUlegal)
                      }else{
                          picks <- c(pathUlegal,pathDlegal,pathLlegal)
                      }
                  
                  if(pathLlegal)
                      costL <- leastcostchain[prevL[1],prevL[2]]
                  else
                      costL <- Inf
                  
                  
                  if(pathUlegal)
                      costU <- leastcostchain[prevU[1],prevU[2]]
                  else
                      costU <- Inf
                  
                  if(pathDlegal)
                      costD <- leastcostchain[prevD[1],prevD[2]]
                  else
                      costD <- Inf
                  

                  if(leftbias)
                      {
                          costs <- c(costL,costD,costU)
                          prevs <- rbind(prevL,prevD,prevU)
                      }else{
                          costs <- c(costU,costD,costL)
                          prevs <- rbind(prevU,prevD,prevL)
                      }
                  
                  
                  ##Let's pick which path is the smallest.
                  argmin <- which.min(costs)
                  bestprev <- prevs[argmin,]
                  leastcostchain[i,j] <- curcost + costs[argmin]
                  
                  mapping$bestpath[i,j,] <- bestprev
                  
                  
              }
        }
    }


    
    
    mapping$leastcostchain<- leastcostchain
    mapping$minmap <- TRUE
    mapping$minlinkcost <- leastcostchain[nrow(leastcostchain),ncol(leastcostchain)]

    mapping
}
