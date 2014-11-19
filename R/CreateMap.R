CreateMap <-
function(xy1.1,xy2.1,
         plotgrid = F,
         costfn = Cost.area,
         nondecreasingos=F,
         verbose=F,
         insertopposites=T)
{
    
    impliedpoints <- InsertIntersections(xy1.1,xy2.1,insertopposites=insertopposites)

    xy1 <- impliedpoints[[1]]
    xy2 <- impliedpoints[[2]]


    ##for later reference, we need a way of moving between xy1/xy2 and xy1.1/xy2.1
    
    l1 <- nrow(xy1)
    l2 <- nrow(xy2)
    
    l1.b <- 2*l1-1 ##size of bigger cost matrix
    l2.b <- 2*l2-1 ##size of bigger cost matrix


#    dists <- matrix(0,nrow=l1,ncol=l2)
#    for(i in 1:l1)
#      for(j in 1:l2)
#        {
#          dists[i,j] <-sqrt((xy1[i,1] - xy2[j,1])^2 + (xy1[i,2] - xy2[j,2])^2)
#        }
    
    ##the lower and upper nodes for each cell of costs:
    l1keya <-c(rep(1:(l1-1),each=2),l1)
    l1keyb <-c(1,rep(2:(l1),each=2))
    
    l2keya <-c(rep(1:(l2-1),each=2),l2)
    l2keyb <-c(1,rep(2:(l2),each=2))
    
    ##now, we need to find the least-cost chain.
    ##But cost needs to be defined as area of each consecutive polygon.

    ##We want to determine which pairs of
    ## node-segment and segment-nodes are
    ## opposite of one another; the line orthogonal to
    ## the segment intersecting the point also intersects the
    ## line.
    if(verbose) print("Computing matches")
    matches <- matrix(0,l1.b,l2.b)
    for(i in 1:l1.b)
      {
        if(verbose) print(paste(i,"of", l1.b,l2.b))
    
        for(j in 1:l2.b)
          {
            if(odd(i) & even(j))
              {

                intprop <- IntersectPoint(unlist(xy2[l2keya[j],]),unlist(xy2[l2keyb[j],]),unlist(xy1[l1keya[i],]))


                if(intprop<0)
                  {
                    matches[i,j]<- 0
                  }else if (intprop>1)
                    {
                      matches[i,j]<-1
                    }
                  else
                    {
                      matches[i,j]<- intprop
                    }

                ##Now, make sure o is non-decreasing.
                if(i>1 & nondecreasingos)
                  {
                    ##This makes the o's non-decreasing on a segment.
                    ##it isn't quite right, because it doesn't choose
                    ##the optimal set of non-decreasing os, which is
                    ##a much more complex optimization.
                    matches[i,j] <- max(matches[i,j],matches[i-2,j])
                  }

              }
            if(even(i) & odd(j))
              {
                

                intprop <- IntersectPoint(unlist(xy1[l1keya[i],]),unlist(xy1[l1keyb[i],]),unlist(xy2[l2keya[j],]))

                if(intprop<0)
                  {
                    matches[i,j]<-0
                    }else if(intprop>1)
                  {
                    matches[i,j]<- 1
                  }else{
                    matches[i,j]<- intprop
                  }

                ##Now, make sure o is non-decreasing.
                if(j>1&nondecreasingos)
                  {
                    matches[i,j] <- max(matches[i,j],matches[i,j-2])
                  }

              }
          }
      }


    pathEnvelope <-  matrix(F,l1.b,l2.b)
    pathEnvelope[1,1] <- T
    
    ##matches now identifies whether we can match a thing in A-list to a thing in B-list.
    ##normally, 
    
    if(plotgrid)
      {
        par(mfrow=c(1,1))
        PlotGrid(l1,l2)
      }

    ##the (cumulative) least cost network
    leastcost <-matrix(0,nrow=l1.b,ncol=l2.b)

    ##the  distance associated with each link between
    ##paths.  This is used to select among the equivalent
    ## area mappings
    linkcost <- matrix(0,nrow=l1.b,ncol=l2.b)

    if(verbose)print("Computing linkage costs")
    for(i in 1:l1.b)
      {
        if(verbose)cat(".")
        for(j in 1:l2.b)
          {
            linkcost[i,j] <- LinkCost(xy1,xy2,i,j)
           }
       }
    if(verbose)cat("\n")


    chain <- matrix(0,nrow=2*(l1+l2))
    bestpath <- array(0,c(l1.b,l2.b,2),c("d1","d2","xy"))

    if(verbose) print("Computing paths:")
    id <- 1
    for(i in 1:(l1.b))
      {
        if(verbose)print(paste(i, "of",l1.b))
        for(j in 1:(l2.b))
          {


            if(i==1 & j==1)
              {
                tmpcosts <- 0
                bestpath[i,j,] <- c(1,1)  ##dummy here
                leastcost[i,j]<-min(tmpcosts,na.rm=T)
                
              } else if(odd(i) & odd(j))
                  {
                    ## The current mapping is just a point-point mapping,
                    ## which has an implicit distance of 0.
                    ## But to get here, were in one of three states before:

                    ## 1. a segment-point
                    ## 2. a point-segment.

                    ## and the (possibly) uneccessary:
                    ## 3. previous point-point.  This is just the same as
                    ##                           a 1-2 or a 2-1 move, so we will ignore that.
                    

                    pair1 <- c(i,j-1)
                    pair2 <- c(i-1,j)
                    pair3 <- c(i-2,j-2)
                    
                    ## The cost of getting to this node is 0 if
                    ## the the previous point was not 'opposite',
                    ## otherwise it is the remainder of the area.
                    if(all(pair1>0))
                      {
                        path1 <- leastcost[pair1[1],pair1[2]]
                      } else {
                        path1 <-Inf
                      }
                    
                    cost1 <- Cost(xy1,xy2,i,j,i,j-1,matches,costfn=costfn)
                    
                    if(all(pair2>0))
                      {
                        path2 <- leastcost[pair2[1],pair2[2]]
                      }else{
                        path2  <- Inf
                      }
                    cost2 <- Cost(xy1,xy2,i,j,i-1,j,matches,costfn=costfn)
                    

                    if(all(pair3>0))
                      {
                        path3 <- leastcost[pair3[1],pair3[2]]
                      }else{
                        path3  <- Inf
                      }
                    cost3 <- Cost(xy1,xy2,i,j,i-2,j-2,matches,costfn=costfn)


                    
                    ##The actual cost to minimize
                    cost <- min(path1+cost1,path2+cost2,path3+cost3,na.rm=T)                    

                    ##which options have that cost.  Let small differences because of calculation be ignored.
                    opts <- c(path1+cost1,path2+cost2,path3+cost3)
                    choices <- which((opts-min(opts))<.00001)
                    
                    leastcost[i,j] <- min(opts)
                    ##Compute the minimum link cost, based on only the equal-area options.

                    
                    ##there are only 3 options
                    if(length(choices)>1)
                      {
                        ##it could be any of them..choose the shortest cumulative cost.
                        ##this prefers the left, center,  then right.
                        choice <- which.min(c(1,3,2)[choices])
                        
                      }else{

                        choice <- choices
                        
                      }
                    
                    ##pick from the min-area paths that have the least linkage cost.
                    bestpath[i,j,] <-rbind(c(i,j-1),c(i-1,j),c(i-2,j-2))[choice,]

                      
                    if(plotgrid)
                      {
                        points(j-.5,l1.b-i+1,pch=16,col="white",cex=3)
                        text(j-.5,l1.b-i+1,round(cost1,2),cex=.8,col="black")
                        
                        points(j,l1.b-i+1.5,pch=16,col="white",cex=3)
                        text(j,l1.b-i+1.5,round(cost2,2),cex=.8,col="black")
                        
                        points(j-.5,l1.b-i+1.5,pch=16,col="white",cex=3)
                        text(  j-.5,l1.b-i+1.5,round(cost3,2),cex=.8,col="black")

                        points(j,l1.b-i+1,pch=16,col="grey",cex=3)
                        text(j,l1.b-i+1,round(leastcost[i,j],2),cex=.8,col="black")
                        
                      }
                    
                    
                  }else if(even(i) & odd(j))
                      {
                        

                        ##this is a mapping between an edge and a point.
                        ## we could have gotten here from two directions, directly up (i-1)
                        ## or two to the left (j-2)

                        cost1 <- Cost(xy1,xy2,i,j,i-1,j,matches,costfn=costfn)
                        path1 <- cost1 + leastcost[i-1,j]

                        cost2 <- Cost(xy1,xy2,i,j,i,j-2,matches,costfn=costfn)
                        path2 <- cost2 + leastcost[i,j-2]
                            

                        ##which options have that cost?

                        opts <- c(path1,path2)
                        ##opts might be essentially the same, but different because
                        ##of rounding errors, so use a threshold here.
                        ##this will screw things up if you are using very very small
                        ##values.
                        choices <- which((opts-min(opts))<.00001)
                    


                        if(length(choices)>1)
                          {
                            choice <- which.min(c(1,2)[choices])
                          } else {
                            
                            choice <- choices
                          }

                        leastcost[i,j] <- min(path1,path2)
                        if(length(choices)>1)
                          {
                            ##it could be any of them..choose the shortest cumulative cost.
                            choice <- which.min(c(1,2))
                            
                          }else{
                            
                            ##choose randomly here when tied.
                            choice <- choices
                            
                          }
                        
                        bestpath[i,j,] <- rbind(c(i-1,j),c(i,j-2))[choice,]
                        
                        if(plotgrid)
                          {
                            ##eveni/oddj

                            #print(paste(i-1,j,i,j,cost1))
                            #print(paste(i,j-2,i,j,cost2))

                            points(j,l1.b-i+1+.5,pch=16,col="white",cex=3)
                            text(j,l1.b-i+1+.5,round(cost1,2),cex=.8,col="black")
                            
                            points(j-.5,l1.b-i+1,pch=16,col="white",cex=3)
                            text(  j-.5,l1.b-i+1,round(cost2,2),col="black",cex=.8)


                            points(j,l1.b-i+1,pch=16,col="grey",cex=3)
                            text(j,l1.b-i+1,round(leastcost[i,j],2),cex=.8,col="black")
                          }


                      }else if(odd(i) & even(j))
                        {
                            
                            ##The cost here is symmetric to the even/odd one above.

                          cost1 <- Cost(xy1,xy2,i,j,i-2,j,matches,costfn=costfn)
                          prev1 <- ifelse(i<3,0,leastcost[i-2,j]) ##guard for edge of network
                          path1 <- cost1 + prev1
                          
                          cost2 <- Cost(xy1,xy2,i,j,i,j-1,matches,costfn=costfn)
                          prev2 <- ifelse(j<2,0,leastcost[i,j-1])
                          path2 <- cost2 + prev2
                          
                          ##do 2/1 here so we still have a i vs j bias for min path.
                          opts <- c(path2,path1)
                          ##select all options that are equally minimal:
                          choices <- which((opts-min(opts))<.00001)

                          if(length(choices)>1)
                            {
                              choice <- which.min(c(1,2)[choices])
                            } else {
                              choice <- choices
                            }
                          leastcost[i,j] <- min(path1,path2)


                          bestpath[i,j,] <- rbind(c(i,j-1),c(i-2,j))[choice,]

                          
                            if(plotgrid)
                              
                              {
                                ##oddi/evenj
                                
                                #print(paste(i,j-1,i,j,cost1))
                                #print(paste(i-2,j,i,j,cost2))

                                points(j-.5,l1.b-i+1,pch=16,col="white",cex=3)
                                text(j-.5, l1.b-i+1,round(cost2,2),col="black",cex=.8)
                                      
                                points(j,l1.b-i+1+.5,pch=16,col="white",cex=3)
                                text(j,  l1.b-i+1+.5,round(cost1,2),col="black",cex=.8)

                                
                                points(j,l1.b-i+1,pch=16,col="grey",cex=3)
                                text(  j,l1.b-i+1,round(leastcost[i,j],2),col="black",cex=.8)
                              }
                            
                          }
          }
        
      }

    
    if(plotgrid)
      {
        ##create the  'best' path.


        i <- l1.b
        j <- l2.b
        path <- c(i,j)
        
        previ <- i
        prevj <- j


        while(i>1 | j >1)
          {

            points(j,l1.b-i+1,cex=3.1,col="red")

            #path <- rbind(c(i,j),path)
            previ <- i
            prevj <- j
            nexti <- bestpath[i,j,1]
            nextj <- bestpath[i,j,2]
            i <- nexti
            j <- nextj
          }
        
      }

    ## start at the end and work backward
    ## to find the chain
  
    return (list(path1 = xy1,  ##Complete path used, after point insertion
                 path2 = xy2,
                 origpath1 = xy1.1,  ##Original points, ignoring implied points
                 origpath2 = xy2.1,
                 key1 = impliedpoints[[3]],
                 key2 = impliedpoints[[4]],
                 linkcost = linkcost,
                 leastcost = leastcost,
                 bestpath =bestpath,
                 minmap = FALSE,
                 #path = path,
                 opposite = matches,
                 deviation=leastcost[nrow(leastcost),ncol(leastcost)])
            )

  }
