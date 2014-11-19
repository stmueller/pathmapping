##This returns a data frame/matrix identifying the mapping point
##of each node in one path of a mapping to the other.
SummarizeMapping <- function(mapping)
    {
        ##now, mapping$bestpath will provide the best
        ##mapping, but we need to walk through it to
        ##generate the actual path.

        xy1 <- c()
        xy2 <- c()
        dist <- c()
        cumdist1 <-c(0) ##cumulative distance along path 1
        cumdist2 <- c(0) ##cumulative distance along path 1

        if(mapping$minmap)
            {
                i <- nrow(mapping$bestpath[,,1])
                j <- ncol(mapping$bestpath[,,2])
                bestmapping <- c(i,j)
                dist <- c(mapping$linkcost[i,j],dist)
                
                x1index <- (i+1)/2
                x2index <- (j+1)/2
                
                xy1 <- mapping$path1[(i+1)/2,]
                xy2 <- mapping$path2[(j+1)/2,]
                
                while(i>1 |j>1)
                    {


                        newi <- mapping$bestpath[i,j,1]
                        newj <- mapping$bestpath[i,j,2]


                        
                        i <- newi
                        j <- newj
                        bestmapping <- rbind(c(i,j),
                                             bestmapping)
                        ##find the link distances:
                        dist <- c(mapping$linkcost[i,j],dist)

                        
                        opp <- mapping$opposite[i,j]
                        ##find the actual coordinates?
                        ##this depends on if the index is even or not.
                        if(even(i))
                            {
                                x1index <- i/2
                                xy1point <- mapping$path1[x1index,]*(1-opp) +  (mapping$path1[x1index+1,] *(opp))
                            }else{
                                x1index <- (i+1)/2
                                xy1point <- mapping$path1[x1index,]
                            }
                        if(even(j))
                            {
                                x2index <- j/2
                                xy2point <- mapping$path2[x2index,]*(1-opp) +  (mapping$path2[x2index+1,] *(opp))
                            }else{
                                x2index <- (j+1)/2
                                xy2point <- mapping$path2[x2index,]
                            }

                        ##Now, we need the point of attachment.  i is even
                        ##or j is even, that point requires looking at the
                        ##opposite matrix.
                        
                        
                        xy1 <- rbind(xy1point,xy1)
                        xy2 <- rbind(xy2point,xy2)

                        cumdist1 <- c(PathDist(xy1[1:2,])+cumdist1[1],cumdist1)
                        cumdist2 <- c(PathDist(xy2[1:2,])+cumdist2[1],cumdist2)
                    }


                ##now, figure out which rows correspond to the original points.
                ##key1/key2 refers to the actual points, not the segments

                key1 <- mapping$key1
                
                keep1 <- (key1==round(key1))&key1>0
                ##this is the rows of the matrix associated with
                ##the original:
                key1base1<-((1:length(keep1)))[keep1] 
                key1base <- key1base1*2-1
                reversekey1 <- rep(0,nrow(mapping$bestpath[,,1]))
                reversekey1[key1base] <- 1:length(key1base)

                ##valid is good, but we need to figure out which
                ##original element it actuall mapped to:
                ##
                valid1 <- is.element(bestmapping[,1],key1base)


                
                key2 <- mapping$key2
                keep2 <- (key2==round(key2))&key2>0
                ##this is the columns of the matrix associated with
                ##the original path2
                key2base1 <- ((1:length(keep2)))[keep2]
                key2base <- key2base1*2-1

                reversekey2 <- rep(0,ncol(mapping$bestpath[,,1]))
                reversekey2[key2base] <- 1:length(key2base)
                valid2 <- is.element(bestmapping[,2],key2base)

                valid1[valid1] <-reversekey1[bestmapping[valid1,1]]
                valid2[valid2]<-reversekey2[bestmapping[valid2,2]]    
                bestmapping2 <- data.frame(bestmapping[,1],bestmapping[,2],
                                           valid1,valid2,
                                           xy1[,1],xy1[,2],
                                           xy2[,1],xy2[,2],
                                           cumdist1[1]-cumdist1,cumdist2[1]-cumdist2,
                                           dist) 
                colnames(bestmapping2) <- c("node1","node2","orig1","orig2","p1x","p1y","p2x","p2y","cumdist1","cumdist2","dist")
#                rownames(bestmapping2)<-rep("",nrow(bestmapping2))
                
                return(bestmapping2)
            }else{
                message("Function SummarizeMapping failed.  Argument needs to have a specific minimal mapping computed using GetMinMap prior to using this function")
                
            }
    }

