PlotMap <-
function(mapping,cols=c("grey40"),linecol="grey25",xlim=NA,ylim=NA,...)
  {
    xy1 <- mapping$path1
    xy2 <- mapping$path2

    bestpath <- mapping$bestpath
    opposite <- mapping$opposite
    
    l1 <- nrow(xy1)
    l2 <- nrow(xy2)
    
    l1.b <- 2*l1-1 ##size of bigger cost matrix
    l2.b <- 2*l2-1 ##size of bigger cost matrix

    ##This maps l1 to /1b
    l1keya <-c(rep(1:(l1-1),each=2),l1)
    l1keyb <-c(1,rep(2:(l1),each=2))
    
    l2keya <-c(rep(1:(l2-1),each=2),l2)
    l2keyb <-c(1,rep(2:(l2),each=2))

    if(is.na(xlim[1]))
        {
            xlim=range(c(xy1[,1],xy2[,1]))
        }
    
    if(is.na(ylim[1]))
        {
            ylim=range(c(xy1[,2],xy2[,2]))
        }

    plot(xy1[,1],xy1[,2],type="o",col="red",lwd=3,bty="n",xlab="",ylab="",
         xlim=xlim,ylim=ylim,...)

    points(xy2[,1],xy2[,2],type="o",pch=16,cex=.8,lwd=2.5)


    coli <- 1 ##color randomizer index
    sumarea <- 0
    i<-l1.b
    j<-l2.b

    while(i>1 | j>1)
      {

        ##get the row/column of cell from cost matrix:
        pi <- bestpath[i,j,1]
        pj <- bestpath[i,j,2]

        oppCurr <- opposite[i,j]
        oppPrev <- opposite[pi,pj]

        ##Adjust the xy points on current and previous
        ##here, we need to adjust xy[12]
        ##path1 current point is a segment.  It might be split

        ##odd i/odd j means mapping from point to point.
        ##the polygon for this mapping should always be 0,
        ## unless the previous path was 'opposite'.
        ##in that case, we have a 3-gon to draw,

        if(odd(i)&odd(j))
          {
            xyCurr1 <- xy1[l1keyb[i],]
            xyCurr2 <- xy2[l2keyb[j],]

            ##this is the point-point path
            if((i-pi)==2 & (j-pj)==2)
              {

                xyPrev1 <- xy1[l1keyb[pi],] 
                xyPrev2 <- xy2[l2keyb[pj],]                 
                
              }else if(oppPrev>0)
                {
                  
                  if(odd(pj))
                    {
                      xyPrev1 <- xy1[l1keyb[max(pi-1,1)],]  + oppPrev * (xy1[l1keyb[pi],]-xy1[l1keyb[max(pi-1,1)],])
                      xyPrev2 <- xy2[l2keyb[max(pj-1,1)],]                 
                    }
                  else
                    {
                      xyPrev1 <- xy1[l1keyb[max(pi-1,1)],] 
                      xyPrev2 <- xy2[l2keyb[max(pj-1,1)],]  + oppPrev * (xy2[l2keyb[pj],]-xy2[l2keyb[max(pj-1,1)],])
                    }
                }
              else
                {
                  ##No polygon drawn; just a line.
                  xyPrev1 <- xy1[l1keyb[i],]
                  xyPrev2 <- xy2[l2keyb[j],]

                }
          }
        else if(even(i) & odd(j))
          {

            #The best path will either come from i-1 or j-2
            ##if its i-1, then we have a triangle to draw
            ##if its j-2, we have a 4-gone, as j-2 was opposite.
            if(pi==(i-1))
              {
                ##triangle.
                xyCurr1 <- xy1[l1keyb[i],]
                xyCurr2 <- xy2[l2keyb[j],]
                xyPrev1 <- xy1[l1keyb[i-1],]
                xyPrev2 <- xyPrev1
                
              }else if(pj==(j-2)& oppPrev)
                {
                  ##point j-2 is opposite pi
                  ##pi should equal i
                  
                  xyCurr1 <- xy1[l1keyb[i],]
                  xyCurr2 <- xy2[l2keyb[j],]

                  
                  xyPrev1a <- xy1[l1keyb[pi-1],]
                  xydelta1 <- xyCurr1-xyPrev1a
                  xyPrev1 <- xyPrev1a + oppPrev * xydelta1

                  xyPrev2 <- xy2[l2keyb[pj],]


                  
                }else{
                  print("Error 1")
                }

            ##But wait--the current point i may be 'opposite' adjust if so
            if(oppCurr>0)
              {

                xyPrev1a <- xy1[l1keyb[i-1],]
                xydelta1b <- xy1[l1keyb[i],]-xyPrev1a
                xyCurr1 <- xyPrev1a  + oppCurr * xydelta1b
              }

            
          }
        else if(odd(i)&even(j)) ##path2 current point is a segment.  It might be split
          {

            #The best path will either come from j-1 or i-2
            ##if its j-1, then we have a triangle to draw
            ##if its i-2, we have a 4-gone, as i-2 was opposite.

            if(pj==(j-1))
              {
                ##triangle.
                xyCurr1 <- xy1[l1keyb[i],]
                xyCurr2 <- xy2[l2keyb[j],]

                xyPrev2 <- xy2[l2keyb[j-1],]                
                xyPrev1 <- xyPrev2
                
              }else if(pi==(i-2) & oppPrev)
                {
                  ## point i-2 is opposite pj!
                  ## pj should equal j
                  
                  xyCurr1 <- xy1[l1keyb[i],]
                  xyCurr2 <- xy2[l2keyb[j],]

                  xyPrev2a <- xy2[l2keyb[pj-1],]
                  xydelta2 <- xyCurr2-xyPrev2a
                  xyPrev2 <- xyPrev2a + oppPrev * xydelta2

                  xyPrev1 <- xy1[l1keyb[pi],]

                  
                }else{
                  print("Error 2")
                }

            ##But wait--the current point i may be 'opposite' adjust if so
            if(oppCurr>0)
              {
                ##point i is opposite segment j.
                
                xyPrev2a <- xy2[l2keyb[j-1],]
                xydelta2b <- xy2[l2keyb[j],]-xyPrev2a
                
                xyCurr2 <- xyPrev2a +  oppCurr * (xydelta2b)


              }
            


          }
        ##This represents a polygon.  If opposite[i,j] > 0,
        ##we need to plot the partial segment.

        
        xypoly <- rbind(unlist(xyCurr1),
                        unlist(xyCurr2),
                        unlist(xyPrev2),
                        unlist(xyPrev1))

        area<- surveyors(xypoly)
        sumarea<-sumarea +area
        coli <- (coli) %% length(cols)+1


        color <-cols[coli]

        if(all(c(pi,pj)>0))
           {
             polygon(xypoly[,1],xypoly[,2],  col=color,border=linecol,lwd=3)

        ## we should be able to eliminate the lines between polygons
        ##        segments(xy1[p1a,1],xy1[p1a,2],xy2[p2a,1],xy2[p2a,2])
        ##        segments(xy1[p1b,1],xy1[p1b,2],xy2[p2b,1],xy2[p2b,2])
           }
        
        i <- pi
        j <- pj
      }

    lines(xy1[,1],xy1[,2],type="l",col="red",lwd=2)
    lines(xy2[,1],xy2[,2],type="l",col="black",lwd=2)

    title(paste("area:",round(sumarea,2)))

    if(abs(sumarea/mapping$deviation-1)>.00001)
      {
        cat("Drawing area disagrees with computed area", mapping$dist, sumarea,"\n")
      }
  }
