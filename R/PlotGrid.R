

PlotGrid <- function(path1,path2)
  {
    x <- path1
    y <- path2
    p<- 2*x-1
    q <- 2*y-1
    live <- livenodes(x,y)
    xs <- rep(1:p,q)
    ys <- rep(1:q,each=p)

    ptsx <-  p:1
    ptsy <- 1:q  ##Backward so it plots top-to-down.

    plot(ptsy[ys],ptsx[xs],pch=c(1,16)[as.numeric(live)+1],
         xlim=c(0,max(ptsy)+1),ylim=c(0,max(ptsx)+1),
         cex=as.numeric(live)*4+1.4,col="grey",
          xaxt="n",yaxt="n",bty="n",xlab="",ylab="")

    points(ptsy[ys],ptsx[xs],pch=1,
           cex=as.numeric(live)*4+1.4,col="black")

    xbase <- rep(1:x,each=2)
    xlab <- paste(xbase[1:p],xbase[2:length(xbase)],sep=",")
    axis(2,ptsx,xlab,las=1)
    ybase <- rep(1:y,each=2)
    ylab <- paste(ybase[1:q],ybase[2:length(ybase)],sep=",")
    axis(3,ptsy,ylab)

    
    for(i in 1:p)
      for(j in 1:q)
        {
          for(ii in i:p)
            for(jj in j:q)
              {
                if(connected(i,j,ii,jj))
                  {
                    arrows(ptsy[j],
                           ptsx[i],
                           max(ptsy[j],ptsy[jj]-.15),
                           min(ptsx[i],ptsx[ii]+.15),
                           length=.1,angle=15,lwd=2)
                  }
              }
        }
  }
