#importFrom("base","plot","points","polygon","lines","title")
ggPlotMap <-  
  function(mapping,cols=c("grey40"),linecol="grey25",xlim=NA,ylim=NA,...)
{

  if(!mapping$minmap==FALSE)
   {
     mapping <- GetMinMap(mapping)
   }

    overlap <- PathOverlap(mapping)
   s <- SummarizeMapping(mapping)  

   ##this will plot each line connecting the paths.
   x1 <- c(s$p1x,s$p2x)
   y1 <- c(s$p1y,s$p2y)
   conn1 <- rep(paste(s$node1,s$node2,sep="-"),2)
   swide <- data.frame(x=x1,y=y1,conn=conn1)
   
   
   ##make the polygons.  To do so, we need each connection pairing doubled.
   n <- nrow(s)
   x2 <- c(c(s$p1x[1:(n-1)],s$p2x[1:(n-1)]),
           rev(c(s$p1x[2:(n)],  s$p2x[2:(n)])))
   
   y2 <- c(c(s$p1y[1:(n-1)],s$p2y[1:(n-1)]),
           rev(c(s$p1y[2:(n)],  s$p2y[2:(n)])))
   

   conn2 <- c(rep(conn1[1:(n-1)],2), 
             rev( rep(conn1[1:(n-1)],2)))
   
   
   ##double these with offset to make polygons
   x <- x2        ##c(x2[1:(n-1)],rev(x2[2:n]))
   y <- y2        ##c(y2[1:(n-1)],rev(y2[2:n]))
   conn <- conn2  ##c(conn2[1:(n-1)],(conn2[1:(n-1)]))
   
   swide2 <- data.frame(x,y,conn)
   
  
    plot <- ggplot(data=s)+
    geom_polygon(data=swide2,aes(x=x,y=y,group=conn),col="grey40",fill="grey42",size=3)+ #grey polygon
    geom_line(data=swide,aes(x=x,y=y,group=conn),col="grey10",size=.2)+  #connector line
      ##Path1
    geom_path(data=s,aes(x=p1x,y=p1y),col="red",size=1.7) + geom_point(aes(x=p1x,y=p1y),size=.5) +
     geom_point(data=subset(s,s$orig1>0),aes(x=p1x,y=p1y),size=3,shape=21)+
     ##path 2
     geom_path(data=s,aes(x=p2x,y=p2y),col="gold",size=1.2) + geom_point(aes(x=p2x,y=p2y),size=.5)+ 
     geom_point(data=subset(s,s$orig2>0),aes(x=p2x,y=p2y),size=3,shape=21) + 
     
    labs(title=paste("Total deviation: ", round(mapping$deviation,3),"\n",
                     "Length overlap: ", round(overlap,3)
                     ))

  return (plot)
  }
