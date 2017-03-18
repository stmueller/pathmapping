

SimplifyPath <-
function(path,tolerance=.075,truncate=F,faster=T,verbose=F,plot=F)
{

  if(plot)
    plot(path,col="grey70")

    if(truncate)
    {
        path <- round(path,truncate)
    }
    
  ##keepres
  ldist <- PathDist(path)
  cont <- 1
  newpath <- path
  while(cont)
    {

      if(verbose)cat("Number of points: " ,nrow(newpath))
      devs <- rep(Inf,nrow(newpath))
      
      for(i in 2:(nrow(newpath)-1))
        {
          
          pprev <- newpath[i-1,]
          pcur <-  newpath[i,]
          pnext <- newpath[i+1,]

          devs[i] <-LLKscore(pprev,pcur,pnext,ldist)
        }

      ##Some might be NA because of 0 length:
      devs[is.na(devs)]<-Inf
      #print(devs)
      min <- min(devs)

      ##Remove the first one that is minimal.
      if(min<tolerance)
        {
          if(faster)
            {
              if(min*1.1>tolerance)
                cutoff <- min
              else
                cutoff <- min*1.1

              remove <- devs<=cutoff
            }
          else
            {
              cutoff <- min
              remove <- devs<=cutoff
            }

          if(verbose)cat("   cutoff: ",cutoff)
          if(verbose)cat("  Removing [",sum(remove),"] points")
          newpath <- newpath[!remove,]
#          argmin <- which.min(devs)
#          newpath <- newpath[-argmin,]
        }else{
          cont <- 0
        }
      if(verbose)cat("\n")
      if(plot)  points(newpath,type="l")
    }

  if(plot)
    points(newpath,col="red",pch=16,cex=2)
  newpath
}
