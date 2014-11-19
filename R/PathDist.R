PathDist <-
function(path)
  {
      ##if we have a single-point path,
      ##the length has to be 0. IN this case,
      ##a matrix might transform to a vector.
      if(!is.matrix(path))
          {
              return(0)
          }
      else{

      ##re-add the start and end nodes.
      p1 <- rbind(path[1,],path)
      p2 <- rbind(path,path[nrow(path),])
      
      return(   sum(sqrt(rowSums((p1-p2)^2))))
  }
  }
