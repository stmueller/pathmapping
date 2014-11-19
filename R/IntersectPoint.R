IntersectPoint <-
function(A, B, C)
  {

    if(all(A==B))
      {
        return (0)
      }
    ##This gives a proportion of AB that the
    ##orthogonal line passing through C meets. If outside (0,1)
    ##it does not pass through AB
    ((B-A) %*% (C-A)) / ((B-A) %*% (B-A))
  }
