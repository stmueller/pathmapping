## this computes the overlap between two paths, in terms of the total 
## path length 

PathOverlap<-function(mapping,costfn=Cost_Area)
{

    ##create left-biased and right(up)-biased mappings to more easily identify the overlap.
    left <- GetMinMap(mapping,leftbias=T,costfn=costfn)
    right<- GetMinMap(mapping,leftbias=F,costfn=costfn)
    
    bestmapping.left <- SummarizeMapping(left)
    bestmapping.right <- SummarizeMapping(right)
    
    ##we need to identify where the first element
    ##of patha that is mapped onto the last element of pathb, etc.


    ##to find the mapping, first take just the odd elements of the path (points):
    
    pts <-   bestmapping.left[1:(ceiling(nrow(bestmapping.left)/2))*2-1,]

    minA <- nrow(pts)-(sum(pts[,1]>1))
    minB <- nrow(pts)-(sum(pts[,2]>1))
    maxA <- sum(pts[,1]<pts[nrow(pts),1])+1
    maxB <- sum(pts[,2]<pts[nrow(pts),2])+1

    core <- (pts[max(minA,minB):min(maxA,maxB),]+1)/2
#    cat("Core:\n")
#    print(core)
    corepath1 <- mapping$path1[min(core[,1]):max(core[,1]),]
    corepath2 <- mapping$path2[min(core[,2]):max(core[,2]),]

    path1Length <- PathDist(mapping$path1)
    path2Length <- PathDist(mapping$path2)
    core1Length <- PathDist(corepath1)
    core2Length <- PathDist(corepath2)
    
    overlap1 <- core1Length/path1Length
    overlap2 <- core2Length/path2Length

    (overlap1+overlap2)/2
}
