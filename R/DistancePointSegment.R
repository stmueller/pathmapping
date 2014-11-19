DistancePointSegment <-
function(px,py,x1,y1,x2,y2)
  {
    closest <- ClosestPoint(px,py,x1,y1,x2,y2)
    dist <- LineMagnitude(px, py, closest[[1]],closest[[2]])
    dist
}
