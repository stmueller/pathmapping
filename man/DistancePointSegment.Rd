\name{DistancePointSegment}
\alias{DistancePointSegment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute distance between a point and a segment
}
\description{
Compute distance between a point and a segment

}
\usage{
DistancePointSegment(px, py, x1, y1, x2, y2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{px}{
x coordinate of point
}
  \item{py}{
y coordinate of point 
}
  \item{x1}{
x coordinate of one end of segment
}
  \item{y1}{
y coordinate of one end of segment
}
  \item{x2}{
x coordinate of other end of segment
}
  \item{y2}{
y coordinate of other end of segment
}
}
\details{
Computes the distance between a point and a segment via the shortest
line.  This line will be perpendicular to the segment if the point
opposes the line, or it will be attached directly to an endpoint.
}
\value{
returns a scalar value measuring the distance
}
\references{
See Mueller et al., (2016). \url{https://sites.google.com/a/mtu.edu/mapping/}
}
\author{
Shane T. Mueller and Brandon Perelman
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{LineMagnitude}},\code{\link{ClosestPoint}}
}
\examples{

##select a random point and find its closest point
##on the segment.
x1 <- runif(1)*20
y1 <- runif(1)*20

s1x <- 5;s1y <- 5
s2x <- 0;s2y <- 15

d <- DistancePointSegment(x1,y1,s1x,s1y,s2x,s2y)
plot(c(s1x,s2x),c(s1y,s2y),pch=16,xlab="x",ylab="y",
      ylim=c(-1,20),xlim=c(-1,20),type="o")
points(x1,y1,col="red",pch=16,cex=2)

p2 <-  ClosestPoint(x1,y1,s1x,s1y,s2x,s2y)
segments(p2[1],p2[2],x1,y1,lty=3)
text(10,2,paste("Distance from line to point:",
       round(d,3)))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
