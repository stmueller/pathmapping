\name{ClosestPoint}
\alias{ClosestPoint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find Closest Point
}
\description{
Find the the point on a line segment (x1,y1) (x2,y2) that is closest to point (px,py).
}
\usage{
ClosestPoint(px, py, x1, y1, x2, y2)
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
x coordinate of one end of a segment
}
  \item{y1}{
y coordinate of one end of a segment
}
  \item{x2}{
x coordenite of other end of a segment
}
  \item{y2}{
y coordinate of other end of a segment
}
}
\details{
This function finds the the point on a line segment (x1,y1) (x2,y2) that is closest to point (px,py).  If the line perpendicular to the
line segment does not intersect the segment, the function will return an end point of the segment, otherwise, it will return a point on the line segment where the perpendicular line intersects the segment.


}
\value{
a point-pair (x,y)
}
\references{
See Mueller et al., (2016). \url{https://sites.google.com/a/mtu.edu/mapping/}
}
\author{
Shane T. Mueller and Brandon Perelman
}
\note{
Uses LineMagnitude, also supplied by the pathmapping package.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{LineMagnitude}}, \code{\link{DistancePointSegment}}

}
\examples{
ClosestPoint(1,10,2,0,5,0)    #returns end point 2 0
ClosestPoint(20,10,2,0,5,0)   #returns other end point 5 0
ClosestPoint(4.5,10,2,0,5,0)  #returns closest point 4.5 0


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

