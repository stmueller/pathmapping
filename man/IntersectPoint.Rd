\name{IntersectPoint}
\alias{IntersectPoint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find where point opposite segment intersects segment.
}
\description{
Find where point opposite segment intersects segment.    It gives a proportion of AB that the
orthogonal line passing through C meets. If outside (0,1) it does not pass through AB
}
\usage{
IntersectPoint(A, B, C)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
\code{(x,y)} point on one end of line segment
}
  \item{B}{
\code{(x,y)} point on other end of line segment
}
  \item{C}{
\code{(x,y)} point to compare to line segment
}
}
\value{
returns a value which is the proportion of the length of \code{AB} where \code{AC} proects onto the line defined by AB. If the return value is between 0 and 1, the point is opposite the line segment.  If negative, it falls on the \code{A} side of \code{AB}; if greater than 1, it falls to the \code{B} side of \code{AB}.
}
\references{
See Mueller et al., (under review). \url{https://sites.google.com/a/mtu.edu/mapping/}
}
\author{
Shane T. Mueller and Brandon Perelman
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{LineMagnitude}},\code{\link{ClosestPoint}},\code{\link{DistancePointSegment}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

