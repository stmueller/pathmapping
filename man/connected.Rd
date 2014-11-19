\name{connected}
\alias{connected}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Determine whether two nodes are connected.  }

\description{ Determines whether two nodes in the planar graph
describing the mapping between two adjacent node-node mappings.  That
is, given a two correspondences between a nodes on two paths, it
determines whether there is a legal transition between them.  This can
be seen as two elements of the mapping matrix, each specified by a row
and column.  }

\usage{ connected(r1, c1, r2, c2) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r1}{
row of first node
}
  \item{c1}{
column of first node
}
  \item{r2}{
row of second node
}
  \item{c2}{
column of second node.
}
}
\details{
r1,c1 should specify a node to the left/above r2,c2.  The outcome
depends on whether r,c is a node or segment on the path.  Point-point
mappings can transition to the next point-segment mappings, or the next
point-point segment. 
  
}
\value{
  returns \code{T} or \code{F}
}
\references{
  See Mueller et al., (under review). \url{https://sites.google.com/a/mtu.edu/mapping/}
}
\author{
Shane T. Mueller and Brandon Perelman
}
\note{
  The outcome of this does not depend on the actual paths--it is simply
  a logical computation based on transitions between points and segments.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
connected(3,5,2,4)
connected(3,3,1,1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
