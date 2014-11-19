\name{LinkCost}
\alias{LinkCost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes the distance of a particular link between paths
}
\description{
This computes the distance between two paths for a particular pair of
nodes (points or segments).  This is primarily a helper function for the
pathmapping library, and typically does not need to be used by end users.
}
\usage{
LinkCost(xy1, xy2, i, j)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xy1}{
Path1
}
  \item{xy2}{
Path2
}
  \item{i}{
Index of path1.  This is not the row of path1, but the implied node,
where the first node is the first point, the second node is the first
segment, the third node is the second point, and so on.
}
  \item{j}{
Index of path2.  This is not the row of path1, but the implied node,
where the first node is the first point, the second node is the first
segment, the third node is the second point, and so on.
}
}
\references{
See Mueller et al., (under review). \url{https://sites.google.com/a/mtu.edu/mapping/}
}
\author{
Shane T. Mueller and Brandon Perelman
}
%% ~Make other sections like Warning with \section{Warning }{....} ~


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
