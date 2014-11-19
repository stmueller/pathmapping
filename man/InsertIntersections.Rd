\name{InsertIntersections}
\alias{InsertIntersections}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Inserts points on paths where two paths intersect
}
\description{
This function does two rounds of insertion.  First, it inserts a point on each path whenever path1 intersects path2.  Next, it optionally inserts points on segments of one path that are 'opposite' points on the other path, to allow a monotonic mapping between the two paths.
}
\usage{
InsertIntersections(path1, path2, insertopposites = T, verbose = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path1}{
path1
}
  \item{path2}{
path2
}
  \item{insertopposites}{
T/F, whether points opposite points on the other path should be inserted
}
  \item{verbose}{
T/F, whether to print interim progress information
}
}
\value{
A list of four data sequences are returned:

 \item{newpath1}{New list 1 with new points inserted}
 \item{newpath2}{New list2 with new points inserted}
  \item{key1}{Set of indices mapping the points back to the original path 1.  Inserted points are labeled -1}
  \item{key2}{Set of indices mapping the points back to the original path 2.  Inserted points are labeled -1}

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
