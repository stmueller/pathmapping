\name{linesIntersect}
\alias{linesIntersect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Checks whether two line segments intersect.
}
\description{
Checks whether two line segments intersect.
}
\usage{
linesIntersect(A1, A2, B1, B2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A1}{
one end of line A (x,y) pair
}
  \item{A2}{
other end of line A (x,y) pair
}
  \item{B1}{
one end of line B (x,y) pair
}
  \item{B2}{
other end of line B (x,y) pair
}
}
\value{
returns a boolean value indicating whether there is an intersection.
}
\note{
Results may not be consistent if intersection happens exactly at one end of a segment, due to rounding error.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
