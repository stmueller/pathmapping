\name{SummarizeMapping}
\alias{SummarizeMapping}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summarize the mapping obtained by GetMinMap
}
\description{
Returns a data frame outlining specifically the mapping between two
paths, including all the inferred points, identifying the original
points, and the distances between corresponding points/segments.
}
\usage{
 SummarizeMapping(mapping)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mapping}{
The output of \code{GetMinMapping()}
}
}
\details{
This provides a detailed analysis of the outcome of a mapping, in the
form of a seven-column data frame.  An original path is transformed into
a multi-segment path by adding intersections and 'opposite' points lying
on segments.  

}
\value{
The first two columns indicate the index node of these two 
lengthened paths that correspond to one another.  Odd nodes indicate
points, and even nodes indicate segments (segments can be
skipped). These correspond to rows and columns of the optimization
matrix also returned by \code{CreateMap} and \code{GetMinMapping}.

The next two columns indicate how these rows are mapped back onto the
original paths.  0 indicates the row was not an original point, non-zero
integers indicate specific elements of the paths.

The next two column indicate the x,y coordinates on path 1 of each point
(original or inferred). The following two columns indicate the x,y,
coordinates on path 2. The final column indicates the euclidean distance
corresponding to the particular mapping indicated.

}

\author{
Shane T. Mueller and Brandon Perelman
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
