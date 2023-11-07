\name{GetMappingStats}
\alias{GetMappingStats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gets various statistics related to the mapping between two paths.
}
\description{
Returns a list object containing statistics related to the mapping between a pair of paths..
}
\usage{
 GetMappingStats(mapping)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mapping}{
The output of \code{GetMinMapping()}
}
}
\details{
This provides a detailed analysis of the outcome of a mapping, in the
form of several statistics that can be used for further analysis.

}
\value{
\item{length1}{The length of path 1}
\item{length2}{The length of path 2}
\item{overlap}{The percent overlap between paths.  That is, the percent of both paths that map onto a point on the other path, other than the end point. If two paths match for half of the longer path but one path is shortened, this should measure the proportion of the shortening.}
\item{deviation}{The Area-based deviation between paths}
\item{devperunit}{Deviation divided by the geometric mean of the lengths of path 1 and path 2}
\item{percentdev}{Percent deviation--sqrt of deviation divided by geometric length of path1 and path2.  This measures the proportion of the length that of the average deviation.}
\item{adjusteddev}{percentdev divided by overlap--proportion overlap is lengthened by amount of overlap.  If overlap is 1.0, no adjustment.}
}
\author{
Shane T. Mueller
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
