\name{PathOverlap}
\alias{PathOverlap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute the proportion overlap of two paths.
}
\description{
This computes the proportion of the two paths that are mapped onto one
another. The outcome is a number between 0 and 1.0, measuring how much
of each path corresponds to a non-endpoint of the other map.  This does not by itself measure path similarity, because two
paths that are highly dissimilar that happen to start and end at similar
spots would have a value close to 1.0.  This is sensitive to 'partial'
paths. If on path is a sub-path of the other, then it should measure the
average of the proportion of path1 that is in path2 and the proportion
of path2 that is in path1.
}
\usage{
PathOverlap(mapping,costfn=Cost_Area)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mapping}{
%%     ~~Describe \code{mapping} here~~
      \code{mapping} is the output of \code{CreateMap()}.  
    }
    \item{costfn}{
         \code{costfn} is the cost function to use
      }

}
\details{
\code{PathOverlap} works with a basic mapping and computes 'minimal' mappings,
both left-biased and right-biased, to arrive at two best mappings (those
involving the least distance between corresponding points).  These will
typically be identical, but there are non-degenerate cases where they
can differ.

Once this mapping is arrived at, the algorithm identifies the core of
the mapping--the central segment of both paths that are mapped onto
the core of the other path, by identifying the segments of each path
(on both ends) that are mapped onto the endpoint of the other path.  For
each path, the length of its core is subtracted from the total length of
the path, and these two values are averaged together for the returned
proportion value.

The result of this can be used to weigh distance between paths in terms
of overall path similarity.  A reasonable measure of path similarity
might be the (area between paths) / (average length of two paths) /
overlap, so that two paths with low overlap get their total distance
inflated by that proportion.
}

\value{
Return value is a number between 0 and 1.  Values below 0.5 are difficult, because the average overlap of the two paths is found.  Thus, even if one path has 0 overlap, the other is likely to have an overlap near 1.0, resulting in an average of 0.5.
}
\references{
See Mueller et al., (2016). \url{https://sites.google.com/a/mtu.edu/mapping/}
}
\author{
Shane T. Mueller and Brandon Perelman
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
