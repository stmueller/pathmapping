\name{PlotGrid}
\alias{PlotGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the grid associated with a particular mapping problem. 
}
\description{
This function plots a grid associated with the mapping between two paths.  This will plot n*2-1 by m*2-1 nodes,
associated with each point and segment in each path.  This gets quite substantial when the paths have more than a 
handful of points (5-10), and is only really useful for display and debugging purposes.  This is called by
\code{CreateMap}, which overlays costs over the map on its own.
}
\usage{
PlotGrid(path1,path2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path1}{
The first path
}
  \item{path2}{
The second path
}
}
\details{
  Warning--do not use for anything but very small paths, as it will take a long time to draw and be uninterpretable.

}
\references{
See Mueller et al., (under review). \url{https://sites.google.com/a/mtu.edu/mapping/}
}
\author{
Shane T. Mueller and Brandon Perelman
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
