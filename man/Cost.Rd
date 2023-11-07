\name{Cost}
\alias{Cost}
\alias{Cost.quadratic}
\alias{Cost.area}
\alias{Cost.areasquared}
\alias{Cost.weightedbydist}
\alias{Cost.rectangular}
\alias{Cost_Area}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute area-based cost
}
\description{
This function computes an area associated with a transition between two
correspondences on two paths.  A number of cost functions can be
specified, but the most reasonable are Cost.quadratic (the default)
and Cost.area.  other cost functions are not robust to choices of segmentation.

}
\usage{
Cost(xy1, xy2, i, j, pi, pj, opposite, costfn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xy1}{
A path of x,y coordinates (a matrix in two columns)
}
  \item{xy2}{
A  second path of x,y coordinates (a matrix in two columns)
}
  \item{i}{
Index of Node i of path xy1
}
\item{j}{
Index of Node j of path xy2

}
  \item{pi}{
Index of node previous to node i on path xy1
}
  \item{pj}{
Index of node previous to node j on path xy2
}
  \item{opposite}{
matrix specifying whether points on one path are 'opposite' points on
  another path, and if so the proportion between the two points where the
  orthogonal line falls.
}
\item{costfn}{
  Specify a cost function to use. By default, Cost.quadratic is used,
  although Cost.area is also reasonable.  Other cost functions are
  provided but may be sensitive to segmentation.
}
}
\details{
This is the basic cost function for a transition between nodes.  This
  function does not need to be used directly, but is called repeatedly
  by \code{CreateMap}
}
\value{
returns a floating-point value descibing the area defined by the the two
  path transitions provided the transitions are legal (according to
  \code{connected}.  Otherwise, it will return \code{Inf}.  
}
\references{
See Mueller et al., (2016). \url{https://sites.google.com/a/mtu.edu/mapping/}
}
\author{
Shane T. Mueller and Brandon Perelman
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
