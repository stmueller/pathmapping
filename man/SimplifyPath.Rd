\name{SimplifyPath}
\alias{SimplifyPath}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simplify a path
}
\description{
This function takes a path and removes points that impact the shape of the path less than a tolerance value, which is a scaled measure of degree deviation from removing a path. It is based on an algorithm described by Latecki & Lakaemper.
}
\usage{
SimplifyPath(path,
             tolerance = 0.075,
             truncate=F,
             faster = T, 
             verbose = F, plot = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
a 2-column matrix of x,y points
}
\item{truncate}{Number of digits to round values to.  If FALSE (the
  default), there is no rounding.  If another value, the value is used
  as the \code{digits} argument of  \code{\link{round}}
  to use to truncate the precision of the path.  This can help
  simplify the path by rounding to a known precision level.
  }
  \item{tolerance}{
   a tolerance threshold; any point that does not impact the shape of the path more than this is removed. For this, smaller values remove fewer points.  Depending on the complexity of the path, values from 0.01 to 0.1 may be good starting points. 
}
  \item{faster}{
If \code{TRUE}, this uses a faster point-trimming method.  If \code{T}, it removes all points that have a K value equal to the minimum value, which can be many (for example, a bunch that are equal to 0). If \code{F}, it will only remove one one each round, which can be very slow.
}
  \item{verbose}{
if \code{TRUE}, will print out intermediate information during the shape evolution.
}
  \item{plot}{
if \code{TRUE}, will create a plot of the path and overlay the evolving simplified path.
}
}
\value{
Returns a path with redundant points removed.
}

\references{
See Mueller et al., (2016). \cr
and\cr
L. J. Latecki and R. Lakaemper. Convexity Rule for Shape 
Decomposition Based on Discrete Contour Evolution. Computer Vision
and Image Understanding, vol. 73, pp. 441-454, 1999.\cr
\url{https://sites.google.com/a/mtu.edu/mapping/}
}
\author{
Shane T. Mueller and Brandon Perelman
}


\examples{
path <- cbind(1:100,exp(-(1:100-50)^2/80))
path2 <- SimplifyPath(path)

%Don't run the following to avoid long package build times/large package file.
\dontrun{
plot(path)
plot(path2)
plot(path)
points(SimplifyPath(path,tolerance=.1),type="o",
        col="red",cex=1.2,lwd=2)
plot(path)
points(SimplifyPath(path,tolerance=.01,plot=TRUE),type="o",
        col="red",cex=1.2,lwd=2)
plot(path)
points(SimplifyPath(path,tolerance=.005,plot=TRUE),type="o",
        col="red",cex=1.2,lwd=2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
