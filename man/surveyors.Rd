\name{surveyors}
\alias{surveyors}
\alias{surveyors.3}
\alias{surveyors.4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Surveyor's Function
}
\description{
Computes the area of a polygon using the Surveyor's function.  Polygon \code{poly} is a matrix with two columns, where each row specifies a consecutive vertex of the polygon.
}
\usage{
surveyors(poly,usedet=FALSE)
surveyors.3(poly)
surveyors.4(poly)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{poly}{
A 2-column matrix containing the vertices of a polygon.
}
\item{usedet}{
  TRUE/FALSE variable, if T, will force the use of the true surveyor's
  formula, which is 15-20x slower than the special-purpose
  triangle/4-gon code.  This is only really good for testing/validating things.
}
}
\details{
This computes the area of a polygon using the so-called 'surveyor's' formula.  It computes the sum of the determinants of each edge, which results in the area of the polygon, provided the polygon is regular (does not intersect itself, etc.).  If the polygon is not regular, it will not measure the area, because negative areas will be subtracted from positive areas.

The general surveyor's formula is pretty inefficient. We primarily
(probably exclusively) use it for 2-, 3-, and 4-gons, and so there are
special-ppurpose functions defined as \code{surveyors.3} and \code{surveyors.4} that
are used in these cases that are about 15-20x faster than the general
one, using the so-called 'shoelace' formula.

Calls to this function account for a lot of the efficiency of the
entire algorithm.  Currently, surveyors tests the size of the n-gon and
routes to the specialized function, which appears to add 20\% overhead.
If you know that you are dealing with a trigon or a quadrilateral, you
can cut down time by a small amount.  A compiled version might offer
even more efficiency.
}
\value{
returns a measure of area.
}
\references{

See Mueller et al., (under review). \url{https://sites.google.com/a/mtu.edu/mapping/}
and
B. Braden. "The Surveyor's Area Formula". The College Mathematics Journal, vol. 17, no. 4, pp. 326-337, 1986.

}
\author{
Shane T. Mueller and Brandon Perelman
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{


poly <- rbind(c(1,1),c(10,1),c(5,3))
surveyors(poly)

## Profiling test for 3-gon
  poly <- rbind(c(1.1,1.2),c(2.1,3.3),c(4.1,1.2))
#system.time(for(i in 1:50000)surveyors(poly,usedet=TRUE))
#system.time(for(i in 1:50000)surveyors(poly))
#system.time(for(i in 1:50000)surveyors.3(poly))
# Profiling Test for 4-gon
poly2 <- rbind(c(1.1,1.2),c(2.2,1.3),c(4.0,4.25),c(1.3,3.9))
#system.time(for(i in 1:50000)surveyors(poly2,usedet=TRUE))
#system.time(for(i in 1:50000)surveyors(poly2))
#system.time(for(i in 1:50000)surveyors.4(poly2))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

