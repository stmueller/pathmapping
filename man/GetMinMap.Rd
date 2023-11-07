\name{GetMinMap}
\alias{GetMinMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gets least-distance mapping among the minimum-area mappings.
}
\description{
Finds minimum linear-distance mapping among the least-cost area-based mappings between paths.
}
\usage{
GetMinMap(mapping, leftbias=T, verbose = F, costfn=Cost_Area)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mapping}{
An object computed via CreateMap()
%%     ~~Describe \code{mapping} here~~
}
\item{leftbias}{
Boolean value determining whether to prefer left or upper node connections first.
}
  \item{verbose}{
Whether intermediate output should be printed.
%%     ~~Describe \code{verbose} here~~
}
  \item{costfn}{
      \code{costfn} is the cost function to use
    }

}
\details{
%%  ~~ If necessary, more details than the description above ~~

GetMinMap() finds the best mapping between two paths amongst those that
have the smallest area-based dissimilarity. It adds several data
structures to a mapping produced by CreateMap: $leastcostchain and
$chainpath, and it sets the boolian $minmap from FALSE to TRUE.
}

\value{

\item{$leastcostchain}{The complete matrix used to solve the minimization.}
\item{$chainpath}{The sequence of consecutive nodes representing the path.}

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
See Mueller et al., (2016). \url{https://sites.google.com/a/mtu.edu/mapping/}
}
\author{
Shane T. Mueller and Brandon Perelman
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also \code{\link{CreateMap}}, 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
