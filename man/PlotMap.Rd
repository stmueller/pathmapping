\name{PlotMap}
\alias{PlotMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the paths and their mapping.
}
\description{
Plots the paths and the mappings between paths.
}
\usage{
PlotMap(mapping, cols = c("grey40"), linecol = "grey25",
           xlim= NA, ylim = NA, ...)
} 

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mapping}{
A map object produced by CreateMap() or GetMinMap()
}
  \item{cols}{
A set of colors to shade consecutive polygons.
}
  \item{linecol}{
Color for lines connecting paths
}
 \item{xlim}{
  two values that override default x range.
} \item{ylim}{
   two values that override default y range.
}
 \item{...}{
   other graphical arguments


}

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
