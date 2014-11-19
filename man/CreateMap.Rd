\name{CreateMap}
\alias{CreateMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a mapping between paths
}
\description{
Given two paths, this creates a mapping that minimizes the 
}
\usage{
CreateMap(xy1.1, xy2.1, plotgrid = F,costfn=Cost.area,
         nondecreasingos = F, verbose = F, insertopposites = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xy1.1}{
The first path (a matrix of x,y points).
}
  \item{xy2.1}{
The second path (a matrix of x,y points).
}
\item{plotgrid}{
T/F variable; should the lattice grid be plotted? Defaults to F
}

\item{costfn}{
  Cost function to use to measure deviation between path segments.
  }
  \item{nondecreasingos}{
T/F variable; defaults to F.  If T, forces multiple consecutive mappings
of points on one map to a single segment on the second map be
monotonic.  This will not necessarily find the optimal mapping; to do
this, you must set insertopposites=T.
}
\item{verbose}{
   T/F; Whether to print out intermediate status information.
    
}
  \item{insertopposites}{
T/F; defaults to T.  If T, it will insert points on each path when they
  are opposite a point on the other graph.  This allows for an optimal
  monotonic mapping between paths, at the cost of (possibly substantial)
  efficiency cost.
}
}
\details{
This finds the minimum-area mapping between two paths.  It also produces
  a candidate minimum-area mapping.
}
\value{
  returns a mapping object
 }
\references{
See Mueller et al., (under review). \url{https://sites.google.com/a/mtu.edu/mapping/}
}
\author{
Shane T. Mueller and Brandon Perelman
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{GetMinMap}}
}

\examples{
##################################
## Example from appendix of Mueller, Perelman, & Veinott:

pathA <- rbind(c(0,0),c(5,0),c(10,0))
pathB <- rbind(c(1,1),c(2,-1),c(3,4),c(5,1),c(10,-3))
answer<- CreateMap(pathA,pathB,FALSE)
PlotMap(answer)
answer2 <- GetMinMap(answer)
PlotMap(answer2)


%%The rest of these examples work, and all aspects of the
%%the package have been exercised here.  These are placed in
%%dontrun to avoid long package builds and large package size
\dontrun{
##################################
##Here is an example of two diagonal paths, a fixed number
##of units apart look at how different equivalent paths produce
## different mappings, but the same area

test2.a <- cbind(1:10*10,1:10*10)
test2.b <- cbind(1:10*10+10,1:10*10)

test2.outa <- CreateMap(test2.a,test2.b,FALSE)
test2.outb <- CreateMap(test2.b,test2.a,FALSE)
test2.outc <- CreateMap((test2.a[10:1,]),(test2.b[10:1,]),FALSE)
test2.outd <- CreateMap((test2.b[10:1,]),(test2.a[10:1,]),FALSE)

par(mfrow=c(2,2))
PlotMap(test2.outa)
PlotMap(test2.outb)
PlotMap(test2.outc)
PlotMap(test2.outd)

##################################
##Now, get the 'minimum-distance' mapping among these:

test2.mapa <- GetMinMap(test2.outa)
test2.mapb <- GetMinMap(test2.outb)
test2.mapc <- GetMinMap(test2.outc)
test2.mapd <- GetMinMap(test2.outd)

par(mfrow=c(2,2))
PlotMap(test2.mapa)
PlotMap(test2.mapb)
PlotMap(test2.mapc)
PlotMap(test2.mapd)

##################################
## Example: a loop and itself

test3.a <- rbind(c(102, 100),
      c(  120, 109),      c(  133, 124),
      c(  146, 138),      c(  158, 155),
      c(  174, 166),      c(  194, 170),
      c(  213, 173),      c(  233, 176),
      c(  251, 169),      c( 260, 151),
      c( 255, 132),      c( 245, 115),
      c( 235,  98),      c( 223,  82),
      c( 212,  65),      c( 194,  58),
      c( 175,  65),      c( 166,  82),
      c( 169, 101),      c(300,101))

test3.b <- test3.a

test3.out <- CreateMap(test3.a,test3.b)
PlotMap(test3.out)


########################################
##Example: A loop with an offset version of itself

test4.a <- test3.a
test4.b <- test3.a + 20
test4.out <- CreateMap(test4.a,test4.b,plotgrid=FALSE)
par(mfrow=c(1,2))
PlotMap(test4.out)
PlotMap(GetMinMap(test4.out)) 

#######################################
## Example:  a gentle curve, and a line.
test5.a <- cbind((-10):10*10,exp(-(-10:10*10)^2/500))
test5.b <- cbind(-10:10*10,-.5)
test5.a2 <- test5.a[21:1,]
test5.b2 <- test5.b[21:1,]


test5.out <- CreateMap(test5.b,test5.a,FALSE)
test5.outb <-CreateMap(test5.b2,test5.a2,FALSE)
par(mfrow=c(2,2))
PlotMap(test5.out) 
PlotMap(test5.outb)

PlotMap(GetMinMap(test5.out)) 
PlotMap(GetMinMap(test5.outb))

## Note: the curved path gets 'shadow' opposite points inserted, and so
##the MinMap is a bit off.  In this case, we shouldn't need to insert
##opposites, so we can turn it off:

test5.out <- CreateMap(test5.b,test5.a,plotgrid=FALSE,insertopposites=FALSE)
test5.outb <-CreateMap(test5.b2,test5.a2,plotgrid=FALSE,insertopposites=FALSE)
par(mfrow=c(2,2))
PlotMap(test5.out) 
PlotMap(test5.outb)

PlotMap(GetMinMap(test5.out)) 
PlotMap(GetMinMap(test5.outb))


#######################################
##Cut off one part:
test5.b2<- test5.b[c(1,5,21),]
test5.out2 <- CreateMap(test5.a,test5.b2,FALSE)

PlotMap(test5.out2) 
PlotMap(GetMinMap(test5.out2))


######################################
## Example: a path with a bump.  Note that
## if we don't allow mapping points onto segments
## the area goes outside the polygon.

test6.a <- rbind(c(0,0),c(1,0),c(10,0))
test6.b <- rbind(c(0,1),c(4,1),c(5,9),c(6,1),c(10,1))

##true area should be 1x10 + 2*8/2 = 18.
test6.out <- CreateMap(test6.a,test6.b,FALSE)
PlotMap(test6.out) 
PlotMap(GetMinMap(test6.out))


#######################################
## Example: to lines, one with a bump
test7.a  <-  rbind(c(1,0),c(2,-1),c(3,0),c(4,0),c(5,0),c(6,0))
test7.b <- rbind(c(1,1),c(2,1),c(3,1),c(4,1),c(5,1),c(6,1))

test7.out <- CreateMap(test7.a,test7.b,FALSE)
test7.outr <- CreateMap(test7.b,test7.a,FALSE)

test7.outmin <- GetMinMap(test7.out)

par(mfrow=c(3,1),mar=c(3,2,2,0))
PlotMap(test7.out)
PlotMap(test7.outr)
PlotMap(GetMinMap(test7.out))

########################################
## Example: simplified case with a lot of 'opposites'
test8.a <- cbind(0:4+.5,0)
test8.b <- cbind(0:4,1)
test8.out <- CreateMap(test8.a,test8.b,FALSE)
par(mfrow=c(1,2))
PlotMap(test8.out)
PlotMap(GetMinMap(test8.out))

########################################
## Example: a crossover

test9.a <- rbind(c(0,0),c(1,0),c(10,0))
test9.b <- rbind(c(0,-1),c(4,-1),c(5,9),c(6,-1),c(10,-1))
test9.out <- CreateMap(test9.a,test9.b,FALSE)
PlotMap(test9.out) 
PlotMap(GetMinMap(test9.out))

########################################
## Example: a variation on previous
test10.a <- test9.b
test10.b <- rbind(c(0,10),c(20,10))
test10.out <- CreateMap(test10.a,test10.b,FALSE)
test10.out2 <- CreateMap(test10.b,test10.a,FALSE)

PlotMap(test10.out)
PlotMap(test10.out2) 
PlotMap(GetMinMap(test10.out))
PlotMap(GetMinMap(test10.out2) )


#######################################
##  Example: Appendix figures
pathA <- rbind(c(0,0),c(5,0),c(10,0))
pathB <- rbind(c(1,1),c(2,-1),c(3,4),c(5,1),c(10,-3))
map1 <- CreateMap(pathA,pathB,FALSE,insertopposites=FALSE)


##map2 is broken, or at least the display of map2:
map2 <- GetMinMap(map1)

par(mfrow=c(2,1))
PlotMap(map1)
PlotMap(map2) 



############################################
## Example: another crossover

real.sub <- rbind(c(50,25),c(100,150),c(275,275))
mem.sub <- rbind(c(100,30),c(150,250), c(250,200))

xy1 <- real.sub
xy2 <- mem.sub

test10.out <- CreateMap(xy1,xy2,FALSE)
PlotMap(test10.out)
PlotMap(GetMinMap(test10.out))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
