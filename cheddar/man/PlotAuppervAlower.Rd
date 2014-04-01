\name{PlotAuppervAlower}
\alias{PlotAuppervAlower}
\title{Plot upper-versus-lower link angles}
\description{High-level function for plotting upper-versus-lower link angles.}
\usage{
PlotAuppervAlower(community, 
                  main=CPS(community)$title,
                  xlab=~A[lower], 
                  ylab=~A[upper], 
                  xlim=c(-180, 180),
                  ylim=c(-180, 180), 
                  pch=19, 
                  \dots)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{main}{title of the plot}
  \item{xlab}{title of the x axis.}
  \item{ylab}{title of the y axis.}
  \item{xlim}{limits of the x axis.}
  \item{ylim}{limits of the y axis.}
  \item{pch}{plotting symbol.}
  \item{\dots}{other values to plot functions.}
}

\references{Cohen, J.E. and Schittler, D.N. and Raffaelli, D.G. and Reuman, 
D.C. (2009) Food webs are more than the sum of their tritrophic parts. 
Proceedings of the National Academy of Sciences of the United States of America 
\bold{106}, 52, 22335--22340.}

\author{Lawrence Hudson}

\seealso{\code{\link{NvMTriTrophicStatistics}}}

\examples{
data(TL84)
PlotAuppervAlower(TL84)
}
\keyword{hplot}

