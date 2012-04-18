\name{PlotCircularWeb}
\alias{PlotCircularWeb}
\title{Plot circular web}
\description{High-level function for plotting nodes in a circle.}
\usage{
PlotCircularWeb(community, 
                clockwise = TRUE, 
                origin.degrees = 0, 
                proportional.radius = 1, 
                frame.plot = FALSE,
                xlim = c(-1,1), 
                ylim = c(-1,1), 
                \dots)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{clockwise}{logical - if \code{TRUE} nodes are plotted in a 
    clockwise order.}
  \item{origin.degrees}{the angle in degrees at which the first node in 
    \code{community} will be placed.}
  \item{proportional.radius}{a value between 0 and 1.}
  \item{frame.plot}{logical.}
  \item{xlim}{limits of the x axis.} 
  \item{ylim}{limits of the y axis.} 
  \item{\dots}{other values to \code{PlotNPS}.}
}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, 
         \code{\link{PlotBSpectrum}}, 
         \code{\link{PlotNPS}}, 
         \code{\link{PlotNPSDistribution}}, 
         \code{\link{PlotNSpectrum}}, 
         \code{\link{PlotRankNPS}}, 
         \code{\link{PlotTLPS}}, 
         \code{\link{PlotWebByLevel}}}

\examples{
data(TL84)
PlotCircularWeb(TL84)

# Plot the first node at the 6 o'clock position
PlotCircularWeb(TL84, origin.degrees=180)

# Plot the first node at the 6 o'clock position and plot nodes 
# counter-clockwise
PlotCircularWeb(TL84, origin.degrees=180, clockwise=FALSE)
}
\keyword{hplot}

