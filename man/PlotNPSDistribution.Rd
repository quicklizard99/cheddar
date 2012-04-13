\name{PlotNPSDistribution}
\alias{PlotNPSDistribution}
\alias{PlotBDistribution}
\alias{PlotMDistribution}
\alias{PlotNDistribution}
\alias{PlotDegreeDistribution}
\title{Plot distributions of node properties}
\description{High-level functions for plotting distributions of node 
properties.}

\usage{
PlotNPSDistribution(community, 
                    property, 
                    main = CPS(community)$title, 
                    \dots)

PlotBDistribution(community, 
                  xlab = Log10BLabel(community), 
                  \dots)

PlotMDistribution(community, 
                  xlab = Log10MLabel(community), 
                  \dots)

PlotNDistribution(community, 
                  xlab = Log10NLabel(community), 
                  \dots)

PlotDegreeDistribution(community, 
                       xlab = "Number of links", 
                       \dots)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{property}{the name of a property that is plotted on the y axis. Must 
    meet the criteria of the \code{properties} parameter of \code{NPS}.}
  \item{main}{title of the plot}
  \item{xlab}{title of the x axis.}
  \item{\dots}{other values to plot functions.}
}

\details{The convenience functions \code{PlotBDistribution}, 
\code{PlotMDistribution} and \code{PlotNDistribution} are wrappers around 
\code{PlotNPSDistribution}.}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, 
         \code{\link{NPS}}, 
         \code{\link{DegreeDistribution}}, 
         \code{\link{PlotCircularWeb}}, 
         \code{\link{PlotNPS}}, 
         \code{\link{PlotNPSDistribution}}, 
         \code{\link{PlotRankNPS}}, 
         \code{\link{PlotTLPS}}, 
         \code{\link{PlotWebByLevel}}}

\examples{
data(TL84)

PlotMDistribution(TL84)
PlotDegreeDistribution(TL84)
}
\keyword{hplot}


