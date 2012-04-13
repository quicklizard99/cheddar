\name{PlotRankNPS}
\alias{PlotRankNPS}
\alias{PlotBvRankB}
\alias{PlotMvRankM}
\alias{PlotNvRankN}
\title{Plot rank of node properties}
\description{High-level functions for plotting value-versus-rank of node 
properties.}

\usage{
PlotRankNPS(community, 
            property, 
            rank.by=property, 
            log10.rank = FALSE, 
            xlab, 
            ylab, 
            show.web=FALSE, 
            \dots)

PlotMvRankM(community, 
            log10.rank = FALSE, 
            xlab, 
            ylab, 
            \dots)

PlotNvRankN(community, 
            log10.rank = FALSE, 
            xlab, 
            ylab, 
            \dots)

PlotBvRankB(community, 
            log10.rank = FALSE, 
            xlab, 
            ylab, 
            \dots)
}

\arguments{
  \item{community}{an object of class \code{Community}}
  \item{property}{the name of a property that is plotted on the y axis. Must 
    meet the criteria of the \code{properties} parameter of \code{NPS}.}
  \item{rank.by}{the name of a property by which points are ordered along the 
    x axis. Must meet the criteria of the \code{properties} parameter of 
    \code{NPS}.}
  \item{log10.rank}{logical - if \code{TRUE} the rank values plotted on the 
    x axis are log10-transformed.}
  \item{xlab}{title of the x axis.}
  \item{ylab}{title of the y axis.}
  \item{show.web}{logical - if \code{TRUE} and \code{community} has trophic 
    links then the food web is plotted using the \code{link*} and 
    \code{highlight.links} parameters}
  \item{\dots}{other values to \code{PlotNPS}.}
}

\details{
The convenience functions \code{PlotMvRankM}, \code{PlotNvRankN} and 
\code{PlotBvRankB} are `wrappers' around \code{PlotRankNPS} that plot rank 
log10-transformed body mass (M), numerical abundance (N) or biomass (B).}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, 
         \code{\link{NPS}}, 
         \code{\link{PlotBSpectrum}}, 
         \code{\link{PlotCircularWeb}}, 
         \code{\link{PlotNPS}}, 
         \code{\link{PlotNPSDistribution}}, 
         \code{\link{PlotNSpectrum}}, 
         \code{\link{PlotTLPS}}, 
         \code{\link{PlotWebByLevel}}}

\examples{
data(TL84)
PlotNvRankN(TL84)

# log10(N) against log10(rank of M)
PlotRankNPS(TL84, property='Log10N', rank.by='M', log10.rank=TRUE)
}
\keyword{hplot}

