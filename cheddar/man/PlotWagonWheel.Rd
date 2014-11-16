\name{PlotWagonWheel}
\alias{PlotWagonWheel}
\title{Plot wagon wheel}
\description{Plot a nodes as concentric circles around a focal node.}
\usage{
PlotWagonWheel(community,
               focus,
               clockwise=TRUE,
               origin.degrees=0,
               frame.plot=FALSE,
               main=NULL,
               \dots)
}

\arguments{
  \item{community}{an object of class \code{Community}.}
  \item{focus}{the node to be placed at the centre of the plot - a node name or
    integer index.}
  \item{clockwise}{logical - if \code{TRUE} nodes are plotted in a
    clockwise order.}
  \item{origin.degrees}{the angle in degrees at which the first node in
    \code{community} will be placed.}
  \item{frame.plot}{logical.}
  \item{main}{the plot's title.}
  \item{\dots}{other values to \code{PlotNPS}.}
}

\details{The node given in \code{focus} is plotted at the centre of the
wagon wheel. Other nodes in the community are plotted in concentric circles
that are one, two, three etc trophic links away from the focus. Isolated
nodes - those with no resources or consumers, other than possibly themselves - 
are not shown.
}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}},
         \code{\link{Degree}},
         \code{\link{IsolatedNodes}},
         \code{\link{OrderCommunity}},
         \code{\link{PlotCircularWeb}},
         \code{\link{PlotNPS}},
         \code{\link{PlotWebByLevel}},
         \code{\link{rgb}},
         \code{\link{ShortestPaths}}}

\examples{
data(TL84)
# Ploesoma sp. is the focal species
PlotWagonWheel(TL84, 'Ploesoma sp.')

# Show nodes as numbers
PlotWagonWheel(TL84, 'Ploesoma sp.', show.nodes.as='labels')

# 'Daphnia pulex' is the focus, nodes ordered by degree (total number of
# trophic links), lines partially transparent
PlotWagonWheel(OrderCommunity(TL84, 'Degree'), 'Daphnia pulex',
               show.nodes.as='labels', link.col=rgb(0.8,0.8,0.8,0.5))


}
\keyword{hplot}
